use std::io;
use std::path::Path;

use bollard::container::{DownloadFromContainerOptions, LogOutput, UploadToContainerOptions};
use bollard::exec::{CreateExecOptions, StartExecResults};
use bollard::Docker;
use futures::{StreamExt, TryStreamExt};

/// Archives the contents of `src_dir` under a top-level directory named `root`.
pub fn tar_directory(src_dir: &Path, root: impl AsRef<Path>) -> io::Result<Vec<u8>> {
    let mut builder = tar::Builder::new(Vec::new());
    builder.follow_symlinks(false);
    builder.append_dir_all(root, src_dir)?;
    builder.into_inner()
}

pub fn tar_file(src: &Path, name: impl AsRef<Path>) -> io::Result<Vec<u8>> {
    let mut builder = tar::Builder::new(Vec::new());
    builder.append_path_with_name(src, name)?;
    builder.into_inner()
}

/// Writes the single file of a `GET /containers/{id}/archive` response to `dest`.
pub fn untar_file(archive: &[u8], dest: &Path) -> io::Result<()> {
    let mut archive = tar::Archive::new(archive);
    let mut entry = archive
        .entries()?
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "empty archive"))??;
    if !entry.header().entry_type().is_file() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "archive does not hold a regular file",
        ));
    }
    entry.unpack(dest)?;
    Ok(())
}

/// Splits `dest` into the directory to upload into and the archive root that lands at `dest`,
/// so `dest` is created when missing, like `docker cp`.
fn split_dest(dest: &str) -> Result<(&str, &str), String> {
    let path = Path::new(dest);
    path.parent()
        .and_then(Path::to_str)
        .zip(path.file_name().and_then(|name| name.to_str()))
        .ok_or_else(|| format!("invalid container path: {dest}"))
}

async fn upload(
    docker: &Docker,
    container: &str,
    dir: &str,
    archive: Vec<u8>,
) -> Result<(), String> {
    let options = UploadToContainerOptions {
        path: dir,
        ..Default::default()
    };
    docker
        .upload_to_container(container, Some(options), archive.into())
        .await
        .map_err(|e| format!("unable to upload to {container}:{dir}: {e}"))
}

/// Same as `docker cp {src_dir}/. {container}:{dest}`.
pub async fn copy_dir_to_container(
    docker: &Docker,
    container: &str,
    src_dir: &Path,
    dest: &str,
) -> Result<(), String> {
    let (dir, root) = split_dest(dest)?;
    let archive = tar_directory(src_dir, root)
        .map_err(|e| format!("unable to archive {}: {e}", src_dir.display()))?;
    upload(docker, container, dir, archive).await
}

/// Same as `docker cp {src} {container}:{dest}`.
pub async fn copy_file_to_container(
    docker: &Docker,
    container: &str,
    src: &Path,
    dest: &str,
) -> Result<(), String> {
    let (dir, name) = split_dest(dest)?;
    let archive =
        tar_file(src, name).map_err(|e| format!("unable to archive {}: {e}", src.display()))?;
    upload(docker, container, dir, archive).await
}

/// Same as `docker cp {container}:{src} {dest}`.
pub async fn copy_file_from_container(
    docker: &Docker,
    container: &str,
    src: &str,
    dest: &Path,
) -> Result<(), String> {
    let archive: Vec<u8> = docker
        .download_from_container(container, Some(DownloadFromContainerOptions { path: src }))
        .map_ok(|chunk| chunk.to_vec())
        .try_concat()
        .await
        .map_err(|e| format!("unable to download {container}:{src}: {e}"))?;
    untar_file(&archive, dest).map_err(|e| format!("unable to write {}: {e}", dest.display()))
}

pub struct ExecOutput {
    pub exit_code: Option<i64>,
    pub stderr: String,
}

impl ExecOutput {
    pub fn success(&self) -> bool {
        self.exit_code == Some(0)
    }
}

/// Same as `docker exec {container} {cmd}`: runs `cmd` to completion.
pub async fn exec_in_container(
    docker: &Docker,
    container: &str,
    cmd: &[&str],
) -> Result<ExecOutput, String> {
    let options = CreateExecOptions {
        cmd: Some(cmd.to_vec()),
        attach_stdout: Some(true),
        attach_stderr: Some(true),
        ..Default::default()
    };
    let exec = docker
        .create_exec(container, options)
        .await
        .map_err(|e| format!("unable to create exec in {container}: {e}"))?;
    let mut stderr = String::new();
    if let StartExecResults::Attached { mut output, .. } =
        docker
            .start_exec(&exec.id, None)
            .await
            .map_err(|e| format!("unable to start exec in {container}: {e}"))?
    {
        while let Some(log) = output.next().await {
            if let LogOutput::StdErr { message } =
                log.map_err(|e| format!("unable to read exec output: {e}"))?
            {
                stderr.push_str(&String::from_utf8_lossy(&message));
            }
        }
    }
    let exit_code = docker
        .inspect_exec(&exec.id)
        .await
        .map_err(|e| format!("unable to inspect exec in {container}: {e}"))?
        .exit_code;
    Ok(ExecOutput { exit_code, stderr })
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    fn entry_paths(archive: &[u8]) -> Vec<String> {
        let mut paths: Vec<_> = tar::Archive::new(archive)
            .entries()
            .unwrap()
            .map(|entry| entry.unwrap().path().unwrap().display().to_string())
            .collect();
        paths.sort();
        paths
    }

    #[test]
    fn tar_directory_roots_contents_under_name() {
        let dir = tempfile::tempdir().unwrap();
        fs::create_dir_all(dir.path().join("chainstate/blocks")).unwrap();
        fs::write(dir.path().join("chainstate/blocks/0"), "block").unwrap();
        fs::write(dir.path().join("marf.sqlite"), "marf").unwrap();

        let archive = tar_directory(dir.path(), "devnet").unwrap();

        assert_eq!(
            entry_paths(&archive),
            [
                "devnet/",
                "devnet/chainstate",
                "devnet/chainstate/blocks",
                "devnet/chainstate/blocks/0",
                "devnet/marf.sqlite",
            ]
        );
    }

    #[test]
    fn tar_file_round_trips_through_untar_file() {
        let dir = tempfile::tempdir().unwrap();
        let src = dir.path().join("events.tsv");
        fs::write(&src, "a\tb\n").unwrap();

        let archive = tar_file(&src, "events_cache.tsv").unwrap();
        assert_eq!(entry_paths(&archive), ["events_cache.tsv"]);

        let dest = dir.path().join("out.tsv");
        untar_file(&archive, &dest).unwrap();
        assert_eq!(fs::read_to_string(dest).unwrap(), "a\tb\n");
    }

    #[test]
    fn untar_file_rejects_directories() {
        let dir = tempfile::tempdir().unwrap();
        let archive = tar_directory(dir.path(), "tmp").unwrap();
        assert!(untar_file(&archive, &dir.path().join("out")).is_err());
    }

    #[test]
    fn split_dest_uploads_into_parent() {
        assert_eq!(
            split_dest("/home/bitcoin/.bitcoin/regtest").unwrap(),
            ("/home/bitcoin/.bitcoin", "regtest")
        );
        assert_eq!(split_dest("/devnet").unwrap(), ("/", "devnet"));
        assert!(split_dest("/").is_err());
    }
}
