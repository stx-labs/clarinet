use std::process::Command;

pub fn run_docker_command(
    command: &str,
    docker_host: Option<&str>,
) -> std::io::Result<std::process::Output> {
    let mut cmd = if cfg!(target_os = "windows") {
        let mut c = Command::new("cmd");
        c.args(["/C", command]);
        c
    } else {
        let mut c = Command::new("sh");
        c.arg("-c").arg(command);
        c
    };
    if let Some(host) = docker_host {
        cmd.env("DOCKER_HOST", host);
    }
    cmd.output()
}
