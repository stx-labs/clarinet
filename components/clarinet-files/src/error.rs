use std::path::PathBuf;

/// Typed errors for file and path operations in `clarinet-files`.
#[derive(Debug, thiserror::Error)]
pub enum FileLocationError {
    #[error("unable to {operation} `{}`: {source}", path.display())]
    Io {
        path: PathBuf,
        operation: &'static str,
        source: std::io::Error,
    },

    #[error("unable to read `{}` as UTF-8: {source}", path.display())]
    Utf8 {
        path: PathBuf,
        source: std::string::FromUtf8Error,
    },

    #[error("unable to find project root from `{}`", path.display())]
    ProjectRootNotFound { path: PathBuf },

    #[error("no Clarinet.toml found for contract `{name}`")]
    ManifestNotFound { name: String },

    #[error("`{}` is not under `{}`", path.display(), base.display())]
    StripPrefixFailed { path: PathBuf, base: PathBuf },

    #[error("unable to convert path `{}` to URL", path.display())]
    PathToUrl { path: PathBuf },
}

/// Allow downstream callers that still use `Result<T, String>` to convert
/// typed errors transparently via the `?` operator.
impl From<FileLocationError> for String {
    fn from(err: FileLocationError) -> String {
        err.to_string()
    }
}
