use thiserror::Error;

#[derive(Error, Debug)]
pub enum DownloadError {
    #[error("HTTP error: {0}")]
    Http(#[from] reqwest::Error),

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Download not found: {0}")]
    NotFound(String),

    #[error("Checksum mismatch: expected {expected}, got {actual}")]
    ChecksumMismatch { expected: String, actual: String },

    #[error("Invalid range header")]
    InvalidRangeHeader,

    #[error("Server does not support range requests")]
    RangeNotSupported,

    #[error("Download failed: {0}")]
    Failed(String),

    #[error("Queue is full")]
    QueueFull,

    #[error("Download cancelled")]
    Cancelled,

    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
}

pub type Result<T> = std::result::Result<T, DownloadError>;
