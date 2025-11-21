use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use uuid::Uuid;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DownloadRequest {
    pub url: String,
    pub destination: PathBuf,
    pub dataset_id: Option<String>,
    pub competition_id: Option<String>,
    pub user_id: String,
    pub checksum: Option<String>,
    pub checksum_algorithm: Option<ChecksumAlgorithm>,
    pub priority: DownloadPriority,
    pub chunk_size: Option<usize>,
    pub max_connections: Option<usize>,
    pub headers: Option<std::collections::HashMap<String, String>>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum DownloadPriority {
    Low = 0,
    Normal = 1,
    High = 2,
    Critical = 3,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ChecksumAlgorithm {
    SHA256,
    SHA512,
    MD5,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DownloadStatus {
    Queued,
    Downloading,
    Paused,
    Completed,
    Failed,
    Verifying,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DownloadInfo {
    pub id: Uuid,
    pub url: String,
    pub destination: PathBuf,
    pub dataset_id: Option<String>,
    pub competition_id: Option<String>,
    pub user_id: String,
    pub status: DownloadStatus,
    pub total_size: Option<u64>,
    pub downloaded_size: u64,
    pub progress_percentage: f64,
    pub speed_bps: u64,
    pub eta_seconds: Option<u64>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub error: Option<String>,
    pub chunks_completed: usize,
    pub chunks_total: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChunkInfo {
    pub index: usize,
    pub start: u64,
    pub end: u64,
    pub downloaded: u64,
    pub completed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DownloadConfig {
    pub default_chunk_size: usize,
    pub default_max_connections: usize,
    pub max_concurrent_downloads: usize,
    pub retry_attempts: usize,
    pub retry_delay_ms: u64,
    pub timeout_seconds: u64,
    pub max_bandwidth_bps: Option<u64>,
    pub storage_path: PathBuf,
}

impl Default for DownloadConfig {
    fn default() -> Self {
        Self {
            default_chunk_size: 10 * 1024 * 1024, // 10 MB
            default_max_connections: 8,
            max_concurrent_downloads: 5,
            retry_attempts: 3,
            retry_delay_ms: 1000,
            timeout_seconds: 300,
            max_bandwidth_bps: None,
            storage_path: PathBuf::from("/tmp/downloads"),
        }
    }
}
