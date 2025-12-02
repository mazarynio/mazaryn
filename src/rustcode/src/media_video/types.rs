use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VideoMetadata {
    pub id: String,
    pub user_id: String,
    pub title: String,
    pub duration: f64,
    pub resolution: Resolution,
    pub codec: String,
    pub bitrate: u64,
    pub frame_rate: f32,
    pub file_size: u64,
    pub ipfs_cid: String,
    pub status: VideoStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum VideoStatus {
    Processing,
    Ready,
    Failed,
    Transcoding,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Resolution {
    pub width: u32,
    pub height: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlaybackSession {
    pub session_id: String,
    pub video_id: String,
    pub user_id: String,
    pub quality: String,
    pub position: f64,
    pub playback_rate: f32,
    pub volume: f32,
    pub buffering: bool,
    pub started_at: DateTime<Utc>,
    pub last_heartbeat: DateTime<Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ViewerStats {
    pub video_id: String,
    pub user_id: String,
    pub watch_duration: f64,
    pub completion_percentage: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LiveStream {
    pub stream_id: String,
    pub video_id: String,
    pub streamer_id: String,
    pub title: String,
    pub status: StreamStatus,
    pub ingest_url: String,
    pub playback_url: String,
    pub stream_key: String,
    pub latency_mode: LatencyMode,
    pub current_viewers: u32,
    pub peak_viewers: u32,
    pub viewers: Vec<String>,
    pub started_at: Option<DateTime<Utc>>,
    pub ended_at: Option<DateTime<Utc>>,
    pub current_bitrate: u64,
    pub current_resolution: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StreamStatus {
    Created,
    Live,
    Ended,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LatencyMode {
    Normal,
    LowLatency,
    UltraLow,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TranscodingJob {
    pub job_id: String,
    pub video_id: String,
    pub source_cid: String,
    pub target_qualities: Vec<QualityTarget>,
    pub status: TranscodingStatus,
    pub progress: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TranscodingStatus {
    Pending,
    Processing,
    Completed,
    Failed,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityTarget {
    pub name: String,
    pub resolution: Resolution,
    pub bitrate: u64,
    pub codec: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MediaPlayerConfig {
    pub adaptive_bitrate: bool,
    pub buffer_size_seconds: u32,
    pub enable_analytics: bool,
    pub enable_p2p: bool,
}

impl Default for MediaPlayerConfig {
    fn default() -> Self {
        Self {
            adaptive_bitrate: true,
            buffer_size_seconds: 30,
            enable_analytics: true,
            enable_p2p: false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VideoInfo {
    pub duration: f64,
    pub resolution: String,
    pub codec: String,
    pub bitrate: u64,
    pub frame_rate: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageStats {
    pub total_size_bytes: u64,
    pub file_count: u32,
    pub storage_path: String,
}
