use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SystemMetrics {
    pub total_sessions: usize,
    pub active_sessions: usize,
    pub total_streams: usize,
    pub live_streams: usize,
    pub total_viewers: u32,
    pub transcoding_jobs: usize,
    pub active_transcoding: usize,
    pub total_videos: usize,
    pub total_storage_bytes: u64,
    pub uptime_seconds: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    pub avg_bitrate: f32,
    pub avg_buffer_duration: f32,
    pub quality_switches: u32,
    pub seek_operations: u32,
    pub errors: u32,
}

pub struct MetricsCollector {
    start_time: SystemTime,
    session_count: Arc<DashMap<String, u64>>,
    stream_count: Arc<DashMap<String, u64>>,
    performance: Arc<DashMap<String, PerformanceMetrics>>,
}

impl MetricsCollector {
    pub fn new() -> Self {
        Self {
            start_time: SystemTime::now(),
            session_count: Arc::new(DashMap::new()),
            stream_count: Arc::new(DashMap::new()),
            performance: Arc::new(DashMap::new()),
        }
    }

    pub fn record_session(&self, video_id: &str) {
        self.session_count
            .entry(video_id.to_string())
            .and_modify(|e| *e += 1)
            .or_insert(1);
    }

    pub fn record_stream(&self, stream_id: &str) {
        self.stream_count
            .entry(stream_id.to_string())
            .and_modify(|e| *e += 1)
            .or_insert(1);
    }

    pub fn record_quality_switch(&self, video_id: &str) {
        self.performance
            .entry(video_id.to_string())
            .and_modify(|e| e.quality_switches += 1)
            .or_insert(PerformanceMetrics {
                avg_bitrate: 0.0,
                avg_buffer_duration: 0.0,
                quality_switches: 1,
                seek_operations: 0,
                errors: 0,
            });
    }

    pub fn record_error(&self, video_id: &str) {
        self.performance
            .entry(video_id.to_string())
            .and_modify(|e| e.errors += 1)
            .or_insert(PerformanceMetrics {
                avg_bitrate: 0.0,
                avg_buffer_duration: 0.0,
                quality_switches: 0,
                seek_operations: 0,
                errors: 1,
            });
    }

    pub fn get_system_metrics(&self, total_videos: usize, total_storage: u64) -> SystemMetrics {
        let uptime = self.start_time.elapsed().unwrap_or_default();

        SystemMetrics {
            total_sessions: self.session_count.len(),
            active_sessions: 0,
            total_streams: self.stream_count.len(),
            live_streams: 0,
            total_viewers: 0,
            transcoding_jobs: 0,
            active_transcoding: 0,
            total_videos,
            total_storage_bytes: total_storage,
            uptime_seconds: uptime.as_secs(),
        }
    }

    pub fn get_performance_metrics(&self, video_id: &str) -> Option<PerformanceMetrics> {
        self.performance.get(video_id).map(|e| e.value().clone())
    }
}
