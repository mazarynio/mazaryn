use super::types::*;
use anyhow::Result;
use dashmap::DashMap;
use std::sync::Arc;

pub struct StreamManager {
    streams: Arc<DashMap<String, LiveStream>>,
}

impl StreamManager {
    pub fn new() -> Self {
        Self {
            streams: Arc::new(DashMap::new()),
        }
    }

    pub async fn create_stream(
        &self,
        video_id: String,
        streamer_id: String,
        title: String,
        latency_mode: LatencyMode,
    ) -> Result<LiveStream> {
        let stream_id = uuid::Uuid::new_v4().to_string();
        let stream_key = uuid::Uuid::new_v4().to_string();

        let ingest_url = format!("rtmp://ingest.mazaryn.com/live/{}", stream_key);
        let playback_url = format!("https://stream.mazaryn.com/live/{}", stream_id);

        let stream = LiveStream {
            stream_id: stream_id.clone(),
            video_id,
            streamer_id,
            title,
            status: StreamStatus::Created,
            ingest_url,
            playback_url,
            stream_key,
            latency_mode,
            current_viewers: 0,
            peak_viewers: 0,
            viewers: Vec::new(),
            started_at: None,
            ended_at: None,
            current_bitrate: 0,
            current_resolution: String::new(),
        };

        self.streams.insert(stream_id.clone(), stream.clone());
        Ok(stream)
    }

    pub async fn start_stream(&self, stream_id: &str) -> Result<()> {
        if let Some(mut stream) = self.streams.get_mut(stream_id) {
            stream.status = StreamStatus::Live;
            stream.started_at = Some(chrono::Utc::now());
            Ok(())
        } else {
            Err(anyhow::anyhow!("Stream not found"))
        }
    }

    pub async fn end_stream(&self, stream_id: &str) -> Result<()> {
        if let Some(mut stream) = self.streams.get_mut(stream_id) {
            stream.status = StreamStatus::Ended;
            stream.ended_at = Some(chrono::Utc::now());
            Ok(())
        } else {
            Err(anyhow::anyhow!("Stream not found"))
        }
    }

    pub fn get_stream(&self, stream_id: &str) -> Option<LiveStream> {
        self.streams.get(stream_id).map(|s| s.value().clone())
    }

    pub fn list_live_streams(&self) -> Vec<LiveStream> {
        self.streams
            .iter()
            .filter(|entry| matches!(entry.value().status, StreamStatus::Live))
            .map(|entry| entry.value().clone())
            .collect()
    }

    pub async fn add_viewer(&self, stream_id: &str, user_id: String) -> Result<()> {
        if let Some(mut stream) = self.streams.get_mut(stream_id) {
            if !stream.viewers.contains(&user_id) {
                stream.viewers.push(user_id);
            }
            let viewers = &stream.viewers;
            stream.current_viewers = viewers.len() as u32;
            if stream.current_viewers > stream.peak_viewers {
                stream.peak_viewers = stream.current_viewers;
            }
            Ok(())
        } else {
            Err(anyhow::anyhow!("Stream not found"))
        }
    }

    pub async fn remove_viewer(&self, stream_id: &str, user_id: &str) -> Result<()> {
        if let Some(mut stream) = self.streams.get_mut(stream_id) {
            stream.viewers.retain(|id| id != user_id);
            let viewers = &stream.viewers;
            stream.current_viewers = viewers.len() as u32;
            Ok(())
        } else {
            Err(anyhow::anyhow!("Stream not found"))
        }
    }

    pub fn get_viewers(&self, stream_id: &str) -> Vec<String> {
        self.streams
            .get(stream_id)
            .map(|s| s.value().viewers.clone())
            .unwrap_or_default()
    }

    pub fn get_viewer_count(&self, stream_id: &str) -> u32 {
        self.streams
            .get(stream_id)
            .map(|s| s.value().current_viewers)
            .unwrap_or(0)
    }

    pub async fn update_stream_stats(&self, stream_id: &str, bitrate: u64, resolution: String) {
        if let Some(mut stream) = self.streams.get_mut(stream_id) {
            stream.current_bitrate = bitrate;
            stream.current_resolution = resolution;
        }
    }
}
