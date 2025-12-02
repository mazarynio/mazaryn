use super::types::*;
use anyhow::Result;
use chrono::Utc;
use dashmap::DashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct MediaPlayer {
    sessions: Arc<DashMap<String, PlaybackSession>>,
    videos: Arc<DashMap<String, VideoMetadata>>,
    config: Arc<RwLock<MediaPlayerConfig>>,
}

impl MediaPlayer {
    pub fn new(config: MediaPlayerConfig) -> Self {
        Self {
            sessions: Arc::new(DashMap::new()),
            videos: Arc::new(DashMap::new()),
            config: Arc::new(RwLock::new(config)),
        }
    }

    pub async fn create_session(
        &self,
        video_id: String,
        user_id: String,
        quality: Option<String>,
    ) -> Result<PlaybackSession> {
        let video = self
            .videos
            .get(&video_id)
            .ok_or_else(|| anyhow::anyhow!("Video not found"))?;

        if video.status != VideoStatus::Ready {
            return Err(anyhow::anyhow!("Video not ready for playback"));
        }

        let config = self.config.read().await;
        let quality = quality.or_else(|| Some("720p".to_string())).unwrap();

        let session = PlaybackSession {
            session_id: uuid::Uuid::new_v4().to_string(),
            video_id: video_id.clone(),
            user_id,
            quality,
            position: 0.0,
            playback_rate: 1.0,
            volume: 1.0,
            buffering: false,
            started_at: Utc::now(),
            last_heartbeat: Utc::now(),
        };

        self.sessions
            .insert(session.session_id.clone(), session.clone());

        Ok(session)
    }

    pub fn get_session(&self, session_id: &str) -> Option<PlaybackSession> {
        self.sessions.get(session_id).map(|s| s.value().clone())
    }

    pub fn get_all_sessions(&self) -> Vec<PlaybackSession> {
        self.sessions
            .iter()
            .map(|entry| entry.value().clone())
            .collect()
    }

    pub async fn update_position(&self, session_id: &str, position: f64) -> Result<()> {
        let mut session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| anyhow::anyhow!("Session not found"))?;

        session.position = position;
        session.last_heartbeat = Utc::now();
        Ok(())
    }

    pub async fn update_quality(&self, session_id: &str, quality: String) -> Result<()> {
        let mut session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| anyhow::anyhow!("Session not found"))?;

        session.quality = quality;
        session.last_heartbeat = Utc::now();
        Ok(())
    }

    pub async fn set_playback_rate(&self, session_id: &str, rate: f32) -> Result<()> {
        if rate < 0.0 || rate > 2.0 {
            return Err(anyhow::anyhow!("Playback rate must be between 0.0 and 2.0"));
        }

        let mut session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| anyhow::anyhow!("Session not found"))?;

        session.playback_rate = rate;
        session.last_heartbeat = Utc::now();
        Ok(())
    }

    pub async fn set_volume(&self, session_id: &str, volume: f32) -> Result<()> {
        if volume < 0.0 || volume > 1.0 {
            return Err(anyhow::anyhow!("Volume must be between 0.0 and 1.0"));
        }

        let mut session = self
            .sessions
            .get_mut(session_id)
            .ok_or_else(|| anyhow::anyhow!("Session not found"))?;

        session.volume = volume;
        session.last_heartbeat = Utc::now();
        Ok(())
    }

    pub async fn close_session(&self, session_id: &str) -> Result<ViewerStats> {
        let (_, session) = self
            .sessions
            .remove(session_id)
            .ok_or_else(|| anyhow::anyhow!("Session not found"))?;

        let video = self
            .videos
            .get(&session.video_id)
            .ok_or_else(|| anyhow::anyhow!("Video not found"))?;

        let watch_duration = (Utc::now() - session.started_at).num_seconds() as f64;

        let completion_percentage = if video.duration > 0.0 {
            ((session.position / video.duration * 100.0).min(100.0)) as f64
        } else {
            0.0
        };

        Ok(ViewerStats {
            video_id: session.video_id,
            user_id: session.user_id,
            watch_duration,
            completion_percentage,
        })
    }

    pub fn register_video(&self, video: VideoMetadata) {
        self.videos.insert(video.id.clone(), video);
    }

    pub fn get_video(&self, video_id: &str) -> Option<VideoMetadata> {
        self.videos.get(video_id).map(|v| v.value().clone())
    }

    pub fn update_video_status(&self, video_id: &str, status: VideoStatus) -> Result<()> {
        let mut video = self
            .videos
            .get_mut(video_id)
            .ok_or_else(|| anyhow::anyhow!("Video not found"))?;

        video.status = status;
        Ok(())
    }

    pub async fn cleanup_inactive_sessions(&self, timeout_seconds: i64) {
        let now = Utc::now();
        let to_remove: Vec<String> = self
            .sessions
            .iter()
            .filter(|entry| (now - entry.value().last_heartbeat).num_seconds() > timeout_seconds)
            .map(|entry| entry.key().clone())
            .collect();

        for session_id in to_remove {
            self.sessions.remove(&session_id);
        }
    }
}
