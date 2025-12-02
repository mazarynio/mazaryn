use super::types::*;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct VideoMetrics {
    pub total_views: u64,
    pub unique_viewers: u64,
    pub total_watch_time: f64,
    pub completed_views: u64,
    pub buffer_events: u32,
    pub quality_changes: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalyticsReport {
    pub video_id: String,
    pub metrics: VideoMetrics,
    pub retention_curve: Vec<(f64, f64)>,
    pub geographic_breakdown: Vec<(String, u64)>,
}

pub struct AnalyticsManager {
    metrics: Arc<DashMap<String, VideoMetrics>>,
    views: Arc<DashMap<String, Vec<ViewerStats>>>,
    buffer_events: Arc<DashMap<String, Vec<BufferEvent>>>,
    quality_changes: Arc<DashMap<String, Vec<QualityChangeEvent>>>,
    geographic_data: Arc<DashMap<String, DashMap<String, u64>>>,
}

impl AnalyticsManager {
    pub fn new() -> Self {
        Self {
            metrics: Arc::new(DashMap::new()),
            views: Arc::new(DashMap::new()),
            buffer_events: Arc::new(DashMap::new()),
            quality_changes: Arc::new(DashMap::new()),
            geographic_data: Arc::new(DashMap::new()),
        }
    }

    pub async fn record_view(&self, stats: ViewerStats) {
        let video_id = stats.video_id.clone();

        self.views
            .entry(video_id.clone())
            .or_insert_with(Vec::new)
            .push(stats.clone());

        self.update_video_metrics(&video_id, |metrics| {
            metrics.total_views += 1;
            metrics.total_watch_time += stats.watch_duration;
            if stats.completion_percentage >= 90.0 {
                metrics.completed_views += 1;
            }
        });
    }

    pub async fn record_engagement(
        &self,
        video_id: String,
        user_id: String,
        event_type: EngagementEvent,
    ) {
        match event_type {
            EngagementEvent::BufferStart(timestamp) => {
                self.buffer_events
                    .entry(video_id.clone())
                    .or_insert_with(Vec::new)
                    .push(BufferEvent {
                        user_id,
                        timestamp,
                        duration: 0.0,
                    });
            }
            EngagementEvent::QualityChange(from, to) => {
                self.quality_changes
                    .entry(video_id.clone())
                    .or_insert_with(Vec::new)
                    .push(QualityChangeEvent {
                        user_id,
                        from_quality: from,
                        to_quality: to,
                    });

                self.update_video_metrics(&video_id, |metrics| {
                    metrics.quality_changes += 1;
                });
            }
            _ => {}
        }
    }

    pub async fn record_buffer_event(&self, video_id: String, user_id: String, duration: f64) {
        self.buffer_events
            .entry(video_id.clone())
            .or_insert_with(Vec::new)
            .push(BufferEvent {
                user_id,
                timestamp: 0.0,
                duration,
            });

        self.update_video_metrics(&video_id, |metrics| {
            metrics.buffer_events += 1;
        });
    }

    pub async fn record_quality_change(
        &self,
        _session_id: String,
        _user_id: String,
        from: String,
        to: String,
    ) {
        let video_id = "unknown".to_string();
        self.quality_changes
            .entry(video_id.clone())
            .or_insert_with(Vec::new)
            .push(QualityChangeEvent {
                user_id: _user_id,
                from_quality: from,
                to_quality: to,
            });
    }

    pub fn get_video_metrics(&self, video_id: &str) -> Option<VideoMetrics> {
        self.metrics.get(video_id).map(|m| m.value().clone())
    }

    pub async fn export_analytics(&self, video_id: &str) -> Result<AnalyticsReport, anyhow::Error> {
        let metrics = self
            .get_video_metrics(video_id)
            .unwrap_or_else(VideoMetrics::default);

        let retention_curve = self.get_retention_curve(video_id);
        let geographic_breakdown = self.get_geographic_breakdown(video_id);

        Ok(AnalyticsReport {
            video_id: video_id.to_string(),
            metrics,
            retention_curve,
            geographic_breakdown,
        })
    }

    fn get_retention_curve(&self, video_id: &str) -> Vec<(f64, f64)> {
        let views = self.views.get(video_id);
        if views.is_none() {
            return Vec::new();
        }

        let views = views.unwrap();
        let total_views = views.len() as f64;
        if total_views == 0.0 {
            return Vec::new();
        }

        let mut buckets = vec![0u64; 100];
        for view in views.iter() {
            let bucket = (view.completion_percentage.min(100.0) as usize).min(99);
            buckets[bucket] += 1;
        }

        buckets
            .iter()
            .enumerate()
            .map(|(i, &count)| (i as f64, (count as f64 / total_views) * 100.0))
            .collect()
    }

    fn get_geographic_breakdown(&self, video_id: &str) -> Vec<(String, u64)> {
        if let Some(geo_data) = self.geographic_data.get(video_id) {
            geo_data
                .iter()
                .map(|entry| (entry.key().clone(), *entry.value()))
                .collect()
        } else {
            Vec::new()
        }
    }

    fn update_video_metrics<F>(&self, video_id: &str, mut updater: F)
    where
        F: FnMut(&mut VideoMetrics),
    {
        let video_id_string = video_id.to_string();
        if let Some(mut metrics) = self.metrics.get_mut(&video_id_string) {
            updater(&mut metrics);
        } else {
            let mut new_metrics = VideoMetrics::default();
            updater(&mut new_metrics);
            self.metrics.insert(video_id_string, new_metrics);
        }
    }
}

#[derive(Debug, Clone)]
pub struct BufferEvent {
    pub user_id: String,
    pub timestamp: f64,
    pub duration: f64,
}

#[derive(Debug, Clone)]
pub struct QualityChangeEvent {
    pub user_id: String,
    pub from_quality: String,
    pub to_quality: String,
}

#[derive(Debug, Clone)]
pub enum EngagementEvent {
    Play,
    Pause,
    Seek(f64),
    BufferStart(f64),
    BufferEnd,
    QualityChange(String, String),
}
