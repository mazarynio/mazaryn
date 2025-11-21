use crate::download_manager::types::{ChunkInfo, DownloadInfo};
use parking_lot::RwLock;
use std::sync::Arc;
use std::time::{Duration, Instant};

pub struct ProgressTracker {
    info: Arc<RwLock<DownloadInfo>>,
    chunks: Arc<RwLock<Vec<ChunkInfo>>>,
    start_time: Instant,
    last_update: Arc<RwLock<Instant>>,
    last_downloaded: Arc<RwLock<u64>>,
}

impl ProgressTracker {
    pub fn new(info: DownloadInfo, chunks: Vec<ChunkInfo>) -> Self {
        Self {
            info: Arc::new(RwLock::new(info)),
            chunks: Arc::new(RwLock::new(chunks)),
            start_time: Instant::now(),
            last_update: Arc::new(RwLock::new(Instant::now())),
            last_downloaded: Arc::new(RwLock::new(0)),
        }
    }

    pub fn update_chunk_progress(&self, chunk_index: usize, downloaded: u64) {
        let mut chunks = self.chunks.write();
        if let Some(chunk) = chunks.get_mut(chunk_index) {
            chunk.downloaded = downloaded;
            chunk.completed = downloaded >= (chunk.end - chunk.start + 1);
        }
        drop(chunks);

        self.update_overall_progress();
    }

    fn update_overall_progress(&self) {
        let chunks = self.chunks.read();
        let total_downloaded: u64 = chunks.iter().map(|c| c.downloaded).sum();
        let chunks_completed = chunks.iter().filter(|c| c.completed).count();
        let chunks_total = chunks.len();
        drop(chunks);

        let mut info = self.info.write();
        info.downloaded_size = total_downloaded;
        info.chunks_completed = chunks_completed;
        info.chunks_total = chunks_total;

        if let Some(total_size) = info.total_size {
            info.progress_percentage = (total_downloaded as f64 / total_size as f64) * 100.0;

            let now = Instant::now();
            let elapsed = now.duration_since(*self.last_update.read());

            if elapsed >= Duration::from_secs(1) {
                let last_downloaded = *self.last_downloaded.read();
                let downloaded_since_last = total_downloaded.saturating_sub(last_downloaded);

                info.speed_bps = (downloaded_since_last as f64 / elapsed.as_secs_f64()) as u64;

                if info.speed_bps > 0 {
                    let remaining = total_size.saturating_sub(total_downloaded);
                    info.eta_seconds = Some(remaining / info.speed_bps);
                }

                *self.last_update.write() = now;
                *self.last_downloaded.write() = total_downloaded;
            }
        }
    }

    pub fn get_info(&self) -> DownloadInfo {
        self.info.read().clone()
    }

    pub fn get_chunks(&self) -> Vec<ChunkInfo> {
        self.chunks.read().clone()
    }
}
