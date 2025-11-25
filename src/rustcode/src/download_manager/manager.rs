use crate::download_manager::{
    chunk::ChunkDownloader,
    error::{DownloadError, Result},
    progress::ProgressTracker,
    queue::DownloadQueue,
    storage::StorageManager,
    types::*,
};
use chrono::Utc;
use dashmap::DashMap;
use sha2::{Digest, Sha256};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::fs::{File, OpenOptions};
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use uuid::Uuid;

pub struct DownloadManager {
    config: DownloadConfig,
    queue: Arc<DownloadQueue>,
    active_downloads: Arc<DashMap<Uuid, Arc<ProgressTracker>>>,
    storage: Arc<StorageManager>,
}

impl DownloadManager {
    pub fn new(config: DownloadConfig) -> Result<Self> {
        let storage = StorageManager::new(config.storage_path.clone())?;

        Ok(Self {
            queue: Arc::new(DownloadQueue::new(config.max_concurrent_downloads)),
            active_downloads: Arc::new(DashMap::new()),
            storage: Arc::new(storage),
            config,
        })
    }

    pub async fn start_download(&self, request: DownloadRequest) -> Result<Uuid> {
        let id = Uuid::new_v4();

        let destination = if request.destination.is_absolute() {
            request.destination.clone()
        } else {
            self.config.storage_path.join(&request.destination)
        };

        if let Some(parent) = destination.parent() {
            tokio::fs::create_dir_all(parent).await?;
        }

        let info = DownloadInfo {
            id,
            url: request.url.clone(),
            destination: destination.clone(),
            dataset_id: request.dataset_id.clone(),
            competition_id: request.competition_id.clone(),
            user_id: request.user_id.clone(),
            status: DownloadStatus::Queued,
            total_size: request.expected_size,
            downloaded_size: 0,
            progress_percentage: 0.0,
            speed_bps: 0,
            eta_seconds: None,
            started_at: None,
            completed_at: None,
            error: None,
            chunks_completed: 0,
            chunks_total: 0,
        };

        self.storage.save_download_info(&info).await?;

        self.queue.enqueue(id, request);

        let manager = self.clone_arc();
        tokio::spawn(async move {
            manager.process_queue().await;
        });

        Ok(id)
    }

    async fn process_queue(&self) {
        while let Some((id, request)) = self.queue.dequeue().await {
            if let Err(e) = self.execute_download(id, request).await {
                log::error!("Download {} failed: {}", id, e);

                if let Some(tracker) = self.active_downloads.get(&id) {
                    let mut info = tracker.get_info();
                    info.status = DownloadStatus::Failed;
                    info.error = Some(e.to_string());
                    let _ = self.storage.save_download_info(&info).await;
                }
            }

            self.active_downloads.remove(&id);
        }
    }

    async fn execute_download(&self, id: Uuid, request: DownloadRequest) -> Result<()> {
        log::info!("Starting download {}: {}", id, request.url);

        let chunk_size = request.chunk_size.unwrap_or(self.config.default_chunk_size);
        let max_connections = request
            .max_connections
            .unwrap_or(self.config.default_max_connections);

        let downloader = ChunkDownloader::new(request.url.clone(), max_connections);

        let file_size = if let Some(expected) = request.expected_size {
            log::info!("Using expected file size: {} bytes", expected);
            expected
        } else {
            match downloader.get_file_size().await {
                Ok(Some(size)) => {
                    log::info!("Download {} size: {} bytes", id, size);
                    size
                }
                Ok(None) | Err(_) => {
                    log::warn!("Could not determine file size, attempting download without size");

                    let mut info = self.storage.load_download_info(&id).await?;
                    info.status = DownloadStatus::Downloading;
                    info.started_at = Some(Utc::now());
                    self.storage.save_download_info(&info).await?;

                    return self
                        .execute_download_without_size(id, request, downloader)
                        .await;
                }
            }
        };

        log::info!("Download {} size: {} bytes", id, file_size);

        let supports_range = downloader.supports_range_requests().await.unwrap_or(false);

        let chunks = if supports_range && file_size > chunk_size as u64 {
            ChunkDownloader::calculate_chunks(file_size, chunk_size)
        } else {
            vec![ChunkInfo {
                index: 0,
                start: 0,
                end: file_size - 1,
                downloaded: 0,
                completed: false,
            }]
        };

        log::info!(
            "Download {} will use {} chunks of ~{} MB each",
            id,
            chunks.len(),
            chunk_size / (1024 * 1024)
        );

        let mut info = self.storage.load_download_info(&id).await?;
        info.status = DownloadStatus::Downloading;
        info.total_size = Some(file_size);
        info.started_at = Some(Utc::now());
        info.chunks_total = chunks.len();
        self.storage.save_download_info(&info).await?;

        let tracker = Arc::new(ProgressTracker::new(info.clone(), chunks.clone()));
        self.active_downloads.insert(id, tracker.clone());

        self.preallocate_file(&request.destination, file_size)
            .await?;

        let mut tasks = Vec::new();

        for chunk in chunks {
            let downloader_clone = ChunkDownloader::new(request.url.clone(), max_connections);
            let destination = request.destination.clone();
            let tracker_clone = tracker.clone();
            let storage_clone = self.storage.clone();
            let download_id = id;

            let task = tokio::spawn(async move {
                let storage_for_callback = storage_clone.clone();
                let tracker_for_callback = tracker_clone.clone();

                let result = downloader_clone
                    .download_chunk(chunk.clone(), destination, move |chunk_idx, downloaded| {
                        tracker_for_callback.update_chunk_progress(chunk_idx, downloaded);

                        let info = tracker_for_callback.get_info();
                        let storage_ref = storage_for_callback.clone();
                        tokio::spawn(async move {
                            let _ = storage_ref.save_download_info(&info).await;
                        });
                    })
                    .await;

                if let Err(e) = result {
                    log::error!(
                        "Chunk {} of download {} failed: {}",
                        chunk.index,
                        download_id,
                        e
                    );
                    return Err(e);
                }

                Ok(())
            });

            tasks.push(task);
        }

        for task in tasks {
            task.await
                .map_err(|e| DownloadError::Failed(e.to_string()))??;
        }

        log::info!("Download {} completed, verifying...", id);

        let mut info = tracker.get_info();
        info.status = DownloadStatus::Verifying;
        self.storage.save_download_info(&info).await?;

        if let Some(expected_checksum) = request.checksum {
            let actual_checksum = self.calculate_checksum(&request.destination).await?;

            if expected_checksum != actual_checksum {
                return Err(DownloadError::ChecksumMismatch {
                    expected: expected_checksum,
                    actual: actual_checksum,
                });
            }

            log::info!("Download {} checksum verified", id);
        }

        info.status = DownloadStatus::Completed;
        info.completed_at = Some(Utc::now());
        info.progress_percentage = 100.0;
        self.storage.save_download_info(&info).await?;

        log::info!("Download {} finished successfully", id);

        Ok(())
    }

    async fn execute_download_without_size(
        &self,
        id: Uuid,
        request: DownloadRequest,
        downloader: ChunkDownloader,
    ) -> Result<()> {
        log::info!("Downloading without known size: {}", id);

        let response = reqwest::Client::builder()
            .timeout(std::time::Duration::from_secs(300))
            .user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
            .build()?
            .get(&request.url)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(DownloadError::Failed(format!(
                "HTTP error: {}",
                response.status()
            )));
        }

        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(&request.destination)
            .await?;

        let mut stream = response.bytes_stream();
        let mut downloaded = 0u64;
        use futures::StreamExt;

        while let Some(bytes_result) = stream.next().await {
            let bytes = bytes_result?;
            let len = bytes.len() as u64;

            use tokio::io::AsyncWriteExt;
            file.write_all(&bytes).await?;
            downloaded += len;

            if let Ok(mut info) = self.storage.load_download_info(&id).await {
                info.downloaded_size = downloaded;
                info.total_size = None;
                let _ = self.storage.save_download_info(&info).await;
            }
        }

        file.flush().await?;

        let mut info = self.storage.load_download_info(&id).await?;
        info.status = DownloadStatus::Completed;
        info.completed_at = Some(Utc::now());
        info.downloaded_size = downloaded;
        info.total_size = Some(downloaded);
        info.progress_percentage = 100.0;
        self.storage.save_download_info(&info).await?;

        log::info!("Download {} completed without known size", id);

        Ok(())
    }

    async fn preallocate_file(&self, path: &PathBuf, size: u64) -> Result<()> {
        let file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(path)
            .await?;

        file.set_len(size).await?;
        Ok(())
    }

    async fn calculate_checksum(&self, path: &PathBuf) -> Result<String> {
        let mut file = File::open(path).await?;
        let mut hasher = Sha256::new();
        let mut buffer = vec![0u8; 1024 * 1024];

        loop {
            let n = file.read(&mut buffer).await?;
            if n == 0 {
                break;
            }
            hasher.update(&buffer[..n]);
        }

        Ok(hex::encode(hasher.finalize()))
    }

    pub async fn pause_download(&self, id: &Uuid) -> Result<()> {
        if let Some(tracker) = self.active_downloads.get(id) {
            let mut info = tracker.get_info();
            info.status = DownloadStatus::Paused;
            self.storage.save_download_info(&info).await?;

            self.active_downloads.remove(id);

            Ok(())
        } else {
            Err(DownloadError::NotFound(id.to_string()))
        }
    }

    pub async fn resume_download(&self, id: &Uuid) -> Result<()> {
        let request = self
            .queue
            .get(id)
            .ok_or_else(|| DownloadError::NotFound(id.to_string()))?;

        let mut info = self.storage.load_download_info(id).await?;
        info.status = DownloadStatus::Queued;
        self.storage.save_download_info(&info).await?;

        self.queue.enqueue(*id, request);

        let manager = self.clone_arc();
        tokio::spawn(async move {
            manager.process_queue().await;
        });

        Ok(())
    }

    pub async fn cancel_download(&self, id: &Uuid) -> Result<()> {
        self.queue.remove(id);

        self.active_downloads.remove(id);

        if let Ok(mut info) = self.storage.load_download_info(id).await {
            info.status = DownloadStatus::Failed;
            info.error = Some("Cancelled by user".to_string());
            self.storage.save_download_info(&info).await?;
        }

        Ok(())
    }

    pub async fn get_download_info(&self, id: &Uuid) -> Result<DownloadInfo> {
        if let Some(tracker) = self.active_downloads.get(id) {
            return Ok(tracker.get_info());
        }

        self.storage.load_download_info(id).await
    }

    pub async fn list_downloads(&self, user_id: Option<&str>) -> Result<Vec<DownloadInfo>> {
        self.storage.list_downloads(user_id).await
    }

    pub async fn get_user_downloads(&self, user_id: &str) -> Result<Vec<DownloadInfo>> {
        self.storage.list_downloads(Some(user_id)).await
    }

    pub async fn delete_download(&self, id: &Uuid) -> Result<()> {
        let _ = self.cancel_download(id).await;

        if let Ok(info) = self.storage.load_download_info(id).await {
            let _ = tokio::fs::remove_file(&info.destination).await;
        }

        self.storage.delete_download_info(id).await
    }

    fn clone_arc(&self) -> Arc<Self> {
        Arc::new(Self {
            config: self.config.clone(),
            queue: self.queue.clone(),
            active_downloads: self.active_downloads.clone(),
            storage: self.storage.clone(),
        })
    }
}
