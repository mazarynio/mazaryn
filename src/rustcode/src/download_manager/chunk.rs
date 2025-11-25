use crate::download_manager::{error::Result, types::ChunkInfo, DownloadError};
use futures::StreamExt;
use reqwest::Client;
use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;
use tokio::fs::OpenOptions;
use tokio::io::{AsyncSeekExt, AsyncWriteExt};
use tokio::sync::Semaphore;

pub struct ChunkDownloader {
    client: Client,
    url: String,
    semaphore: Arc<Semaphore>,
}

impl ChunkDownloader {
    pub fn new(url: String, max_connections: usize) -> Self {
        let client = Client::builder()
            .timeout(Duration::from_secs(300))
            .user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
            .redirect(reqwest::redirect::Policy::limited(10))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            client,
            url,
            semaphore: Arc::new(Semaphore::new(max_connections)),
        }
    }

    pub async fn download_chunk(
        &self,
        chunk: ChunkInfo,
        destination: PathBuf,
        progress_callback: impl Fn(usize, u64) + Send + 'static,
    ) -> Result<()> {
        let _permit = self.semaphore.acquire().await.unwrap();

        let range_header = format!("bytes={}-{}", chunk.start, chunk.end);

        let response = self
            .client
            .get(&self.url)
            .header("Range", range_header)
            .send()
            .await?;

        if !response.status().is_success() && response.status().as_u16() != 206 {
            return Err(DownloadError::Failed(format!(
                "Failed to download chunk {}: {}",
                chunk.index,
                response.status()
            )));
        }

        let mut file = OpenOptions::new()
            .write(true)
            .create(true)
            .open(&destination)
            .await?;

        file.seek(std::io::SeekFrom::Start(chunk.start)).await?;

        let mut stream = response.bytes_stream();
        let mut downloaded = 0u64;

        while let Some(bytes_result) = stream.next().await {
            let bytes = bytes_result?;
            let len = bytes.len() as u64;

            file.write_all(&bytes).await?;
            downloaded += len;

            progress_callback(chunk.index, downloaded);
        }

        file.flush().await?;

        Ok(())
    }

    pub async fn get_file_size(&self) -> Result<Option<u64>> {
        let mut retries = 3;
        let mut last_error = None;

        while retries > 0 {
            match self.try_get_file_size().await {
                Ok(Some(size)) => return Ok(Some(size)),
                Ok(None) => {
                    log::warn!("Could not determine file size from headers, trying GET request");
                    match self.try_get_file_size_from_get().await {
                        Ok(size) => return Ok(Some(size)),
                        Err(e) => {
                            last_error = Some(e);
                            retries -= 1;
                            if retries > 0 {
                                tokio::time::sleep(Duration::from_secs(2)).await;
                            }
                        }
                    }
                }
                Err(e) => {
                    last_error = Some(e);
                    retries -= 1;
                    if retries > 0 {
                        tokio::time::sleep(Duration::from_secs(2)).await;
                    }
                }
            }
        }

        Err(last_error.unwrap_or_else(|| {
            DownloadError::Failed("Could not determine file size after retries".to_string())
        }))
    }

    async fn try_get_file_size(&self) -> Result<Option<u64>> {
        let response = self
            .client
            .head(&self.url)
            .timeout(Duration::from_secs(30))
            .send()
            .await?;

        if !response.status().is_success() {
            return Ok(None);
        }

        if let Some(content_length) = response.headers().get("content-length") {
            if let Ok(size_str) = content_length.to_str() {
                if let Ok(size) = size_str.parse::<u64>() {
                    return Ok(Some(size));
                }
            }
        }

        Ok(None)
    }

    async fn try_get_file_size_from_get(&self) -> Result<u64> {
        let response = self
            .client
            .get(&self.url)
            .header("Range", "bytes=0-0")
            .timeout(Duration::from_secs(30))
            .send()
            .await?;

        if let Some(content_range) = response.headers().get("content-range") {
            if let Ok(range_str) = content_range.to_str() {
                if let Some(total) = range_str.split('/').nth(1) {
                    if let Ok(size) = total.parse::<u64>() {
                        return Ok(size);
                    }
                }
            }
        }

        if let Some(content_length) = response.headers().get("content-length") {
            if let Ok(size_str) = content_length.to_str() {
                if let Ok(size) = size_str.parse::<u64>() {
                    return Ok(size);
                }
            }
        }

        Err(DownloadError::Failed(
            "Could not determine file size".to_string(),
        ))
    }

    pub async fn supports_range_requests(&self) -> Result<bool> {
        match self.client.head(&self.url).send().await {
            Ok(response) => Ok(response
                .headers()
                .get("accept-ranges")
                .map(|v| v.to_str().unwrap_or("") == "bytes")
                .unwrap_or(false)),
            Err(_) => Ok(false),
        }
    }

    pub fn calculate_chunks(file_size: u64, chunk_size: usize) -> Vec<ChunkInfo> {
        let chunk_size = chunk_size as u64;
        let num_chunks = (file_size + chunk_size - 1) / chunk_size;

        (0..num_chunks)
            .map(|i| {
                let start = i * chunk_size;
                let end = std::cmp::min(start + chunk_size - 1, file_size - 1);

                ChunkInfo {
                    index: i as usize,
                    start,
                    end,
                    downloaded: 0,
                    completed: false,
                }
            })
            .collect()
    }
}
