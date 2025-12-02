use super::types::StorageStats;
use anyhow::Result;
use bytes::Bytes;
use std::path::PathBuf;

pub struct MediaStorage {
    storage_path: PathBuf,
    ipfs_gateway: String,
    ipfs_api: String,
}

impl MediaStorage {
    pub fn new(storage_path: PathBuf, ipfs_gateway: String, ipfs_api: String) -> Self {
        Self {
            storage_path,
            ipfs_gateway,
            ipfs_api,
        }
    }

    pub async fn upload_to_ipfs(&self, data: Bytes) -> Result<String> {
        let client = reqwest::Client::new();
        let response = client
            .post(format!("{}/api/v0/add", self.ipfs_api))
            .body(data)
            .send()
            .await?;

        let result: serde_json::Value = response.json().await?;
        let cid = result["Hash"]
            .as_str()
            .ok_or_else(|| anyhow::anyhow!("No CID in response"))?
            .to_string();

        Ok(cid)
    }

    pub async fn download_from_ipfs(&self, cid: &str) -> Result<Bytes> {
        let client = reqwest::Client::new();
        let url = format!("{}/ipfs/{}", self.ipfs_gateway, cid);
        let response = client.get(&url).send().await?;
        let bytes = response.bytes().await?;
        Ok(bytes)
    }

    pub async fn store_local(&self, video_id: &str, data: Bytes) -> Result<()> {
        let video_dir = self.storage_path.join(video_id);
        tokio::fs::create_dir_all(&video_dir).await?;

        let file_path = video_dir.join("original.mp4");
        tokio::fs::write(&file_path, &data).await?;

        Ok(())
    }

    pub async fn read_local(&self, video_id: &str, filename: &str) -> Result<Bytes> {
        let file_path = self.storage_path.join(video_id).join(filename);
        let data = tokio::fs::read(&file_path).await?;
        Ok(Bytes::from(data))
    }

    pub async fn delete_local(&self, video_id: &str) -> Result<()> {
        let video_dir = self.storage_path.join(video_id);
        tokio::fs::remove_dir_all(&video_dir).await?;
        Ok(())
    }

    pub async fn store_quality_variant(
        &self,
        video_id: &str,
        quality: &str,
        data: Bytes,
    ) -> Result<String> {
        let video_dir = self.storage_path.join(video_id);
        tokio::fs::create_dir_all(&video_dir).await?;

        let filename = format!("{}.mp4", quality);
        let file_path = video_dir.join(&filename);
        tokio::fs::write(&file_path, &data).await?;

        self.upload_to_ipfs(data).await
    }

    pub async fn store_thumbnail(&self, video_id: &str, data: Bytes) -> Result<String> {
        let video_dir = self.storage_path.join(video_id).join("thumbnails");
        tokio::fs::create_dir_all(&video_dir).await?;

        let file_path = video_dir.join("thumbnail.jpg");
        tokio::fs::write(&file_path, &data).await?;

        self.upload_to_ipfs(data).await
    }

    pub async fn store_subtitle(
        &self,
        video_id: &str,
        language: &str,
        data: Bytes,
    ) -> Result<String> {
        let video_dir = self.storage_path.join(video_id).join("subtitles");
        tokio::fs::create_dir_all(&video_dir).await?;

        let filename = format!("{}.vtt", language);
        let file_path = video_dir.join(&filename);
        tokio::fs::write(&file_path, &data).await?;

        self.upload_to_ipfs(data).await
    }

    pub fn calculate_hash(&self, data: &[u8]) -> String {
        use sha2::{Digest, Sha256};
        let mut hasher = Sha256::new();
        hasher.update(data);
        format!("{:x}", hasher.finalize())
    }

    pub async fn pin_to_ipfs(&self, cid: &str) -> Result<()> {
        let client = reqwest::Client::new();
        client
            .post(format!("{}/api/v0/pin/add?arg={}", self.ipfs_api, cid))
            .send()
            .await?;
        Ok(())
    }

    pub async fn unpin_from_ipfs(&self, cid: &str) -> Result<()> {
        let client = reqwest::Client::new();
        client
            .post(format!("{}/api/v0/pin/rm?arg={}", self.ipfs_api, cid))
            .send()
            .await?;
        Ok(())
    }

    pub async fn get_storage_stats(&self) -> Result<StorageStats> {
        let mut total_size = 0u64;
        let mut file_count = 0usize;

        let mut entries = tokio::fs::read_dir(&self.storage_path).await?;
        while let Some(entry) = entries.next_entry().await? {
            if entry.file_type().await?.is_dir() {
                let mut sub_entries = tokio::fs::read_dir(entry.path()).await?;
                while let Some(sub_entry) = sub_entries.next_entry().await? {
                    if sub_entry.file_type().await?.is_file() {
                        let metadata = sub_entry.metadata().await?;
                        total_size += metadata.len();
                        file_count += 1;
                    }
                }
            }
        }

        Ok(StorageStats {
            total_size_bytes: total_size,
            file_count: file_count as u32,
            storage_path: self.storage_path.to_string_lossy().to_string(),
        })
    }
}
