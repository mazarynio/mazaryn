use crate::download_manager::{error::Result, types::DownloadInfo, DownloadError};
use std::path::PathBuf;
use tokio::fs;
use uuid::Uuid;

pub struct StorageManager {
    storage_path: PathBuf,
}

impl StorageManager {
    pub fn new(storage_path: PathBuf) -> Result<Self> {
        std::fs::create_dir_all(&storage_path)?;
        Ok(Self { storage_path })
    }

    pub async fn save_download_info(&self, info: &DownloadInfo) -> Result<()> {
        let path = self.info_path(&info.id);
        let json = serde_json::to_string_pretty(info)
            .map_err(|e| DownloadError::Failed(e.to_string()))?;
        fs::write(path, json).await?;
        Ok(())
    }

    pub async fn load_download_info(&self, id: &Uuid) -> Result<DownloadInfo> {
        let path = self.info_path(id);
        let json = fs::read_to_string(path).await?;
        serde_json::from_str(&json).map_err(|e| DownloadError::Failed(e.to_string()))
    }

    pub async fn delete_download_info(&self, id: &Uuid) -> Result<()> {
        let path = self.info_path(id);
        fs::remove_file(path).await?;
        Ok(())
    }

    pub async fn list_downloads(&self, user_id: Option<&str>) -> Result<Vec<DownloadInfo>> {
        let mut downloads = Vec::new();
        let mut entries = fs::read_dir(&self.storage_path).await?;

        while let Some(entry) = entries.next_entry().await? {
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Ok(json) = fs::read_to_string(&path).await {
                    if let Ok(info) = serde_json::from_str::<DownloadInfo>(&json) {
                        if let Some(uid) = user_id {
                            if info.user_id == uid {
                                downloads.push(info);
                            }
                        } else {
                            downloads.push(info);
                        }
                    }
                }
            }
        }

        Ok(downloads)
    }

    fn info_path(&self, id: &Uuid) -> PathBuf {
        self.storage_path.join(format!("{}.json", id))
    }
}
