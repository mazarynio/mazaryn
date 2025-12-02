use serde::{Deserialize, Serialize};
use std::time::SystemTime;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthStatus {
    pub status: String,
    pub timestamp: u64,
    pub checks: HealthChecks,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HealthChecks {
    pub storage: bool,
    pub ipfs: bool,
    pub database: bool,
    pub ffmpeg: bool,
}

pub async fn check_health() -> HealthStatus {
    let checks = HealthChecks {
        storage: check_storage().await,
        ipfs: check_ipfs().await,
        database: true,
        ffmpeg: check_ffmpeg().await,
    };

    let all_healthy = checks.storage && checks.ipfs && checks.database && checks.ffmpeg;

    HealthStatus {
        status: if all_healthy {
            "healthy".to_string()
        } else {
            "degraded".to_string()
        },
        timestamp: SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap()
            .as_secs(),
        checks,
    }
}

async fn check_storage() -> bool {
    let test_path = std::path::Path::new("/tmp/mazaryn_videos/.health_check");
    std::fs::write(test_path, b"test").is_ok() && std::fs::remove_file(test_path).is_ok()
}

async fn check_ipfs() -> bool {
    let client = reqwest::Client::new();
    client
        .post("http://localhost:5001/api/v0/version")
        .send()
        .await
        .is_ok()
}

async fn check_ffmpeg() -> bool {
    std::process::Command::new("ffmpeg")
        .arg("-version")
        .output()
        .is_ok()
}
