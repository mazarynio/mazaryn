use crate::download_manager::{DownloadManager, DownloadRequest};
use actix_web::{web, HttpResponse, Responder};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

#[derive(Deserialize)]
pub struct StartDownloadRequest {
    pub url: String,
    pub destination: String,
    pub dataset_id: Option<String>,
    pub competition_id: Option<String>,
    pub user_id: String,
    pub checksum: Option<String>,
    pub priority: Option<String>,
    pub chunk_size_mb: Option<usize>,
    pub max_connections: Option<usize>,
}

#[derive(Serialize)]
pub struct StartDownloadResponse {
    pub download_id: String,
    pub status: String,
}

#[derive(Serialize)]
pub struct ErrorResponse {
    pub error: String,
}

pub async fn start_download(
    manager: web::Data<Arc<DownloadManager>>,
    req: web::Json<StartDownloadRequest>,
) -> impl Responder {
    let priority = match req.priority.as_deref() {
        Some("low") => crate::download_manager::types::DownloadPriority::Low,
        Some("high") => crate::download_manager::types::DownloadPriority::High,
        Some("critical") => crate::download_manager::types::DownloadPriority::Critical,
        _ => crate::download_manager::types::DownloadPriority::Normal,
    };

    let download_request = DownloadRequest {
        url: req.url.clone(),
        destination: std::path::PathBuf::from(&req.destination),
        dataset_id: req.dataset_id.clone(),
        competition_id: req.competition_id.clone(),
        user_id: req.user_id.clone(),
        checksum: req.checksum.clone(),
        checksum_algorithm: Some(crate::download_manager::types::ChecksumAlgorithm::SHA256),
        priority,
        chunk_size: req.chunk_size_mb.map(|mb| mb * 1024 * 1024),
        max_connections: req.max_connections,
        headers: None,
    };

    match manager.start_download(download_request).await {
        Ok(id) => HttpResponse::Ok().json(StartDownloadResponse {
            download_id: id.to_string(),
            status: "queued".to_string(),
        }),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: e.to_string(),
        }),
    }
}

pub async fn get_download_status(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
) -> impl Responder {
    let id = match Uuid::parse_str(&path.into_inner()) {
        Ok(id) => id,
        Err(e) => {
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: format!("Invalid UUID: {}", e),
            })
        }
    };

    match manager.get_download_info(&id).await {
        Ok(info) => HttpResponse::Ok().json(info),
        Err(e) => HttpResponse::NotFound().json(ErrorResponse {
            error: e.to_string(),
        }),
    }
}

pub async fn pause_download(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
) -> impl Responder {
    let id = match Uuid::parse_str(&path.into_inner()) {
        Ok(id) => id,
        Err(e) => {
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: format!("Invalid UUID: {}", e),
            })
        }
    };

    match manager.pause_download(&id).await {
        Ok(_) => HttpResponse::Ok().json(serde_json::json!({"status": "paused"})),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: e.to_string(),
        }),
    }
}

pub async fn resume_download(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
) -> impl Responder {
    let id = match Uuid::parse_str(&path.into_inner()) {
        Ok(id) => id,
        Err(e) => {
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: format!("Invalid UUID: {}", e),
            })
        }
    };

    match manager.resume_download(&id).await {
        Ok(_) => HttpResponse::Ok().json(serde_json::json!({"status": "resumed"})),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: e.to_string(),
        }),
    }
}

pub async fn cancel_download(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
) -> impl Responder {
    let id = match Uuid::parse_str(&path.into_inner()) {
        Ok(id) => id,
        Err(e) => {
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: format!("Invalid UUID: {}", e),
            })
        }
    };

    match manager.cancel_download(&id).await {
        Ok(_) => HttpResponse::Ok().json(serde_json::json!({"status": "cancelled"})),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: e.to_string(),
        }),
    }
}

pub async fn list_user_downloads(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
) -> impl Responder {
    let user_id = path.into_inner();

    match manager.get_user_downloads(&user_id).await {
        Ok(downloads) => HttpResponse::Ok().json(downloads),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: e.to_string(),
        }),
    }
}

pub async fn delete_download(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
) -> impl Responder {
    let id = match Uuid::parse_str(&path.into_inner()) {
        Ok(id) => id,
        Err(e) => {
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: format!("Invalid UUID: {}", e),
            })
        }
    };

    match manager.delete_download(&id).await {
        Ok(_) => HttpResponse::Ok().json(serde_json::json!({"status": "deleted"})),
        Err(e) => HttpResponse::InternalServerError().json(ErrorResponse {
            error: e.to_string(),
        }),
    }
}
