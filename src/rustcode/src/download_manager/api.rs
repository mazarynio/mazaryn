use crate::download_manager::types::DownloadStatus;
use crate::download_manager::{DownloadManager, DownloadRequest};
use actix_files::NamedFile;
use actix_web::http::header::{ContentDisposition, DispositionParam, DispositionType};
use actix_web::HttpRequest;
use actix_web::{web, HttpResponse, Responder};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use uuid::Uuid;

#[derive(Deserialize)]
pub struct StartDownloadRequest {
    pub url: Option<String>,
    pub destination: String,
    pub dataset_id: Option<String>,
    pub competition_id: Option<String>,
    pub user_id: String,
    pub checksum: Option<String>,
    pub priority: Option<String>,
    pub chunk_size_mb: Option<usize>,
    pub max_connections: Option<usize>,
    pub expected_size: Option<u64>,
    pub is_erlang_binary: Option<bool>,
    pub erlang_binary_id: Option<String>,
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
    log::info!("Received download request");
    log::info!("Destination: {}", req.destination);
    log::info!("User ID: {}", req.user_id);
    log::info!("Is Erlang Binary: {:?}", req.is_erlang_binary);

    let is_erlang = req.is_erlang_binary.unwrap_or(false);
    let url = if is_erlang {
        req.erlang_binary_id.clone().unwrap_or_default()
    } else {
        req.url.clone().unwrap_or_default()
    };

    if url.is_empty() {
        return HttpResponse::BadRequest().json(ErrorResponse {
            error: "URL or erlang_binary_id is required".to_string(),
        });
    }

    log::info!("Download URL/ID: {}", url);

    let priority = match req.priority.as_deref() {
        Some("low") => crate::download_manager::types::DownloadPriority::Low,
        Some("high") => crate::download_manager::types::DownloadPriority::High,
        Some("critical") => crate::download_manager::types::DownloadPriority::Critical,
        _ => crate::download_manager::types::DownloadPriority::Normal,
    };

    let download_request = DownloadRequest {
        url: url.clone(),
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
        expected_size: req.expected_size,
        is_erlang_binary: Some(is_erlang),
        erlang_binary_id: req.erlang_binary_id.clone(),
    };

    log::info!("Starting download with manager...");
    match manager.start_download(download_request).await {
        Ok(id) => {
            log::info!("Download started successfully with ID: {}", id);
            HttpResponse::Ok().json(StartDownloadResponse {
                download_id: id.to_string(),
                status: "queued".to_string(),
            })
        }
        Err(e) => {
            log::error!("Failed to start download: {}", e);
            HttpResponse::InternalServerError().json(ErrorResponse {
                error: e.to_string(),
            })
        }
    }
}

pub async fn get_download_status(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
) -> impl Responder {
    let id = match Uuid::parse_str(&path.into_inner()) {
        Ok(id) => id,
        Err(e) => {
            log::error!("Invalid UUID: {}", e);
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: format!("Invalid UUID: {}", e),
            });
        }
    };

    log::info!("Getting download status for ID: {}", id);
    match manager.get_download_info(&id).await {
        Ok(info) => {
            log::info!("Download status: {:?}", info.status);
            HttpResponse::Ok().json(info)
        }
        Err(e) => {
            log::error!("Failed to get download status: {}", e);
            HttpResponse::NotFound().json(ErrorResponse {
                error: e.to_string(),
            })
        }
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

pub async fn download_file(
    manager: web::Data<Arc<DownloadManager>>,
    path: web::Path<String>,
    _req: HttpRequest,
) -> impl Responder {
    let id = match Uuid::parse_str(&path.into_inner()) {
        Ok(id) => id,
        Err(e) => {
            log::error!("Invalid UUID for file download: {}", e);
            return HttpResponse::BadRequest().json(ErrorResponse {
                error: format!("Invalid UUID: {}", e),
            });
        }
    };

    log::info!("Attempting to download file for ID: {}", id);
    match manager.get_download_info(&id).await {
        Ok(info) => {
            log::info!("Download info retrieved, status: {:?}", info.status);
            if info.status != DownloadStatus::Completed {
                log::warn!("Download not completed yet for ID: {}", id);
                return HttpResponse::BadRequest().json(ErrorResponse {
                    error: "Download not completed yet".to_string(),
                });
            }

            log::info!("Opening file: {:?}", info.destination);
            match NamedFile::open(&info.destination) {
                Ok(file) => {
                    let filename = info
                        .destination
                        .file_name()
                        .and_then(|n| n.to_str())
                        .unwrap_or("download.dat");

                    log::info!("Serving file: {}", filename);
                    file.set_content_disposition(actix_web::http::header::ContentDisposition {
                        disposition: actix_web::http::header::DispositionType::Attachment,
                        parameters: vec![actix_web::http::header::DispositionParam::Filename(
                            filename.to_string(),
                        )],
                    })
                    .into_response(&_req)
                }
                Err(e) => {
                    log::error!("Failed to open file: {}", e);
                    HttpResponse::InternalServerError().json(ErrorResponse {
                        error: format!("Failed to open file: {}", e),
                    })
                }
            }
        }
        Err(e) => {
            log::error!("Failed to get download info: {}", e);
            HttpResponse::NotFound().json(ErrorResponse {
                error: e.to_string(),
            })
        }
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
    log::info!("Listing downloads for user: {}", user_id);

    match manager.get_user_downloads(&user_id).await {
        Ok(downloads) => {
            log::info!("Found {} downloads for user {}", downloads.len(), user_id);
            HttpResponse::Ok().json(downloads)
        }
        Err(e) => {
            log::error!("Failed to list user downloads: {}", e);
            HttpResponse::InternalServerError().json(ErrorResponse {
                error: e.to_string(),
            })
        }
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

pub async fn get_download_file(
    manager: web::Data<Arc<DownloadManager>>,
    id: web::Path<String>,
) -> Result<impl Responder, actix_web::Error> {
    let download_id = Uuid::parse_str(&id)
        .map_err(|e| actix_web::error::ErrorBadRequest(format!("Invalid UUID: {}", e)))?;

    let info = manager
        .get_download_info(&download_id)
        .await
        .map_err(|e| actix_web::error::ErrorNotFound(format!("Download not found: {}", e)))?;

    if info.status != DownloadStatus::Completed {
        return Err(actix_web::error::ErrorBadRequest(
            "Download not completed yet",
        ));
    }

    let file = NamedFile::open(&info.destination)
        .map_err(|e| actix_web::error::ErrorNotFound(format!("File not found: {}", e)))?;

    let filename = info
        .destination
        .file_name()
        .and_then(|n| n.to_str())
        .unwrap_or("download")
        .to_string();

    Ok(file
        .use_last_modified(true)
        .set_content_disposition(ContentDisposition {
            disposition: DispositionType::Attachment,
            parameters: vec![DispositionParam::Filename(filename)],
        }))
}
