use actix_web::{web, App, HttpServer};
use log::info;
use std::sync::Arc;
use tokio::main;
use tokio::sync::Mutex;

mod api;
mod download_manager;
mod sfu;
mod signaling;
mod webrtc;

use crate::webrtc::RTCPeerConnection;
use download_manager::{DownloadConfig, DownloadManager};

#[main]
async fn main() -> std::io::Result<()> {
    env_logger::init();
    info!("Starting rustcode service on port 2020");

    let peer_connection = Arc::new(Mutex::new(None::<Arc<RTCPeerConnection>>));
    let peer_connection_data = web::Data::new(peer_connection);

    let download_config = DownloadConfig {
        default_chunk_size: 10 * 1024 * 1024, // 10 MB
        default_max_connections: 8,
        max_concurrent_downloads: 5,
        retry_attempts: 3,
        retry_delay_ms: 1000,
        timeout_seconds: 300,
        max_bandwidth_bps: None,
        storage_path: std::path::PathBuf::from("/tmp/mazaryn_downloads"),
    };

    let download_manager = Arc::new(
        DownloadManager::new(download_config).expect("Failed to initialize download manager"),
    );
    let download_manager_data = web::Data::new(download_manager);

    tokio::spawn(async {
        if let Err(e) = sfu::run_sfu().await {
            log::error!("SFU error: {}", e);
        }
    });

    tokio::spawn(async {
        if let Err(e) = signaling::run_signaling_server().await {
            log::error!("Signaling error: {}", e);
        }
    });

    HttpServer::new(move || {
        App::new()
            .app_data(peer_connection_data.clone())
            .service(api::initiate_call)
            .service(api::accept_call)
            .service(api::end_call)
            .route(
                "/ws/signaling/{call_id}",
                web::get().to(signaling::signaling_ws),
            )
            .app_data(download_manager_data.clone())
            .route(
                "/downloads/start",
                web::post().to(download_manager::api::start_download),
            )
            .route(
                "/downloads/{id}",
                web::get().to(download_manager::api::get_download_status),
            )
            .route(
                "/downloads/{id}/pause",
                web::post().to(download_manager::api::pause_download),
            )
            .route(
                "/downloads/{id}/resume",
                web::post().to(download_manager::api::resume_download),
            )
            .route(
                "/downloads/{id}/cancel",
                web::post().to(download_manager::api::cancel_download),
            )
            .route(
                "/downloads/{id}",
                web::delete().to(download_manager::api::delete_download),
            )
            .route(
                "/downloads/user/{user_id}",
                web::get().to(download_manager::api::list_user_downloads),
            )
    })
    .bind("0.0.0.0:2020")?
    .run()
    .await
}
