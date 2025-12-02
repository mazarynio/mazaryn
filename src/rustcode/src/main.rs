use actix_cors::Cors;
use actix_web::{middleware, web, App, HttpServer};
use log::info;
use std::sync::Arc;
use tokio::main;
use tokio::sync::Mutex;

mod api;
mod download_manager;
mod media_video;
mod notebook;
mod sfu;
mod signaling;
mod webrtc;

use crate::webrtc::RTCPeerConnection;
use download_manager::{DownloadConfig, DownloadManager};
use media_video::{
    api::MediaVideoState, AnalyticsManager, MediaPlayer, MediaPlayerConfig, MediaStorage,
    StreamManager, TranscodingManager,
};
use notebook::{types::KernelConfig, KernelManager};

#[main]
async fn main() -> std::io::Result<()> {
    env_logger::Builder::from_default_env()
        .filter_level(log::LevelFilter::Info)
        .init();

    info!("Starting rustcode service on port 2020");

    let peer_connection = Arc::new(Mutex::new(None::<Arc<RTCPeerConnection>>));
    let peer_connection_data = web::Data::new(peer_connection);

    let storage_path = std::path::PathBuf::from("/tmp/mazaryn_downloads");
    std::fs::create_dir_all(&storage_path).expect("Failed to create storage directory");

    let download_config = DownloadConfig {
        default_chunk_size: 10 * 1024 * 1024,
        default_max_connections: 8,
        max_concurrent_downloads: 5,
        retry_attempts: 3,
        retry_delay_ms: 1000,
        timeout_seconds: 300,
        max_bandwidth_bps: None,
        storage_path,
        erlang_http_endpoint: String::from("http://localhost:4000"),
    };

    let download_manager = Arc::new(
        DownloadManager::new(download_config).expect("Failed to initialize download manager"),
    );
    let download_manager_data = web::Data::new(download_manager);

    let workspace_path = std::path::PathBuf::from("/tmp/mazaryn_notebooks");
    std::fs::create_dir_all(&workspace_path).expect("Failed to create workspace directory");

    let kernel_config = KernelConfig {
        max_execution_time: 300,
        max_memory_mb: 4096,
        enable_gpu: false,
        python_packages: vec![],
        r_packages: vec![],
        workspace_dir: workspace_path,
    };

    let kernel_manager =
        Arc::new(KernelManager::new(kernel_config).expect("Failed to initialize kernel manager"));
    let kernel_manager_data = web::Data::new(kernel_manager);

    info!("Kernel manager initialized successfully");

    let video_storage_path = std::path::PathBuf::from("/tmp/mazaryn_videos");
    std::fs::create_dir_all(&video_storage_path).expect("Failed to create video storage directory");

    let media_player_config = MediaPlayerConfig::default();
    let media_player = Arc::new(MediaPlayer::new(media_player_config));

    let stream_manager = Arc::new(StreamManager::new());

    let media_storage = Arc::new(MediaStorage::new(
        video_storage_path.clone(),
        "https://ipfs.io".to_string(),
        "http://localhost:5001".to_string(),
    ));

    let analytics_manager = Arc::new(AnalyticsManager::new());

    let transcoding_manager = Arc::new(TranscodingManager::new(
        "ffmpeg".to_string(),
        video_storage_path.join("temp"),
    ));

    let media_video_state = web::Data::new(MediaVideoState {
        player: media_player,
        stream_manager,
        storage: media_storage,
        analytics: analytics_manager,
        transcoding: transcoding_manager,
    });

    info!("Media video system initialized successfully");

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
        let cors = Cors::permissive();

        App::new()
            .wrap(cors)
            .wrap(middleware::Logger::default())
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
            .route(
                "/downloads/{id}/file",
                web::get().to(download_manager::api::download_file),
            )
            .app_data(kernel_manager_data.clone())
            .route(
                "/notebooks/sessions",
                web::post().to(notebook::api::create_session),
            )
            .route(
                "/notebooks/execute",
                web::post().to(notebook::api::execute_code),
            )
            .route(
                "/notebooks/sessions/{session_id}",
                web::get().to(notebook::api::get_session_status),
            )
            .route(
                "/notebooks/sessions/{session_id}",
                web::delete().to(notebook::api::close_session),
            )
            .route(
                "/ws/notebooks/{session_id}",
                web::get().to(notebook::websocket::notebook_ws),
            )
            .app_data(media_video_state.clone())
            .route(
                "/media/sessions",
                web::post().to(media_video::api::create_session),
            )
            .route(
                "/media/sessions/active",
                web::get().to(media_video::api::get_all_sessions),
            )
            .route(
                "/media/sessions/{session_id}",
                web::get().to(media_video::api::get_session),
            )
            .route(
                "/media/sessions/{session_id}",
                web::delete().to(media_video::api::close_session),
            )
            .route(
                "/media/sessions/position",
                web::post().to(media_video::api::update_position),
            )
            .route(
                "/media/sessions/quality",
                web::post().to(media_video::api::update_quality),
            )
            .route(
                "/media/sessions/rate",
                web::post().to(media_video::api::set_playback_rate),
            )
            .route(
                "/media/sessions/volume",
                web::post().to(media_video::api::set_volume),
            )
            .route(
                "/media/sessions/cleanup",
                web::post().to(media_video::api::cleanup_sessions),
            )
            .route(
                "/media/streams",
                web::post().to(media_video::api::create_stream),
            )
            .route(
                "/media/streams/live",
                web::get().to(media_video::api::list_live_streams),
            )
            .route(
                "/media/streams/{stream_id}",
                web::get().to(media_video::api::get_stream),
            )
            .route(
                "/media/streams/{stream_id}/start",
                web::post().to(media_video::api::start_stream),
            )
            .route(
                "/media/streams/{stream_id}/end",
                web::post().to(media_video::api::end_stream),
            )
            .route(
                "/media/streams/{stream_id}/stats",
                web::get().to(media_video::api::get_stream_stats),
            )
            .route(
                "/media/streams/join",
                web::post().to(media_video::api::join_stream),
            )
            .route(
                "/media/streams/leave",
                web::post().to(media_video::api::leave_stream),
            )
            .route(
                "/media/streams/stats",
                web::post().to(media_video::api::update_stream_stats),
            )
            .route(
                "/media/analytics/{video_id}",
                web::get().to(media_video::api::get_video_metrics),
            )
            .route(
                "/media/analytics/{video_id}/report",
                web::get().to(media_video::api::get_analytics_report),
            )
            .route(
                "/media/transcoding",
                web::post().to(media_video::api::create_transcoding_job),
            )
            .route(
                "/media/transcoding/{job_id}",
                web::get().to(media_video::api::get_transcoding_job),
            )
            .route(
                "/media/upload",
                web::post().to(media_video::api::upload_video),
            )
            .route(
                "/media/info",
                web::post().to(media_video::api::get_video_info),
            )
            .route(
                "/media/health",
                web::get().to(media_video::api::health_check),
            )
            .route(
                "/media/stats",
                web::get().to(media_video::api::get_system_stats),
            )
    })
    .bind("0.0.0.0:2020")?
    .run()
    .await
}
