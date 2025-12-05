use super::types::StorageStats;
use super::types::*;
use super::{AnalyticsManager, MediaPlayer, MediaStorage, StreamManager, TranscodingManager};
use actix_web::{web, Error, HttpResponse, Result as ActixResult};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::sync::Arc;

pub struct MediaVideoState {
    pub player: Arc<MediaPlayer>,
    pub stream_manager: Arc<StreamManager>,
    pub storage: Arc<MediaStorage>,
    pub analytics: Arc<AnalyticsManager>,
    pub transcoding: Arc<TranscodingManager>,
}

#[derive(Debug, Deserialize)]
pub struct CreateSessionRequest {
    pub video_id: String,
    pub user_id: String,
    pub quality: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct CreateSessionResponse {
    pub session_id: String,
    pub video_id: String,
    pub quality: String,
}

pub async fn create_session(
    state: web::Data<MediaVideoState>,
    req: web::Json<CreateSessionRequest>,
) -> ActixResult<HttpResponse> {
    match state
        .player
        .create_session(
            req.video_id.clone(),
            req.user_id.clone(),
            req.quality.clone(),
        )
        .await
    {
        Ok(session) => Ok(HttpResponse::Ok().json(CreateSessionResponse {
            session_id: session.session_id,
            video_id: session.video_id,
            quality: session.quality,
        })),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn get_session(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let session_id = path.into_inner();
    match state.player.get_session(&session_id) {
        Some(session) => Ok(HttpResponse::Ok().json(session)),
        None => Ok(HttpResponse::NotFound().json(json!({
            "error": "Session not found"
        }))),
    }
}

pub async fn close_session(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let session_id = path.into_inner();
    match state.player.close_session(&session_id).await {
        Ok(stats) => {
            state.analytics.record_view(stats).await;
            Ok(HttpResponse::Ok().json(json!({"status": "ok"})))
        }
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn get_all_sessions(state: web::Data<MediaVideoState>) -> ActixResult<HttpResponse> {
    let sessions = state.player.get_all_sessions();
    Ok(HttpResponse::Ok().json(sessions))
}

#[derive(Debug, Deserialize)]
pub struct UpdatePositionRequest {
    pub session_id: String,
    pub position: f64,
}

pub async fn update_position(
    state: web::Data<MediaVideoState>,
    req: web::Json<UpdatePositionRequest>,
) -> ActixResult<HttpResponse> {
    match state
        .player
        .update_position(&req.session_id, req.position)
        .await
    {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({"status": "ok"}))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

#[derive(Debug, Deserialize)]
pub struct UpdateQualityRequest {
    pub session_id: String,
    pub quality: String,
}

pub async fn update_quality(
    state: web::Data<MediaVideoState>,
    req: web::Json<UpdateQualityRequest>,
) -> ActixResult<HttpResponse> {
    match state
        .player
        .update_quality(&req.session_id, req.quality.clone())
        .await
    {
        Ok(_) => {
            state
                .analytics
                .record_quality_change(
                    req.session_id.clone(),
                    "user_id".to_string(),
                    "old_quality".to_string(),
                    req.quality.clone(),
                )
                .await;
            Ok(HttpResponse::Ok().json(json!({"status": "ok"})))
        }
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

#[derive(Debug, Deserialize)]
pub struct SetPlaybackRateRequest {
    pub session_id: String,
    pub rate: f32,
}

pub async fn set_playback_rate(
    state: web::Data<MediaVideoState>,
    req: web::Json<SetPlaybackRateRequest>,
) -> ActixResult<HttpResponse> {
    match state
        .player
        .set_playback_rate(&req.session_id, req.rate)
        .await
    {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({
            "status": "ok",
            "session_id": req.session_id,
            "playback_rate": req.rate
        }))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

#[derive(Debug, Deserialize)]
pub struct SetVolumeRequest {
    pub session_id: String,
    pub volume: f32,
}

pub async fn set_volume(
    state: web::Data<MediaVideoState>,
    req: web::Json<SetVolumeRequest>,
) -> ActixResult<HttpResponse> {
    match state.player.set_volume(&req.session_id, req.volume).await {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({
            "status": "ok",
            "session_id": req.session_id,
            "volume": req.volume
        }))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

#[derive(Debug, Deserialize)]
pub struct CleanupSessionsRequest {
    pub timeout_seconds: u64,
}

pub async fn cleanup_sessions(
    state: web::Data<MediaVideoState>,
    req: web::Json<CleanupSessionsRequest>,
) -> ActixResult<HttpResponse> {
    state
        .player
        .cleanup_inactive_sessions(req.timeout_seconds as i64)
        .await;
    Ok(HttpResponse::Ok().json(json!({
        "status": "ok",
        "sessions_removed": 0
    })))
}

#[derive(Debug, Deserialize)]
pub struct CreateStreamRequest {
    pub video_id: String,
    pub streamer_id: String,
    pub title: String,
    pub latency_mode: String,
}

pub async fn create_stream(
    state: web::Data<MediaVideoState>,
    req: web::Json<CreateStreamRequest>,
) -> ActixResult<HttpResponse> {
    let latency_mode = match req.latency_mode.as_str() {
        "low" => LatencyMode::LowLatency,
        "ultra" => LatencyMode::UltraLow,
        _ => LatencyMode::Normal,
    };

    match state
        .stream_manager
        .create_stream(
            req.video_id.clone(),
            req.streamer_id.clone(),
            req.title.clone(),
            latency_mode,
        )
        .await
    {
        Ok(stream) => Ok(HttpResponse::Ok().json(stream)),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn get_stream(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let stream_id = path.into_inner();
    match state.stream_manager.get_stream(&stream_id) {
        Some(stream) => Ok(HttpResponse::Ok().json(stream)),
        None => Ok(HttpResponse::NotFound().json(json!({
            "error": "Stream not found"
        }))),
    }
}

pub async fn start_stream(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let stream_id = path.into_inner();
    match state.stream_manager.start_stream(&stream_id).await {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({"status": "ok"}))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn end_stream(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let stream_id = path.into_inner();
    match state.stream_manager.end_stream(&stream_id).await {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({"status": "ok"}))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn list_live_streams(state: web::Data<MediaVideoState>) -> ActixResult<HttpResponse> {
    let streams = state.stream_manager.list_live_streams();
    Ok(HttpResponse::Ok().json(streams))
}

#[derive(Debug, Deserialize)]
pub struct JoinStreamRequest {
    pub stream_id: String,
    pub user_id: String,
}

pub async fn join_stream(
    state: web::Data<MediaVideoState>,
    req: web::Json<JoinStreamRequest>,
) -> ActixResult<HttpResponse> {
    match state
        .stream_manager
        .add_viewer(&req.stream_id, req.user_id.clone())
        .await
    {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({"status": "ok"}))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn leave_stream(
    state: web::Data<MediaVideoState>,
    req: web::Json<JoinStreamRequest>,
) -> ActixResult<HttpResponse> {
    match state
        .stream_manager
        .remove_viewer(&req.stream_id, &req.user_id)
        .await
    {
        Ok(_) => Ok(HttpResponse::Ok().json(json!({"status": "ok"}))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn get_stream_stats(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let stream_id = path.into_inner();

    if let Some(stream) = state.stream_manager.get_stream(&stream_id) {
        Ok(HttpResponse::Ok().json(json!({
            "stream_id": stream.stream_id,
            "status": format!("{:?}", stream.status),
            "current_viewers": stream.current_viewers,
            "peak_viewers": stream.peak_viewers,
            "bitrate": stream.current_bitrate,
            "resolution": stream.current_resolution
        })))
    } else {
        Ok(HttpResponse::NotFound().json(json!({
            "error": "Stream not found"
        })))
    }
}

#[derive(Debug, Deserialize)]
pub struct UpdateStreamStatsRequest {
    pub stream_id: String,
    pub bitrate: Option<u32>,
    pub resolution: Option<String>,
}

pub async fn update_stream_stats(
    state: web::Data<MediaVideoState>,
    req: web::Json<UpdateStreamStatsRequest>,
) -> ActixResult<HttpResponse> {
    if let Some(bitrate) = req.bitrate {
        if let Some(resolution) = &req.resolution {
            state
                .stream_manager
                .update_stream_stats(&req.stream_id, bitrate as u64, resolution.clone())
                .await;
        }
    }

    Ok(HttpResponse::Ok().json(json!({
        "status": "ok",
        "stream_id": req.stream_id
    })))
}

pub async fn get_video_metrics(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let video_id = path.into_inner();
    match state.analytics.get_video_metrics(&video_id) {
        Some(metrics) => Ok(HttpResponse::Ok().json(metrics)),
        None => Ok(HttpResponse::NotFound().json(json!({
            "error": "Metrics not found"
        }))),
    }
}

pub async fn get_analytics_report(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let video_id = path.into_inner();
    match state.analytics.export_analytics(&video_id).await {
        Ok(report) => Ok(HttpResponse::Ok().json(report)),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

#[derive(Debug, Deserialize)]
pub struct CreateTranscodingJobRequest {
    pub video_id: String,
    pub source_cid: String,
    pub qualities: Vec<String>,
}

pub async fn create_transcoding_job(
    state: web::Data<MediaVideoState>,
    req: web::Json<CreateTranscodingJobRequest>,
) -> ActixResult<HttpResponse> {
    let target_qualities = req
        .qualities
        .iter()
        .map(|q| match q.as_str() {
            "1080p" => QualityTarget {
                name: "1080p".to_string(),
                resolution: Resolution {
                    width: 1920,
                    height: 1080,
                },
                bitrate: 5000000,
                codec: "libx264".to_string(),
            },
            "720p" => QualityTarget {
                name: "720p".to_string(),
                resolution: Resolution {
                    width: 1280,
                    height: 720,
                },
                bitrate: 2500000,
                codec: "libx264".to_string(),
            },
            "480p" => QualityTarget {
                name: "480p".to_string(),
                resolution: Resolution {
                    width: 854,
                    height: 480,
                },
                bitrate: 1000000,
                codec: "libx264".to_string(),
            },
            _ => QualityTarget {
                name: "360p".to_string(),
                resolution: Resolution {
                    width: 640,
                    height: 360,
                },
                bitrate: 500000,
                codec: "libx264".to_string(),
            },
        })
        .collect();

    match state
        .transcoding
        .create_job(
            req.video_id.clone(),
            req.source_cid.clone(),
            target_qualities,
        )
        .await
    {
        Ok(job_id) => Ok(HttpResponse::Ok().json(json!({
            "job_id": job_id
        }))),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn get_transcoding_job(
    state: web::Data<MediaVideoState>,
    path: web::Path<String>,
) -> ActixResult<HttpResponse> {
    let job_id = path.into_inner();
    match state.transcoding.get_job(&job_id) {
        Some(job) => Ok(HttpResponse::Ok().json(job)),
        None => Ok(HttpResponse::NotFound().json(json!({
            "error": "Job not found"
        }))),
    }
}

#[derive(Debug, Deserialize)]
pub struct UploadVideoRequest {
    pub video_data: String,
}

pub async fn upload_video(
    state: web::Data<MediaVideoState>,
    req: web::Json<UploadVideoRequest>,
) -> ActixResult<HttpResponse> {
    use base64::{engine::general_purpose, Engine as _};

    let video_data = match general_purpose::STANDARD.decode(&req.video_data) {
        Ok(data) => data,
        Err(e) => {
            return Ok(HttpResponse::BadRequest().json(json!({
                "error": format!("Invalid base64: {}", e)
            })))
        }
    };

    let video_id = uuid::Uuid::new_v4().to_string();
    let bytes_data = bytes::Bytes::from(video_data.clone());

    match state
        .storage
        .store_local(&video_id, bytes_data.clone())
        .await
    {
        Ok(_) => match state.storage.upload_to_ipfs(bytes_data).await {
            Ok(cid) => Ok(HttpResponse::Ok().json(json!({
                "video_id": video_id,
                "cid": cid,
                "size": video_data.len()
            }))),
            Err(e) => Ok(HttpResponse::InternalServerError().json(json!({
                "error": format!("IPFS upload failed: {}", e)
            }))),
        },
        Err(e) => Ok(HttpResponse::InternalServerError().json(json!({
            "error": format!("Storage failed: {}", e)
        }))),
    }
}

#[derive(Debug, Deserialize)]
pub struct GetVideoInfoRequest {
    pub file_path: String,
}

pub async fn get_video_info(
    state: web::Data<MediaVideoState>,
    req: web::Json<GetVideoInfoRequest>,
) -> ActixResult<HttpResponse> {
    use std::path::PathBuf;

    let path = PathBuf::from(&req.file_path);

    if !path.exists() {
        return Ok(HttpResponse::NotFound().json(json!({
            "error": "File not found"
        })));
    }

    match state.transcoding.get_video_info(path).await {
        Ok(info) => Ok(HttpResponse::Ok().json(info)),
        Err(e) => Ok(HttpResponse::BadRequest().json(json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn health_check() -> ActixResult<HttpResponse> {
    Ok(HttpResponse::Ok().json(json!({
        "status": "healthy",
        "timestamp": chrono::Utc::now().timestamp(),
        "service": "media_video"
    })))
}

pub async fn get_system_stats(state: web::Data<MediaVideoState>) -> ActixResult<HttpResponse> {
    let storage_stats = state
        .storage
        .get_storage_stats()
        .await
        .unwrap_or(StorageStats {
            total_size_bytes: 0,
            file_count: 0,
            storage_path: String::new(),
        });

    Ok(HttpResponse::Ok().json(json!({
        "storage": {
            "total_size_bytes": storage_stats.total_size_bytes,
            "file_count": storage_stats.file_count
        },
        "sessions": {
            "active": state.player.get_all_sessions().len()
        },
        "streams": {
            "live": state.stream_manager.list_live_streams().len()
        }
    })))
}
