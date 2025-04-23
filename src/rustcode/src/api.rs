use actix_web::{post, web, HttpResponse};
   use log::info;
   use serde::Deserialize;
   use std::sync::Arc;
   use tokio::sync::Mutex;
   use webrtc::peer_connection::RTCPeerConnection;
   use crate::webrtc::start_webrtc_session;

   #[derive(Deserialize)]
   pub struct CallRequest {
       pub user_id: String,
       pub target_id: String,
       pub call_id: String,
   }

   #[derive(Deserialize)]
   pub struct EndCallRequest {
       pub call_id: String,
   }

   #[post("/call/initiate")]
   pub async fn initiate_call(
       req: web::Json<CallRequest>,
       peer_connection: web::Data<Arc<Mutex<Option<Arc<RTCPeerConnection>>>>>,
   ) -> HttpResponse {
       info!(
           "Initiating video call: user_id={}, target_id={}, call_id={}",
           req.user_id, req.target_id, req.call_id
       );

       match start_webrtc_session(&req.user_id, &req.target_id, &req.call_id).await {
           Ok(peer_conn) => {
               let mut pc = peer_connection.lock().await;
               *pc = Some(Arc::new(peer_conn));
               HttpResponse::Ok().json(serde_json::json!({
                   "status": "initiated",
                   "call_id": req.call_id,
                   "call_link": format!("ws://localhost:2020/ws/signaling/{}", req.call_id)
               }))
           }
           Err(e) => {
               log::error!("Failed to start WebRTC session: {}", e);
               HttpResponse::InternalServerError().json(serde_json::json!({
                   "error": format!("Failed to initiate call: {}", e)
               }))
           }
       }
   }

   #[post("/call/accept")]
   pub async fn accept_call(
       req: web::Json<EndCallRequest>,
       peer_connection: web::Data<Arc<Mutex<Option<Arc<RTCPeerConnection>>>>>,
   ) -> HttpResponse {
       info!("Accepting video call: call_id={}", req.call_id);

       let pc = peer_connection.lock().await;
       if pc.is_none() {
           log::error!("No peer connection found for call_id={}", req.call_id);
           return HttpResponse::BadRequest().json(serde_json::json!({
               "error": "No active call found"
           }));
       }

       HttpResponse::Ok().json(serde_json::json!({
           "status": "connected",
           "call_id": req.call_id
       }))
   }

   #[post("/call/end")]
   pub async fn end_call(
       req: web::Json<EndCallRequest>,
       peer_connection: web::Data<Arc<Mutex<Option<Arc<RTCPeerConnection>>>>>,
   ) -> HttpResponse {
       info!("Ending video call: call_id={}", req.call_id);

       let mut pc = peer_connection.lock().await;
       if let Some(peer_conn) = pc.take() {
           let _ = peer_conn.close().await;
       }

       HttpResponse::Ok().json(serde_json::json!({
           "status": "ended",
           "call_id": req.call_id
       }))
   }