use actix::prelude::*;
use actix_web::{web, HttpResponse};
use actix_web_actors::ws;
use log::info;
use std::sync::Arc;
use tokio::sync::Mutex;
use webrtc::peer_connection::RTCPeerConnection;

pub async fn signaling_ws(
    call_id: web::Path<String>,
    req: actix_web::HttpRequest,
    stream: web::Payload,
    peer_connection: web::Data<Arc<Mutex<Option<Arc<RTCPeerConnection>>>>>,
) -> Result<HttpResponse, actix_web::Error> {
    info!("WebSocket connection for call_id={}", call_id);
    ws::start(
        SignalingServer {
            call_id: call_id.into_inner(),
            peer_connection: Arc::clone(&peer_connection),
        },
        &req,
        stream,
    )
}

struct SignalingServer {
    call_id: String,
    peer_connection: Arc<Mutex<Option<Arc<RTCPeerConnection>>>>,
}

impl Actor for SignalingServer {
    type Context = ws::WebsocketContext<Self>;
}

impl StreamHandler<Result<ws::Message, ws::ProtocolError>> for SignalingServer {
    fn handle(&mut self, msg: Result<ws::Message, ws::ProtocolError>, ctx: &mut Self::Context) {
        match msg {
            Ok(ws::Message::Text(text)) => {
                info!("Received WebSocket message for call_id={}: {}", self.call_id, text);
                ctx.text(text); 
            }
            Ok(ws::Message::Close(reason)) => {
                info!("WebSocket closed for call_id={}: {:?}", self.call_id, reason);
                ctx.close(reason);
            }
            _ => {}
        }
    }
}

pub async fn run_signaling_server() -> Result<(), Box<dyn std::error::Error>> {
    info!("Signaling server running");
    Ok(())
}