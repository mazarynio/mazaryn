use actix::prelude::*;
use actix_web::{web, HttpRequest, HttpResponse};
use actix_web_actors::ws;
use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::Mutex;

const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
const CLIENT_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Message, Clone, Serialize, Deserialize, Debug)]
#[rtype(result = "()")]
pub struct SignalingMessage {
    pub message_type: String,
    pub payload: serde_json::Value,
    pub from_user: String,
    pub to_user: Option<String>,
}

pub struct SignalingServer {
    call_id: String,
    user_id: Option<String>,
    sessions: Arc<Mutex<HashMap<String, HashMap<String, Addr<SignalingServer>>>>>,
    hb: Instant,
}

impl SignalingServer {
    fn new(
        call_id: String,
        sessions: Arc<Mutex<HashMap<String, HashMap<String, Addr<SignalingServer>>>>>,
    ) -> Self {
        info!("Creating new SignalingServer for call_id={}", call_id);
        SignalingServer {
            call_id,
            user_id: None,
            sessions,
            hb: Instant::now(),
        }
    }

    fn hb(&self, ctx: &mut ws::WebsocketContext<Self>) {
        ctx.run_interval(HEARTBEAT_INTERVAL, |act, ctx| {
            if Instant::now().duration_since(act.hb) > CLIENT_TIMEOUT {
                warn!(
                    "WebSocket client heartbeat failed, disconnecting call_id={}",
                    act.call_id
                );
                ctx.stop();
                return;
            }

            ctx.ping(b"");
        });
    }
}

impl Actor for SignalingServer {
    type Context = ws::WebsocketContext<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        info!("WebSocket connection started for call_id={}", self.call_id);
        self.hb(ctx);
    }

    fn stopped(&mut self, _ctx: &mut Self::Context) {
        info!("WebSocket connection stopped for call_id={}", self.call_id);

        if let Some(user_id) = &self.user_id {
            let call_id = self.call_id.clone();
            let user_id_clone = user_id.clone();
            let sessions = Arc::clone(&self.sessions);

            info!("Removing user {} from call {}", user_id_clone, call_id);

            actix::spawn(async move {
                let mut sessions_lock = sessions.lock().await;
                if let Some(call_sessions) = sessions_lock.get_mut(&call_id) {
                    call_sessions.remove(&user_id_clone);
                    info!(
                        "User {} removed from call {}, remaining users: {}",
                        user_id_clone,
                        call_id,
                        call_sessions.len()
                    );

                    if call_sessions.is_empty() {
                        sessions_lock.remove(&call_id);
                        info!("Call {} removed (no users remaining)", call_id);
                    }
                }
            });
        }
    }
}

impl StreamHandler<Result<ws::Message, ws::ProtocolError>> for SignalingServer {
    fn handle(&mut self, msg: Result<ws::Message, ws::ProtocolError>, ctx: &mut Self::Context) {
        match msg {
            Ok(ws::Message::Text(text)) => {
                info!(
                    "Received text message for call_id={}: {}",
                    self.call_id, text
                );

                let text_str = text.to_string();

                match serde_json::from_str::<serde_json::Value>(&text_str) {
                    Ok(json) => {
                        let message_type = json.get("type").and_then(|v| v.as_str()).unwrap_or("");
                        info!(
                            "Processing message type: {} for call_id={}",
                            message_type, self.call_id
                        );

                        match message_type {
                            "join" => {
                                if let Some(user_id) = json.get("userId").and_then(|v| v.as_str()) {
                                    self.user_id = Some(user_id.to_string());
                                    let call_id = self.call_id.clone();
                                    let user_id_str = user_id.to_string();
                                    let addr = ctx.address();
                                    let sessions = Arc::clone(&self.sessions);

                                    info!("User {} joining call {}", user_id_str, call_id);

                                    actix::spawn(async move {
                                        let mut sessions_lock = sessions.lock().await;
                                        let call_sessions = sessions_lock
                                            .entry(call_id.clone())
                                            .or_insert_with(HashMap::new);

                                        call_sessions.insert(user_id_str.clone(), addr);

                                        info!(
                                            "User {} joined call {}, total users: {}",
                                            user_id_str,
                                            call_id,
                                            call_sessions.len()
                                        );
                                    });

                                    let response = serde_json::json!({
                                        "type": "joined",
                                        "userId": user_id,
                                        "callId": self.call_id
                                    });
                                    ctx.text(response.to_string());

                                    info!(
                                        "Sent join confirmation to user {} in call {}",
                                        user_id, self.call_id
                                    );
                                }
                            }
                            "offer" => {
                                info!("Processing offer for call_id={}", self.call_id);

                                if let Some(offer) = json.get("offer") {
                                    if let Some(sdp) = offer.get("sdp").and_then(|v| v.as_str()) {
                                        info!("Offer SDP length: {} bytes", sdp.len());
                                    }
                                }

                                let from_user = json
                                    .get("userId")
                                    .and_then(|v| v.as_str())
                                    .unwrap_or("")
                                    .to_string();
                                let call_id = self.call_id.clone();
                                let sessions = Arc::clone(&self.sessions);
                                let message_clone = text_str.clone();

                                info!(
                                    "Broadcasting offer from user {} in call {}",
                                    from_user, call_id
                                );

                                actix::spawn(async move {
                                    let sessions_lock = sessions.lock().await;
                                    if let Some(call_sessions) = sessions_lock.get(&call_id) {
                                        let mut broadcast_count = 0;
                                        for (user_id, addr) in call_sessions.iter() {
                                            if user_id != &from_user {
                                                addr.do_send(BroadcastMessage {
                                                    message: message_clone.clone(),
                                                });
                                                broadcast_count += 1;
                                                info!(
                                                    "Offer broadcast to user {} in call {}",
                                                    user_id, call_id
                                                );
                                            }
                                        }
                                        info!(
                                            "Offer broadcast to {} users in call {}",
                                            broadcast_count, call_id
                                        );
                                    } else {
                                        warn!("No call session found for call_id={}", call_id);
                                    }
                                });
                            }
                            "answer" => {
                                info!("Processing answer for call_id={}", self.call_id);

                                if let Some(answer) = json.get("answer") {
                                    if let Some(sdp) = answer.get("sdp").and_then(|v| v.as_str()) {
                                        info!("Answer SDP length: {} bytes", sdp.len());
                                    }
                                }

                                let from_user = json
                                    .get("userId")
                                    .and_then(|v| v.as_str())
                                    .unwrap_or("")
                                    .to_string();
                                let call_id = self.call_id.clone();
                                let sessions = Arc::clone(&self.sessions);
                                let message_clone = text_str.clone();

                                info!(
                                    "Broadcasting answer from user {} in call {}",
                                    from_user, call_id
                                );

                                actix::spawn(async move {
                                    let sessions_lock = sessions.lock().await;
                                    if let Some(call_sessions) = sessions_lock.get(&call_id) {
                                        let mut broadcast_count = 0;
                                        for (user_id, addr) in call_sessions.iter() {
                                            if user_id != &from_user {
                                                addr.do_send(BroadcastMessage {
                                                    message: message_clone.clone(),
                                                });
                                                broadcast_count += 1;
                                                info!(
                                                    "Answer broadcast to user {} in call {}",
                                                    user_id, call_id
                                                );
                                            }
                                        }
                                        info!(
                                            "Answer broadcast to {} users in call {}",
                                            broadcast_count, call_id
                                        );
                                    } else {
                                        warn!("No call session found for call_id={}", call_id);
                                    }
                                });
                            }
                            "ice-candidate" => {
                                info!("Processing ICE candidate for call_id={}", self.call_id);

                                if let Some(candidate) = json.get("candidate") {
                                    if let Some(cand_str) =
                                        candidate.get("candidate").and_then(|v| v.as_str())
                                    {
                                        info!("ICE candidate: {}", cand_str);
                                    }
                                }

                                let from_user = json
                                    .get("userId")
                                    .and_then(|v| v.as_str())
                                    .unwrap_or("")
                                    .to_string();
                                let call_id = self.call_id.clone();
                                let sessions = Arc::clone(&self.sessions);
                                let message_clone = text_str.clone();

                                info!(
                                    "Broadcasting ICE candidate from user {} in call {}",
                                    from_user, call_id
                                );

                                actix::spawn(async move {
                                    let sessions_lock = sessions.lock().await;
                                    if let Some(call_sessions) = sessions_lock.get(&call_id) {
                                        let mut broadcast_count = 0;
                                        for (user_id, addr) in call_sessions.iter() {
                                            if user_id != &from_user {
                                                addr.do_send(BroadcastMessage {
                                                    message: message_clone.clone(),
                                                });
                                                broadcast_count += 1;
                                                info!(
                                                    "ICE candidate broadcast to user {} in call {}",
                                                    user_id, call_id
                                                );
                                            }
                                        }
                                        info!(
                                            "ICE candidate broadcast to {} users in call {}",
                                            broadcast_count, call_id
                                        );
                                    } else {
                                        warn!("No call session found for call_id={}", call_id);
                                    }
                                });
                            }
                            _ => {
                                warn!(
                                    "Unknown message type: {} for call_id={}",
                                    message_type, self.call_id
                                );
                            }
                        }
                    }
                    Err(e) => {
                        error!("Failed to parse JSON for call_id={}: {}", self.call_id, e);
                        error!("Raw message: {}", text_str);
                    }
                }
            }
            Ok(ws::Message::Binary(bin)) => {
                info!(
                    "Received binary message of {} bytes for call_id={}",
                    bin.len(),
                    self.call_id
                );
            }
            Ok(ws::Message::Ping(msg)) => {
                self.hb = Instant::now();
                ctx.pong(&msg);
            }
            Ok(ws::Message::Pong(_)) => {
                self.hb = Instant::now();
            }
            Ok(ws::Message::Close(reason)) => {
                info!(
                    "WebSocket close message received for call_id={}: {:?}",
                    self.call_id, reason
                );
                ctx.stop();
            }
            Err(e) => {
                error!(
                    "WebSocket protocol error for call_id={}: {}",
                    self.call_id, e
                );
                ctx.stop();
            }
            _ => {
                warn!(
                    "Unhandled WebSocket message type for call_id={}",
                    self.call_id
                );
            }
        }
    }
}

#[derive(Message)]
#[rtype(result = "()")]
struct BroadcastMessage {
    message: String,
}

impl Handler<BroadcastMessage> for SignalingServer {
    type Result = ();

    fn handle(&mut self, msg: BroadcastMessage, ctx: &mut Self::Context) {
        ctx.text(msg.message);
    }
}

pub async fn signaling_ws(
    call_id: web::Path<String>,
    req: HttpRequest,
    stream: web::Payload,
    sessions: web::Data<Arc<Mutex<HashMap<String, HashMap<String, Addr<SignalingServer>>>>>>,
) -> Result<HttpResponse, actix_web::Error> {
    let call_id_str = call_id.into_inner();
    info!("WebSocket connection request for call_id={}", call_id_str);
    info!("Connection from: {:?}", req.connection_info());

    let server = SignalingServer::new(call_id_str, Arc::clone(&sessions.get_ref()));

    let resp = ws::start(server, &req, stream);

    match &resp {
        Ok(_) => info!("WebSocket handshake successful"),
        Err(e) => error!("WebSocket handshake failed: {}", e),
    }

    resp
}

pub async fn run_signaling_server() -> Result<(), Box<dyn std::error::Error>> {
    info!("Signaling server initialized and running");
    Ok(())
}
