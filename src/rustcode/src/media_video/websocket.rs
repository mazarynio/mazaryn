use actix::{Actor, ActorContext, AsyncContext, Handler, Message, StreamHandler};
use actix_web::{web, Error, HttpRequest, HttpResponse};
use actix_web_actors::ws;
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, Instant};

const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
const CLIENT_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum WsMessage {
    #[serde(rename = "session_update")]
    SessionUpdate {
        session_id: String,
        position: f64,
        buffering: bool,
    },
    #[serde(rename = "stream_update")]
    StreamUpdate {
        stream_id: String,
        current_viewers: u32,
        status: String,
    },
    #[serde(rename = "transcoding_update")]
    TranscodingUpdate {
        job_id: String,
        progress: f32,
        status: String,
    },
    #[serde(rename = "quality_change")]
    QualityChange {
        session_id: String,
        new_quality: String,
    },
    #[serde(rename = "viewer_joined")]
    ViewerJoined {
        stream_id: String,
        viewer_count: u32,
    },
    #[serde(rename = "viewer_left")]
    ViewerLeft {
        stream_id: String,
        viewer_count: u32,
    },
    #[serde(rename = "ping")]
    Ping,
    #[serde(rename = "pong")]
    Pong,
}

#[derive(Message)]
#[rtype(result = "()")]
pub struct BroadcastMessage(pub WsMessage);

pub struct MediaWebSocket {
    hb: Instant,
    client_id: String,
    subscriptions: Vec<String>,
}

impl MediaWebSocket {
    pub fn new(client_id: String) -> Self {
        Self {
            hb: Instant::now(),
            client_id,
            subscriptions: Vec::new(),
        }
    }

    fn hb(&self, ctx: &mut ws::WebsocketContext<Self>) {
        ctx.run_interval(HEARTBEAT_INTERVAL, |act, ctx| {
            if Instant::now().duration_since(act.hb) > CLIENT_TIMEOUT {
                println!("WebSocket Client heartbeat failed, disconnecting!");
                ctx.stop();
                return;
            }

            ctx.ping(b"");
        });
    }
}

impl Actor for MediaWebSocket {
    type Context = ws::WebsocketContext<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        self.hb(ctx);
        println!(
            "WebSocket connection started for client: {}",
            self.client_id
        );
    }

    fn stopped(&mut self, _ctx: &mut Self::Context) {
        println!(
            "WebSocket connection stopped for client: {}",
            self.client_id
        );
    }
}

impl StreamHandler<Result<ws::Message, ws::ProtocolError>> for MediaWebSocket {
    fn handle(&mut self, msg: Result<ws::Message, ws::ProtocolError>, ctx: &mut Self::Context) {
        match msg {
            Ok(ws::Message::Ping(msg)) => {
                self.hb = Instant::now();
                ctx.pong(&msg);
            }
            Ok(ws::Message::Pong(_)) => {
                self.hb = Instant::now();
            }
            Ok(ws::Message::Text(text)) => {
                let text_str = text.to_string();
                if let Ok(ws_msg) = serde_json::from_str::<WsMessage>(&text_str) {
                    match ws_msg {
                        WsMessage::Ping => {
                            let pong = WsMessage::Pong;
                            if let Ok(json) = serde_json::to_string(&pong) {
                                ctx.text(json);
                            }
                        }
                        _ => {
                            println!("Received message: {:?}", ws_msg);
                        }
                    }
                }
            }
            Ok(ws::Message::Binary(_)) => {
                println!("Unexpected binary message");
            }
            Ok(ws::Message::Close(reason)) => {
                ctx.close(reason);
                ctx.stop();
            }
            _ => ctx.stop(),
        }
    }
}

impl Handler<BroadcastMessage> for MediaWebSocket {
    type Result = ();

    fn handle(&mut self, msg: BroadcastMessage, ctx: &mut Self::Context) {
        if let Ok(json) = serde_json::to_string(&msg.0) {
            ctx.text(json);
        }
    }
}

pub async fn ws_index(
    req: HttpRequest,
    stream: web::Payload,
    query: web::Query<WsQuery>,
) -> Result<HttpResponse, Error> {
    let client_id = query.client_id.clone();
    let ws = MediaWebSocket::new(client_id);
    ws::start(ws, &req, stream)
}

#[derive(Deserialize)]
pub struct WsQuery {
    client_id: String,
}

pub struct WsManager {
    connections: Arc<DashMap<String, Vec<actix::Addr<MediaWebSocket>>>>,
}

impl WsManager {
    pub fn new() -> Self {
        Self {
            connections: Arc::new(DashMap::new()),
        }
    }

    pub fn subscribe(&self, video_id: String, addr: actix::Addr<MediaWebSocket>) {
        self.connections
            .entry(video_id)
            .or_insert_with(Vec::new)
            .push(addr);
    }

    pub fn broadcast(&self, video_id: &str, message: WsMessage) {
        if let Some(addrs) = self.connections.get(video_id) {
            for addr in addrs.value() {
                addr.do_send(BroadcastMessage(message.clone()));
            }
        }
    }
}
