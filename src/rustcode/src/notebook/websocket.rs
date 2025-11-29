use actix::{Actor, ActorContext, AsyncContext, Handler, Message, StreamHandler};
use actix_web::{web, Error, HttpRequest, HttpResponse};
use actix_web_actors::ws;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::{Duration, Instant};

const HEARTBEAT_INTERVAL: Duration = Duration::from_secs(5);
const CLIENT_TIMEOUT: Duration = Duration::from_secs(10);

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ClientMessage {
    ExecuteCode { cell_id: String, code: String },
    Interrupt,
    Ping,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum ServerMessage {
    ExecutionStarted {
        cell_id: String,
        execution_id: String,
    },
    Output {
        cell_id: String,
        execution_id: String,
        output: serde_json::Value,
    },
    ExecutionComplete {
        cell_id: String,
        execution_id: String,
        status: String,
        execution_time_ms: u128,
    },
    Error {
        cell_id: String,
        error: String,
    },
    Pong,
}

pub struct NotebookWebSocket {
    session_id: String,
    kernel_manager: Arc<super::KernelManager>,
    hb: Instant,
}

impl NotebookWebSocket {
    pub fn new(session_id: String, kernel_manager: Arc<super::KernelManager>) -> Self {
        Self {
            session_id,
            kernel_manager,
            hb: Instant::now(),
        }
    }

    fn hb(&self, ctx: &mut ws::WebsocketContext<Self>) {
        ctx.run_interval(HEARTBEAT_INTERVAL, |act, ctx| {
            if Instant::now().duration_since(act.hb) > CLIENT_TIMEOUT {
                ctx.stop();
                return;
            }
            ctx.ping(b"");
        });
    }
}

impl Actor for NotebookWebSocket {
    type Context = ws::WebsocketContext<Self>;

    fn started(&mut self, ctx: &mut Self::Context) {
        self.hb(ctx);
    }
}

impl StreamHandler<Result<ws::Message, ws::ProtocolError>> for NotebookWebSocket {
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
                self.hb = Instant::now();

                match serde_json::from_str::<ClientMessage>(&text) {
                    Ok(ClientMessage::ExecuteCode { cell_id, code }) => {
                        let session_id = self.session_id.clone();
                        let kernel_manager = self.kernel_manager.clone();
                        let addr = ctx.address();

                        actix::spawn(async move {
                            if let Some(session) = kernel_manager.get_session(&session_id) {
                                let execution_id = uuid::Uuid::new_v4().to_string();

                                let _ =
                                    addr.do_send(SendMessage(ServerMessage::ExecutionStarted {
                                        cell_id: cell_id.clone(),
                                        execution_id: execution_id.clone(),
                                    }));

                                match kernel_manager
                                    .execute_code(
                                        &session.kernel_id,
                                        code,
                                        session.notebook_id.clone(),
                                        cell_id.clone(),
                                    )
                                    .await
                                {
                                    Ok(result) => {
                                        for output in &result.outputs {
                                            let _ =
                                                addr.do_send(SendMessage(ServerMessage::Output {
                                                    cell_id: cell_id.clone(),
                                                    execution_id: execution_id.clone(),
                                                    output: serde_json::to_value(output)
                                                        .unwrap_or(serde_json::Value::Null),
                                                }));
                                        }

                                        let _ = addr.do_send(SendMessage(
                                            ServerMessage::ExecutionComplete {
                                                cell_id: cell_id.clone(),
                                                execution_id: execution_id.clone(),
                                                status: format!("{:?}", result.status),
                                                execution_time_ms: result.execution_time_ms,
                                            },
                                        ));
                                    }
                                    Err(e) => {
                                        let _ = addr.do_send(SendMessage(ServerMessage::Error {
                                            cell_id: cell_id.clone(),
                                            error: e.to_string(),
                                        }));
                                    }
                                }
                            }
                        });
                    }
                    Ok(ClientMessage::Ping) => {
                        ctx.text(serde_json::to_string(&ServerMessage::Pong).unwrap());
                    }
                    Ok(ClientMessage::Interrupt) => {}
                    Err(e) => {
                        log::error!("Failed to parse client message: {}", e);
                    }
                }
            }
            Ok(ws::Message::Binary(_)) => {}
            Ok(ws::Message::Close(reason)) => {
                ctx.close(reason);
                ctx.stop();
            }
            _ => {}
        }
    }
}

#[derive(Message)]
#[rtype(result = "()")]
struct SendMessage(ServerMessage);

impl Handler<SendMessage> for NotebookWebSocket {
    type Result = ();

    fn handle(&mut self, msg: SendMessage, ctx: &mut Self::Context) {
        if let Ok(text) = serde_json::to_string(&msg.0) {
            ctx.text(text);
        }
    }
}

pub async fn notebook_ws(
    req: HttpRequest,
    stream: web::Payload,
    session_id: web::Path<String>,
    kernel_manager: web::Data<Arc<super::KernelManager>>,
) -> Result<HttpResponse, Error> {
    let session_id = session_id.into_inner();

    if kernel_manager.get_session(&session_id).is_none() {
        return Ok(HttpResponse::NotFound().json(serde_json::json!({
            "error": "Session not found"
        })));
    }

    let ws = NotebookWebSocket::new(session_id, kernel_manager.get_ref().clone());

    ws::start(ws, &req, stream)
}
