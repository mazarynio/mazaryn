use actix_web::{post, web, HttpResponse};
use log::info;
use serde::{Deserialize, Serialize};

#[derive(Deserialize, Debug)]
pub struct CallRequest {
    pub user_id: String,
    pub target_id: String,
    pub call_id: String,
}

#[derive(Deserialize, Debug)]
pub struct EndCallRequest {
    pub call_id: String,
}

#[derive(Serialize)]
struct CallResponse {
    status: String,
    call_id: String,
    call_link: String,
}

#[derive(Serialize)]
struct SimpleResponse {
    status: String,
    call_id: String,
}

#[post("/call/initiate")]
pub async fn initiate_call(req: web::Json<CallRequest>) -> HttpResponse {
    info!("========================================");
    info!("[INITIATE CALL] Request received");
    info!("[INITIATE CALL] user_id: {}", req.user_id);
    info!("[INITIATE CALL] target_id: {}", req.target_id);
    info!("[INITIATE CALL] call_id: {}", req.call_id);
    info!("========================================");

    let call_link = format!("ws://localhost:2020/ws/signaling/{}", req.call_id);

    info!("[INITIATE CALL] Generated call link: {}", call_link);

    let response = CallResponse {
        status: "initiated".to_string(),
        call_id: req.call_id.clone(),
        call_link,
    };

    info!(
        "[INITIATE CALL] Sending response: {:?}",
        serde_json::to_string(&response)
    );
    info!("========================================");

    HttpResponse::Ok().json(response)
}

#[post("/call/accept")]
pub async fn accept_call(req: web::Json<EndCallRequest>) -> HttpResponse {
    info!("========================================");
    info!("[ACCEPT CALL] Request received");
    info!("[ACCEPT CALL] call_id: {}", req.call_id);
    info!("========================================");

    let response = SimpleResponse {
        status: "connected".to_string(),
        call_id: req.call_id.clone(),
    };

    info!(
        "[ACCEPT CALL] Sending response: {:?}",
        serde_json::to_string(&response)
    );
    info!("========================================");

    HttpResponse::Ok().json(response)
}

#[post("/call/end")]
pub async fn end_call(req: web::Json<EndCallRequest>) -> HttpResponse {
    info!("========================================");
    info!("[END CALL] Request received");
    info!("[END CALL] call_id: {}", req.call_id);
    info!("========================================");

    let response = SimpleResponse {
        status: "ended".to_string(),
        call_id: req.call_id.clone(),
    };

    info!(
        "[END CALL] Sending response: {:?}",
        serde_json::to_string(&response)
    );
    info!("========================================");

    HttpResponse::Ok().json(response)
}
