use actix_web::{App, HttpServer};
use log::info;
use std::sync::Arc;
use tokio::main;
use tokio::sync::Mutex;
use crate::webrtc::RTCPeerConnection; 

mod api;
mod webrtc;
mod signaling;
mod sfu;

#[main]
async fn main() -> std::io::Result<()> {
    env_logger::init();
    info!("Starting rustcode video chat service on port 2020");

    let peer_connection = Arc::new(Mutex::new(None::<Arc<RTCPeerConnection>>));
    let peer_connection_data = actix_web::web::Data::new(peer_connection);

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
            .route("/ws/signaling/{call_id}", actix_web::web::get().to(signaling::signaling_ws))
    })
    .bind("0.0.0.0:2020")?
    .run()
    .await
}