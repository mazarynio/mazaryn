use log::info;
use std::sync::Arc;
use webrtc::api::media_engine::MediaEngine;
use webrtc::api::APIBuilder;
use webrtc::ice_transport::ice_server::RTCIceServer;
use webrtc::peer_connection::configuration::RTCConfiguration;
pub use webrtc::peer_connection::RTCPeerConnection; 
use webrtc::rtp_transceiver::rtp_codec::{RTCRtpCodecCapability, RTCRtpCodecParameters, RTPCodecType};
use webrtc::track::track_local::TrackLocal;
use webrtc::track::track_local::track_local_static_rtp::TrackLocalStaticRTP;

pub async fn start_webrtc_session(
    user_id: &str,
    target_id: &str,
    call_id: &str,
) -> Result<RTCPeerConnection, String> {
    info!(
        "Starting WebRTC session: user_id={}, target_id={}, call_id={}",
        user_id, target_id, call_id
    );

    let mut media_engine = MediaEngine::default();
    
    media_engine
        .register_codec(
            RTCRtpCodecParameters {
                capability: RTCRtpCodecCapability {
                    mime_type: "video/VP8".to_string(),
                    clock_rate: 90000, 
                    channels: 0, 
                    sdp_fmtp_line: "".to_string(), 
                    ..Default::default()
                },
                payload_type: 96, 
                ..Default::default()
            },
            RTPCodecType::Video,
        )
        .map_err(|e| format!("Failed to register codec: {}", e))?;

    let api = APIBuilder::new()
        .with_media_engine(media_engine)
        .build();

    let config = RTCConfiguration {
        ice_servers: vec![RTCIceServer {
            urls: vec!["stun:stun.l.google.com:19302".to_string()],
            ..Default::default()
        }],
        ..Default::default()
    };

    let peer_connection = api
        .new_peer_connection(config)
        .await
        .map_err(|e| format!("Failed to create peer connection: {}", e))?;

    let video_track = Arc::new(TrackLocalStaticRTP::new(
        RTCRtpCodecCapability {
            mime_type: "video/VP8".to_string(),
            clock_rate: 90000,
            channels: 0,
            sdp_fmtp_line: "".to_string(),
            ..Default::default()
        },
        "video".to_string(),
        "webrtc-rs".to_string(),
    ));

    let rtp_sender = peer_connection
        .add_track(video_track as Arc<dyn TrackLocal + Send + Sync>)
        .await
        .map_err(|e| format!("Failed to add video track: {}", e))?;

    tokio::spawn(async move {
        let _ = rtp_sender;
    });

    let offer = peer_connection
        .create_offer(None)
        .await
        .map_err(|e| format!("Failed to create SDP offer: {}", e))?;
    peer_connection
        .set_local_description(offer)
        .await
        .map_err(|e| format!("Failed to set local description: {}", e))?;

    info!("WebRTC session initialized for call_id={}", call_id);
    Ok(peer_connection)
}