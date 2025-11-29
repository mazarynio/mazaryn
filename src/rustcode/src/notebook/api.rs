use super::{types::*, KernelManager};
use actix_web::{web, HttpResponse, Result};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

#[derive(Debug, Deserialize)]
pub struct CreateSessionRequest {
    pub notebook_id: String,
    pub user_id: String,
    pub language: Language,
}

#[derive(Debug, Serialize)]
pub struct CreateSessionResponse {
    pub session_id: String,
    pub kernel_id: String,
}

#[derive(Debug, Deserialize)]
pub struct ExecuteCodeRequest {
    pub session_id: String,
    pub code: String,
    pub cell_id: String,
}

pub async fn create_session(
    manager: web::Data<Arc<KernelManager>>,
    req: web::Json<CreateSessionRequest>,
) -> Result<HttpResponse> {
    match manager
        .create_session(
            req.notebook_id.clone(),
            req.user_id.clone(),
            req.language.clone(),
        )
        .await
    {
        Ok(session_id) => {
            let session = manager.get_session(&session_id).unwrap();
            Ok(HttpResponse::Ok().json(CreateSessionResponse {
                session_id,
                kernel_id: session.kernel_id,
            }))
        }
        Err(e) => Ok(HttpResponse::InternalServerError().json(serde_json::json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn execute_code(
    manager: web::Data<Arc<KernelManager>>,
    req: web::Json<ExecuteCodeRequest>,
) -> Result<HttpResponse> {
    let session = match manager.get_session(&req.session_id) {
        Some(s) => s,
        None => {
            return Ok(HttpResponse::NotFound().json(serde_json::json!({
                "error": "Session not found"
            })))
        }
    };

    match manager
        .execute_code(
            &session.kernel_id,
            req.code.clone(),
            session.notebook_id.clone(),
            req.cell_id.clone(),
        )
        .await
    {
        Ok(result) => Ok(HttpResponse::Ok().json(result)),
        Err(e) => Ok(HttpResponse::InternalServerError().json(serde_json::json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn close_session(
    manager: web::Data<Arc<KernelManager>>,
    session_id: web::Path<String>,
) -> Result<HttpResponse> {
    match manager.close_session(&session_id).await {
        Ok(_) => Ok(HttpResponse::Ok().json(serde_json::json!({
            "status": "closed"
        }))),
        Err(e) => Ok(HttpResponse::InternalServerError().json(serde_json::json!({
            "error": e.to_string()
        }))),
    }
}

pub async fn get_session_status(
    manager: web::Data<Arc<KernelManager>>,
    session_id: web::Path<String>,
) -> Result<HttpResponse> {
    match manager.get_session(&session_id) {
        Some(session) => Ok(HttpResponse::Ok().json(session)),
        None => Ok(HttpResponse::NotFound().json(serde_json::json!({
            "error": "Session not found"
        }))),
    }
}
