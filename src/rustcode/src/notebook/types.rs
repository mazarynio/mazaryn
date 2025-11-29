use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Language {
    Python,
    R,
    Julia,
    Scala,
    SQL,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KernelSpec {
    pub name: String,
    pub language: Language,
    pub display_name: String,
    pub argv: Vec<String>,
    pub env: HashMap<String, String>,
}

#[derive(Debug, Clone)]
pub struct KernelConfig {
    pub max_execution_time: u64,
    pub max_memory_mb: usize,
    pub enable_gpu: bool,
    pub python_packages: Vec<String>,
    pub r_packages: Vec<String>,
    pub workspace_dir: std::path::PathBuf,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NotebookSession {
    pub session_id: String,
    pub notebook_id: String,
    pub user_id: String,
    pub kernel_id: String,
    pub language: Language,
    pub created_at: DateTime<Utc>,
    pub last_activity: DateTime<Utc>,
    pub status: SessionStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SessionStatus {
    Active,
    Idle,
    Busy,
    Terminated,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionRequest {
    pub code: String,
    pub notebook_id: String,
    pub cell_id: String,
    pub session_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionResult {
    pub execution_id: String,
    pub status: ExecutionStatus,
    pub outputs: Vec<CellOutput>,
    pub execution_time_ms: u128,
    pub error: Option<ExecutionError>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ExecutionStatus {
    Success,
    Error,
    Timeout,
    Cancelled,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CellOutput {
    pub output_type: OutputType,
    pub data: OutputData,
    pub metadata: HashMap<String, serde_json::Value>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OutputType {
    Stream,
    DisplayData,
    ExecuteResult,
    Error,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OutputData {
    Text(String),
    Html(String),
    Image {
        mime_type: String,
        data: String,
    },
    Json(serde_json::Value),
    Table {
        headers: Vec<String>,
        rows: Vec<Vec<String>>,
    },
    Plot {
        format: String,
        data: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionError {
    pub error_name: String,
    pub error_value: String,
    pub traceback: Vec<String>,
}
