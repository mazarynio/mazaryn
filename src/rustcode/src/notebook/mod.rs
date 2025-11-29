pub mod api;
pub mod kernel;
pub mod types;
pub mod websocket;

use dashmap::DashMap;
use kernel::KernelInstance;
use parking_lot::RwLock;
use std::sync::Arc;
use types::*;
use uuid::Uuid;

pub struct KernelManager {
    kernels: Arc<DashMap<String, Arc<RwLock<KernelInstance>>>>,
    sessions: Arc<DashMap<String, NotebookSession>>,
    config: KernelConfig,
}

impl KernelManager {
    pub fn new(config: KernelConfig) -> anyhow::Result<Self> {
        Ok(Self {
            kernels: Arc::new(DashMap::new()),
            sessions: Arc::new(DashMap::new()),
            config,
        })
    }

    pub async fn create_kernel(&self, spec: KernelSpec) -> anyhow::Result<String> {
        let kernel_id = Uuid::new_v4().to_string();
        let kernel = KernelInstance::new(kernel_id.clone(), spec, &self.config).await?;

        self.kernels
            .insert(kernel_id.clone(), Arc::new(RwLock::new(kernel)));
        Ok(kernel_id)
    }

    pub async fn execute_code(
        &self,
        kernel_id: &str,
        code: String,
        notebook_id: String,
        cell_id: String,
    ) -> anyhow::Result<ExecutionResult> {
        let kernel = self
            .kernels
            .get(kernel_id)
            .ok_or_else(|| anyhow::anyhow!("Kernel not found"))?;

        let mut kernel_lock = kernel.write();
        kernel_lock.execute(code, notebook_id, cell_id).await
    }

    pub async fn shutdown_kernel(&self, kernel_id: &str) -> anyhow::Result<()> {
        if let Some((_, kernel)) = self.kernels.remove(kernel_id) {
            kernel.write().shutdown().await?;
        }
        Ok(())
    }

    pub async fn create_session(
        &self,
        notebook_id: String,
        user_id: String,
        language: Language,
    ) -> anyhow::Result<String> {
        let session_id = Uuid::new_v4().to_string();
        let spec = self.get_kernel_spec(&language)?;
        let kernel_id = self.create_kernel(spec).await?;

        let session = NotebookSession {
            session_id: session_id.clone(),
            notebook_id,
            user_id,
            kernel_id: kernel_id.clone(),
            language,
            created_at: chrono::Utc::now(),
            last_activity: chrono::Utc::now(),
            status: SessionStatus::Active,
        };

        self.sessions.insert(session_id.clone(), session);
        Ok(session_id)
    }

    pub fn get_session(&self, session_id: &str) -> Option<NotebookSession> {
        self.sessions.get(session_id).map(|s| s.clone())
    }

    pub async fn close_session(&self, session_id: &str) -> anyhow::Result<()> {
        if let Some((_, session)) = self.sessions.remove(session_id) {
            self.shutdown_kernel(&session.kernel_id).await?;
        }
        Ok(())
    }

    pub fn list_sessions(&self) -> Vec<NotebookSession> {
        self.sessions
            .iter()
            .map(|entry| entry.value().clone())
            .collect()
    }

    fn get_kernel_spec(&self, language: &Language) -> anyhow::Result<KernelSpec> {
        match language {
            Language::Python => Ok(KernelSpec {
                name: "python3".to_string(),
                language: Language::Python,
                display_name: "Python 3".to_string(),
                argv: vec!["python3".to_string()],
                env: Default::default(),
            }),
            Language::R => Ok(KernelSpec {
                name: "ir".to_string(),
                language: Language::R,
                display_name: "R".to_string(),
                argv: vec!["Rscript".to_string()],
                env: Default::default(),
            }),
            Language::Julia => Ok(KernelSpec {
                name: "julia".to_string(),
                language: Language::Julia,
                display_name: "Julia".to_string(),
                argv: vec!["julia".to_string()],
                env: Default::default(),
            }),
            Language::Scala => Ok(KernelSpec {
                name: "scala".to_string(),
                language: Language::Scala,
                display_name: "Scala".to_string(),
                argv: vec!["scala".to_string()],
                env: Default::default(),
            }),
            Language::SQL => Ok(KernelSpec {
                name: "sql".to_string(),
                language: Language::SQL,
                display_name: "SQL".to_string(),
                argv: vec![],
                env: Default::default(),
            }),
        }
    }
}
