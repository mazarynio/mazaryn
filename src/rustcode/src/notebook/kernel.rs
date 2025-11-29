use super::types::*;
use anyhow::Result;
use std::process::Stdio;
use std::time::Instant;
use tokio::io::{AsyncBufReadExt, BufReader};
use tokio::process::Command;

pub struct KernelInstance {
    pub kernel_id: String,
    pub spec: KernelSpec,
    pub status: SessionStatus,
    pub execution_count: u32,
    config: KernelConfig,
}

impl KernelInstance {
    pub async fn new(kernel_id: String, spec: KernelSpec, config: &KernelConfig) -> Result<Self> {
        Ok(Self {
            kernel_id,
            spec,
            status: SessionStatus::Idle,
            execution_count: 0,
            config: config.clone(),
        })
    }

    pub async fn execute(
        &mut self,
        code: String,
        _notebook_id: String,
        _cell_id: String,
    ) -> Result<ExecutionResult> {
        self.status = SessionStatus::Busy;
        self.execution_count += 1;

        let execution_id = uuid::Uuid::new_v4().to_string();
        let start = Instant::now();

        let result = match &self.spec.language {
            Language::Python => self.execute_python(&code).await,
            Language::R => self.execute_r(&code).await,
            Language::Julia => self.execute_julia(&code).await,
            Language::Scala => self.execute_scala(&code).await,
            Language::SQL => self.execute_sql(&code).await,
        };

        let execution_time = start.elapsed().as_millis();
        self.status = SessionStatus::Idle;

        match result {
            Ok(outputs) => Ok(ExecutionResult {
                execution_id,
                status: ExecutionStatus::Success,
                outputs,
                execution_time_ms: execution_time,
                error: None,
            }),
            Err(e) => Ok(ExecutionResult {
                execution_id,
                status: ExecutionStatus::Error,
                outputs: vec![],
                execution_time_ms: execution_time,
                error: Some(ExecutionError {
                    error_name: "ExecutionError".to_string(),
                    error_value: e.to_string(),
                    traceback: vec![e.to_string()],
                }),
            }),
        }
    }

    async fn execute_python(&self, code: &str) -> Result<Vec<CellOutput>> {
        let workspace = self.config.workspace_dir.join(&self.kernel_id);
        tokio::fs::create_dir_all(&workspace).await?;

        let temp_file = tempfile::Builder::new()
            .prefix("cell_")
            .suffix(".py")
            .tempfile_in(&workspace)?;

        tokio::fs::write(temp_file.path(), code).await?;

        let mut child = Command::new("python3")
            .arg("-u")
            .arg(temp_file.path())
            .current_dir(&workspace)
            .env("PYTHONUNBUFFERED", "1")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdout = child.stdout.take().unwrap();
        let stderr = child.stderr.take().unwrap();

        let stdout_reader = BufReader::new(stdout);
        let stderr_reader = BufReader::new(stderr);

        let mut outputs = vec![];
        let mut stdout_lines = stdout_reader.lines();
        let mut stderr_lines = stderr_reader.lines();

        let stdout_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stdout_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let stderr_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stderr_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let status = child.wait().await?;

        let stdout_lines = stdout_task.await?;
        let stderr_lines = stderr_task.await?;

        for line in stdout_lines {
            outputs.push(CellOutput {
                output_type: OutputType::Stream,
                data: OutputData::Text(line),
                metadata: std::collections::HashMap::new(),
            });
        }

        if !stderr_lines.is_empty() {
            let error_text = stderr_lines.join("\n");
            outputs.push(CellOutput {
                output_type: if status.success() {
                    OutputType::Stream
                } else {
                    OutputType::Error
                },
                data: OutputData::Text(error_text),
                metadata: std::collections::HashMap::new(),
            });
        }

        Ok(outputs)
    }

    async fn execute_r(&self, code: &str) -> Result<Vec<CellOutput>> {
        let workspace = self.config.workspace_dir.join(&self.kernel_id);
        tokio::fs::create_dir_all(&workspace).await?;

        let temp_file = tempfile::Builder::new()
            .prefix("cell_")
            .suffix(".R")
            .tempfile_in(&workspace)?;

        tokio::fs::write(temp_file.path(), code).await?;

        let mut child = Command::new("Rscript")
            .arg("--vanilla")
            .arg(temp_file.path())
            .current_dir(&workspace)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdout = child.stdout.take().unwrap();
        let stderr = child.stderr.take().unwrap();

        let stdout_reader = BufReader::new(stdout);
        let stderr_reader = BufReader::new(stderr);

        let mut outputs = vec![];
        let mut stdout_lines = stdout_reader.lines();
        let mut stderr_lines = stderr_reader.lines();

        let stdout_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stdout_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let stderr_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stderr_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let status = child.wait().await?;

        let stdout_lines = stdout_task.await?;
        let stderr_lines = stderr_task.await?;

        for line in stdout_lines {
            outputs.push(CellOutput {
                output_type: OutputType::Stream,
                data: OutputData::Text(line),
                metadata: std::collections::HashMap::new(),
            });
        }

        if !stderr_lines.is_empty() {
            let error_text = stderr_lines.join("\n");
            outputs.push(CellOutput {
                output_type: if status.success() {
                    OutputType::Stream
                } else {
                    OutputType::Error
                },
                data: OutputData::Text(error_text),
                metadata: std::collections::HashMap::new(),
            });
        }

        Ok(outputs)
    }

    async fn execute_julia(&self, code: &str) -> Result<Vec<CellOutput>> {
        let workspace = self.config.workspace_dir.join(&self.kernel_id);
        tokio::fs::create_dir_all(&workspace).await?;

        let temp_file = tempfile::Builder::new()
            .prefix("cell_")
            .suffix(".jl")
            .tempfile_in(&workspace)?;

        tokio::fs::write(temp_file.path(), code).await?;

        let mut child = Command::new("julia")
            .arg(temp_file.path())
            .current_dir(&workspace)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdout = child.stdout.take().unwrap();
        let stderr = child.stderr.take().unwrap();

        let stdout_reader = BufReader::new(stdout);
        let stderr_reader = BufReader::new(stderr);

        let mut outputs = vec![];
        let mut stdout_lines = stdout_reader.lines();
        let mut stderr_lines = stderr_reader.lines();

        let stdout_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stdout_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let stderr_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stderr_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let status = child.wait().await?;

        let stdout_lines = stdout_task.await?;
        let stderr_lines = stderr_task.await?;

        for line in stdout_lines {
            outputs.push(CellOutput {
                output_type: OutputType::Stream,
                data: OutputData::Text(line),
                metadata: std::collections::HashMap::new(),
            });
        }

        if !stderr_lines.is_empty() {
            let error_text = stderr_lines.join("\n");
            outputs.push(CellOutput {
                output_type: if status.success() {
                    OutputType::Stream
                } else {
                    OutputType::Error
                },
                data: OutputData::Text(error_text),
                metadata: std::collections::HashMap::new(),
            });
        }

        Ok(outputs)
    }

    async fn execute_scala(&self, code: &str) -> Result<Vec<CellOutput>> {
        let workspace = self.config.workspace_dir.join(&self.kernel_id);
        tokio::fs::create_dir_all(&workspace).await?;

        let temp_file = tempfile::Builder::new()
            .prefix("cell_")
            .suffix(".scala")
            .tempfile_in(&workspace)?;

        tokio::fs::write(temp_file.path(), code).await?;

        let mut child = Command::new("scala")
            .arg(temp_file.path())
            .current_dir(&workspace)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        let stdout = child.stdout.take().unwrap();
        let stderr = child.stderr.take().unwrap();

        let stdout_reader = BufReader::new(stdout);
        let stderr_reader = BufReader::new(stderr);

        let mut outputs = vec![];
        let mut stdout_lines = stdout_reader.lines();
        let mut stderr_lines = stderr_reader.lines();

        let stdout_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stdout_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let stderr_task = tokio::spawn(async move {
            let mut lines = vec![];
            while let Ok(Some(line)) = stderr_lines.next_line().await {
                lines.push(line);
            }
            lines
        });

        let status = child.wait().await?;

        let stdout_lines = stdout_task.await?;
        let stderr_lines = stderr_task.await?;

        for line in stdout_lines {
            outputs.push(CellOutput {
                output_type: OutputType::Stream,
                data: OutputData::Text(line),
                metadata: std::collections::HashMap::new(),
            });
        }

        if !stderr_lines.is_empty() {
            let error_text = stderr_lines.join("\n");
            outputs.push(CellOutput {
                output_type: if status.success() {
                    OutputType::Stream
                } else {
                    OutputType::Error
                },
                data: OutputData::Text(error_text),
                metadata: std::collections::HashMap::new(),
            });
        }

        Ok(outputs)
    }

    async fn execute_sql(&self, _code: &str) -> Result<Vec<CellOutput>> {
        Ok(vec![CellOutput {
            output_type: OutputType::Error,
            data: OutputData::Text(
                "SQL execution not yet implemented. Connect to a database first.".to_string(),
            ),
            metadata: std::collections::HashMap::new(),
        }])
    }

    pub async fn shutdown(&mut self) -> Result<()> {
        self.status = SessionStatus::Terminated;
        Ok(())
    }
}
