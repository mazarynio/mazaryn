use super::types::*;
use anyhow::Result;
use dashmap::DashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Arc;
use tokio::fs;

pub struct TranscodingManager {
    jobs: Arc<DashMap<String, TranscodingJob>>,
    ffmpeg_path: String,
    temp_dir: PathBuf,
}

impl TranscodingManager {
    pub fn new(ffmpeg_path: String, temp_dir: PathBuf) -> Self {
        Self {
            jobs: Arc::new(DashMap::new()),
            ffmpeg_path,
            temp_dir,
        }
    }

    pub async fn create_job(
        &self,
        video_id: String,
        source_cid: String,
        target_qualities: Vec<QualityTarget>,
    ) -> Result<String> {
        let job_id = uuid::Uuid::new_v4().to_string();

        let job = TranscodingJob {
            job_id: job_id.clone(),
            video_id,
            source_cid,
            target_qualities,
            status: TranscodingStatus::Pending,
            progress: 0.0,
        };

        self.jobs.insert(job_id.clone(), job);

        Ok(job_id)
    }

    pub async fn start_job(&self, job_id: &str) -> Result<()> {
        let job = self
            .jobs
            .get(job_id)
            .ok_or_else(|| anyhow::anyhow!("Job not found"))?;

        if job.status != TranscodingStatus::Pending {
            return Err(anyhow::anyhow!("Job already started"));
        }

        let mut job = self.jobs.get_mut(job_id).unwrap();
        job.status = TranscodingStatus::Processing;
        drop(job);

        let jobs_clone = self.jobs.clone();
        let job_id_clone = job_id.to_string();
        let ffmpeg_path = self.ffmpeg_path.clone();
        let temp_dir = self.temp_dir.clone();

        tokio::spawn(async move {
            let result =
                Self::process_job(&jobs_clone, &job_id_clone, &ffmpeg_path, &temp_dir).await;

            if let Some(mut job) = jobs_clone.get_mut(&job_id_clone) {
                match result {
                    Ok(_) => {
                        job.status = TranscodingStatus::Completed;
                        job.progress = 100.0;
                    }
                    Err(e) => {
                        job.status = TranscodingStatus::Failed;
                        eprintln!("Transcoding job {} failed: {}", job_id_clone, e);
                    }
                }
            }
        });

        Ok(())
    }

    async fn process_job(
        jobs: &Arc<DashMap<String, TranscodingJob>>,
        job_id: &str,
        ffmpeg_path: &str,
        temp_dir: &Path,
    ) -> Result<()> {
        let job = jobs
            .get(job_id)
            .ok_or_else(|| anyhow::anyhow!("Job not found"))?
            .clone();

        let input_path = temp_dir.join(format!("{}_input.mp4", job.video_id));
        let _input_format = Self::detect_format(&input_path)?;

        for (idx, quality) in job.target_qualities.iter().enumerate() {
            let output_path = temp_dir.join(format!("{}_{}.mp4", job.video_id, quality.name));

            Self::transcode_video(
                ffmpeg_path,
                &input_path,
                &output_path,
                quality.resolution.clone(),
                quality.bitrate,
                &quality.codec,
            )
            .await?;

            if let Some(mut job) = jobs.get_mut(job_id) {
                job.progress = ((idx + 1) as f32 / job.target_qualities.len() as f32) * 100.0;
            }
        }

        Ok(())
    }

    async fn transcode_video(
        ffmpeg_path: &str,
        input_path: &Path,
        output_path: &Path,
        resolution: Resolution,
        bitrate: u64,
        codec: &str,
    ) -> Result<()> {
        let scale = format!("{}:{}", resolution.width, resolution.height);
        let bitrate_str = format!("{}k", bitrate / 1000);

        let output = Command::new(ffmpeg_path)
            .arg("-i")
            .arg(input_path)
            .arg("-vf")
            .arg(format!("scale={}", scale))
            .arg("-b:v")
            .arg(&bitrate_str)
            .arg("-c:v")
            .arg(codec)
            .arg("-c:a")
            .arg("aac")
            .arg("-y")
            .arg(output_path)
            .output()?;

        if !output.status.success() {
            return Err(anyhow::anyhow!(
                "FFmpeg failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(())
    }

    pub async fn convert_format(
        &self,
        input_path: PathBuf,
        output_path: PathBuf,
        target_format: VideoFormat,
    ) -> Result<()> {
        let codec = target_format.default_codec();
        let audio_codec = target_format.audio_codec();

        let mut cmd = Command::new(&self.ffmpeg_path);
        cmd.arg("-i").arg(&input_path);

        if matches!(target_format, VideoFormat::MP4 | VideoFormat::MOV) {
            cmd.arg("-movflags").arg("+faststart");
        } else if matches!(target_format, VideoFormat::WebM) {
            cmd.arg("-deadline").arg("good").arg("-cpu-used").arg("2");
        }

        cmd.arg("-c:v")
            .arg(&codec)
            .arg("-c:a")
            .arg(&audio_codec)
            .arg("-y")
            .arg(&output_path);

        let output = cmd.output()?;

        if !output.status.success() {
            return Err(anyhow::anyhow!(
                "FFmpeg conversion failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(())
    }

    pub async fn extract_thumbnail(
        &self,
        video_path: PathBuf,
        output_path: PathBuf,
        timestamp: f64,
    ) -> Result<()> {
        let output = Command::new(&self.ffmpeg_path)
            .arg("-i")
            .arg(&video_path)
            .arg("-ss")
            .arg(timestamp.to_string())
            .arg("-vframes")
            .arg("1")
            .arg("-q:v")
            .arg("2")
            .arg("-y")
            .arg(&output_path)
            .output()?;

        if !output.status.success() {
            return Err(anyhow::anyhow!(
                "Thumbnail extraction failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(())
    }

    pub async fn generate_preview(
        &self,
        video_path: PathBuf,
        output_path: PathBuf,
        duration: f64,
    ) -> Result<()> {
        let output = Command::new(&self.ffmpeg_path)
            .arg("-i")
            .arg(&video_path)
            .arg("-t")
            .arg(duration.to_string())
            .arg("-vf")
            .arg("scale=640:360")
            .arg("-c:v")
            .arg("libx264")
            .arg("-c:a")
            .arg("aac")
            .arg("-y")
            .arg(&output_path)
            .output()?;

        if !output.status.success() {
            return Err(anyhow::anyhow!(
                "Preview generation failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ));
        }

        Ok(())
    }

    pub async fn get_video_info(&self, video_path: PathBuf) -> Result<VideoInfo> {
        let output = Command::new(&self.ffmpeg_path)
            .arg("-i")
            .arg(&video_path)
            .output()?;

        let stderr = String::from_utf8_lossy(&output.stderr);

        let duration = Self::parse_duration(&stderr)?;
        let resolution = Self::parse_resolution(&stderr)?;
        let codec = Self::parse_codec(&stderr)?;
        let bitrate = Self::parse_bitrate(&stderr)?;
        let frame_rate = Self::parse_frame_rate(&stderr)?;

        Ok(VideoInfo {
            duration,
            resolution,
            codec,
            bitrate,
            frame_rate,
        })
    }

    fn parse_duration(ffmpeg_output: &str) -> Result<f64> {
        for line in ffmpeg_output.lines() {
            if line.contains("Duration:") {
                if let Some(duration_str) = line.split("Duration:").nth(1) {
                    if let Some(time_str) = duration_str.split(',').next() {
                        let time_str = time_str.trim();
                        let parts: Vec<&str> = time_str.split(':').collect();
                        if parts.len() == 3 {
                            let hours: f64 = parts[0].parse().unwrap_or(0.0);
                            let minutes: f64 = parts[1].parse().unwrap_or(0.0);
                            let seconds: f64 = parts[2].parse().unwrap_or(0.0);
                            return Ok(hours * 3600.0 + minutes * 60.0 + seconds);
                        }
                    }
                }
            }
        }
        Ok(0.0)
    }

    fn parse_resolution(ffmpeg_output: &str) -> Result<String> {
        for line in ffmpeg_output.lines() {
            if line.contains("Video:") && line.contains("x") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                for part in parts {
                    if part.contains("x") && !part.contains("0x") {
                        return Ok(part.trim_end_matches(',').to_string());
                    }
                }
            }
        }
        Ok("unknown".to_string())
    }

    fn parse_codec(ffmpeg_output: &str) -> Result<String> {
        for line in ffmpeg_output.lines() {
            if line.contains("Video:") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                for (i, part) in parts.iter().enumerate() {
                    if *part == "Video:" && i + 1 < parts.len() {
                        return Ok(parts[i + 1].trim_end_matches(',').to_string());
                    }
                }
            }
        }
        Ok("unknown".to_string())
    }

    fn parse_bitrate(ffmpeg_output: &str) -> Result<u64> {
        for line in ffmpeg_output.lines() {
            if line.contains("bitrate:") {
                if let Some(bitrate_str) = line.split("bitrate:").nth(1) {
                    if let Some(rate) = bitrate_str.split_whitespace().next() {
                        if let Ok(value) = rate.parse::<u64>() {
                            return Ok(value * 1000);
                        }
                    }
                }
            }
        }
        Ok(0)
    }

    fn parse_frame_rate(ffmpeg_output: &str) -> Result<f32> {
        for line in ffmpeg_output.lines() {
            if line.contains("fps") {
                let parts: Vec<&str> = line.split_whitespace().collect();
                for (i, part) in parts.iter().enumerate() {
                    if *part == "fps," && i > 0 {
                        if let Ok(fps) = parts[i - 1].parse::<f32>() {
                            return Ok(fps);
                        }
                    }
                }
            }
        }
        Ok(0.0)
    }

    fn detect_format(path: &Path) -> Result<VideoFormat> {
        let extension = path
            .extension()
            .and_then(|e| e.to_str())
            .unwrap_or("")
            .to_lowercase();

        Ok(VideoFormat::from_extension(&extension))
    }

    pub fn get_job(&self, job_id: &str) -> Option<TranscodingJob> {
        self.jobs.get(job_id).map(|j| j.value().clone())
    }

    pub async fn cancel_job(&self, job_id: &str) -> Result<()> {
        let mut job = self
            .jobs
            .get_mut(job_id)
            .ok_or_else(|| anyhow::anyhow!("Job not found"))?;

        if job.status == TranscodingStatus::Processing {
            job.status = TranscodingStatus::Failed;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum VideoFormat {
    MP4,
    WebM,
    MKV,
    MOV,
    AVI,
}

impl VideoFormat {
    pub fn from_extension(ext: &str) -> Self {
        match ext.to_lowercase().as_str() {
            "mp4" => VideoFormat::MP4,
            "webm" => VideoFormat::WebM,
            "mkv" => VideoFormat::MKV,
            "mov" => VideoFormat::MOV,
            "avi" => VideoFormat::AVI,
            _ => VideoFormat::MP4,
        }
    }

    pub fn mime_type(&self) -> &str {
        match self {
            VideoFormat::MP4 => "video/mp4",
            VideoFormat::WebM => "video/webm",
            VideoFormat::MKV => "video/x-matroska",
            VideoFormat::MOV => "video/quicktime",
            VideoFormat::AVI => "video/x-msvideo",
        }
    }

    pub fn default_codec(&self) -> &str {
        match self {
            VideoFormat::MP4 | VideoFormat::MOV => "libx264",
            VideoFormat::WebM => "libvpx-vp9",
            VideoFormat::MKV => "libx264",
            VideoFormat::AVI => "libx264",
        }
    }

    pub fn audio_codec(&self) -> &str {
        match self {
            VideoFormat::WebM => "libopus",
            _ => "aac",
        }
    }
}
