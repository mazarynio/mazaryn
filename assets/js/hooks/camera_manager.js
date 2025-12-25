const CameraManager = {
  mounted() {
    this.stream = null;
    this.mediaRecorder = null;
    this.recordedChunks = [];
    this.isRecording = false;
    this.recordingStartTime = null;
    this.durationInterval = null;
    this.durationStarted = false;
    this.recordingBlob = null;
    this.streamId = null;
    const streamingVideo = document.getElementById("streaming-video");
    const cameraPreview = document.getElementById("camera-preview");
    if (streamingVideo) {
      this.isStreaming = true;
      this.handleStreamingPage();
    } else if (cameraPreview) {
      this.isStreaming = false;
      this.handlePreviewPage();
    }
    setTimeout(() => this.attachEventHandlers(), 100);
  },
  async handlePreviewPage() {
    const existingStream = window.MAZARYN_GLOBAL_CAMERA_STREAM;
    if (existingStream && existingStream.active) {
      this.stream = existingStream;
      this.attachStreamToPreview(existingStream);
    } else {
      await this.requestCameraPermission();
    }
  },
  async handleStreamingPage() {
    const existingStream = window.MAZARYN_GLOBAL_CAMERA_STREAM;
    if (existingStream && existingStream.active) {
      this.stream = existingStream;
      this.attachStreamToStreaming(existingStream);
      this.startDurationCounter();
      const autoRecord = document.querySelector('[data-auto-record="true"]');
      if (autoRecord) {
        setTimeout(() => this.startRecording(), 2000);
      }
    } else {
      await this.requestCameraPermission();
      if (this.stream) {
        this.startDurationCounter();
        const autoRecord = document.querySelector('[data-auto-record="true"]');
        if (autoRecord) {
          setTimeout(() => this.startRecording(), 2000);
        }
      }
    }
    const streamIdEl = document.querySelector("[data-stream-id]");
    if (streamIdEl) {
      this.streamId = streamIdEl.dataset.streamId;
    }
  },
  async requestCameraPermission() {
    try {
      const constraints = {
        video: {
          width: { ideal: 1920, min: 1280 },
          height: { ideal: 1080, min: 720 },
          frameRate: { ideal: 60, min: 30 },
          facingMode: "user",
          resizeMode: "crop-and-scale",
        },
        audio: {
          echoCancellation: true,
          noiseSuppression: true,
          autoGainControl: true,
          sampleRate: { ideal: 48000 },
          channelCount: { ideal: 2 },
        },
      };
      const stream = await navigator.mediaDevices.getUserMedia(constraints);
      const videoTrack = stream.getVideoTracks()[0];
      if (videoTrack) {
        const settings = videoTrack.getSettings();
        console.log("Camera started:", {
          resolution: `${settings.width}x${settings.height}`,
          frameRate: settings.frameRate,
          device: videoTrack.label,
        });
      }
      this.stream = stream;
      window.MAZARYN_GLOBAL_CAMERA_STREAM = stream;
      window.globalCameraStream = stream;
      if (this.isStreaming) {
        this.attachStreamToStreaming(stream);
      } else {
        this.attachStreamToPreview(stream);
      }
      this.pushEvent("camera_ready", {});
      const startBtn = document.getElementById("start-camera-btn");
      if (startBtn) startBtn.style.display = "none";
      return true;
    } catch (error) {
      console.error("Camera permission error:", error);
      if (error.name === "OverconstrainedError") {
        return this.requestFallbackCamera();
      }
      alert("Camera permission denied. Please allow camera access.");
      return false;
    }
  },
  async requestFallbackCamera() {
    try {
      const fallbackStream = await navigator.mediaDevices.getUserMedia({
        video: {
          width: { ideal: 1280 },
          height: { ideal: 720 },
          frameRate: { ideal: 30 },
          facingMode: "user",
        },
        audio: true,
      });
      this.stream = fallbackStream;
      window.MAZARYN_GLOBAL_CAMERA_STREAM = fallbackStream;
      if (this.isStreaming) {
        this.attachStreamToStreaming(fallbackStream);
      } else {
        this.attachStreamToPreview(fallbackStream);
      }
      this.pushEvent("camera_ready", {});
      return true;
    } catch (err) {
      console.error("Fallback camera failed:", err);
      alert("Unable to access camera. Please check permissions.");
      return false;
    }
  },
  attachStreamToPreview(stream) {
    const video = document.getElementById("camera-preview");
    if (!video) return;
    video.srcObject = stream;
    video.muted = true;
    video.autoplay = true;
    video.playsInline = true;
    video.play();
  },
  attachStreamToStreaming(stream) {
    const video = document.getElementById("streaming-video");
    if (!video) {
      setTimeout(() => this.attachStreamToStreaming(stream), 300);
      return;
    }
    video.srcObject = stream;
    video.muted = false;
    video.autoplay = true;
    video.playsInline = true;
    video.play();
  },
  attachEventHandlers() {
    const startBtn = document.getElementById("start-camera-btn");
    if (startBtn && !startBtn.dataset.listenerAttached) {
      startBtn.addEventListener("click", () => this.requestCameraPermission());
      startBtn.dataset.listenerAttached = "true";
    }
    const recordBtn = document.getElementById("test-record-btn");
    if (recordBtn && !recordBtn.dataset.listenerAttached) {
      recordBtn.addEventListener("click", () => {
        if (this.isRecording) {
          this.stopRecording();
        } else {
          this.startRecording();
        }
      });
      recordBtn.dataset.listenerAttached = "true";
    }
    const continueBtn = document.getElementById("continue-btn");
    if (continueBtn && !continueBtn.dataset.listenerAttached) {
      continueBtn.addEventListener("click", () => {
        const title = document.getElementById("stream-title")?.value || "";
        const description =
          document.getElementById("stream-description")?.value || "";
        const category =
          document.getElementById("stream-category")?.value || "gaming";
        if (!title.trim()) {
          alert("Please enter a stream title");
          return;
        }
        if (this.stream && this.stream.active) {
          window.MAZARYN_GLOBAL_CAMERA_STREAM = this.stream;
        }
        this.pushEvent("setup_stream", { title, description, category });
      });
      continueBtn.dataset.listenerAttached = "true";
    }
    const endStreamBtn = document.querySelector('[phx-click="end_stream"]');
    if (endStreamBtn && !endStreamBtn.dataset.jsListenerAttached) {
      endStreamBtn.addEventListener(
        "click",
        async (e) => {
          e.preventDefault();
          let recordingFile = null;
          let durationSeconds = 0;
          if (this.isRecording && this.mediaRecorder) {
            try {
              await new Promise((resolve) => {
                const timeout = setTimeout(resolve, 8000);
                this.mediaRecorder.onstop = () => {
                  clearTimeout(timeout);
                  if (this.recordedChunks.length === 0) {
                    resolve();
                    return;
                  }
                  const blob = new Blob(this.recordedChunks, {
                    type: "video/webm",
                  });
                  durationSeconds = Math.floor(
                    (Date.now() - this.recordingStartTime) / 1000,
                  );
                  recordingFile = new File(
                    [blob],
                    `livestream_${this.streamId}_${Date.now()}.webm`,
                    {
                      type: "video/webm",
                    },
                  );
                  resolve();
                };
                this.mediaRecorder.stop();
              });
            } catch (err) {
              console.error("Error stopping recording:", err);
            }
          }
          if (this.durationInterval) {
            clearInterval(this.durationInterval);
            this.durationInterval = null;
          }
          if (this.stream) {
            this.stream.getTracks().forEach((track) => track.stop());
            this.stream = null;
            window.MAZARYN_GLOBAL_CAMERA_STREAM = null;
          }
          if (recordingFile && this.streamId) {
            await this.uploadRecordingFile(
              recordingFile,
              durationSeconds,
              this.streamId,
            );
          }
          this.pushEvent("end_stream", {});
        },
        { capture: true },
      );
      endStreamBtn.dataset.jsListenerAttached = "true";
    }
  },
  startRecording() {
    if (!this.stream || !this.stream.active) {
      alert("Please start the camera first");
      return;
    }
    if (this.isRecording) {
      return;
    }
    this.recordedChunks = [];
    this.recordingStartTime = Date.now();
    const mimeType = MediaRecorder.isTypeSupported("video/webm;codecs=vp9,opus")
      ? "video/webm;codecs=vp9,opus"
      : MediaRecorder.isTypeSupported("video/webm;codecs=vp8,opus")
        ? "video/webm;codecs=vp8,opus"
        : "video/webm";
    try {
      this.mediaRecorder = new MediaRecorder(this.stream, {
        mimeType: mimeType,
        videoBitsPerSecond: 5000000,
        audioBitsPerSecond: 192000,
      });
      this.mediaRecorder.ondataavailable = (event) => {
        if (event.data && event.data.size > 0) {
          this.recordedChunks.push(event.data);
        }
      };
      this.mediaRecorder.onstop = () => {
        if (this.recordedChunks.length === 0) return;
        const blob = new Blob(this.recordedChunks, { type: mimeType });
        this.recordingBlob = blob;
        this.isRecording = false;
      };
      this.mediaRecorder.start(1000);
      this.isRecording = true;
      this.pushEvent("start_recording", {});
    } catch (error) {
      console.error("Failed to start recording:", error);
      alert("Failed to start recording");
    }
  },
  stopRecording() {
    if (this.isRecording && this.mediaRecorder) {
      this.mediaRecorder.stop();
    }
  },
  async uploadRecordingFile(file, durationSeconds, streamId) {
    try {
      const formData = new FormData();
      formData.append("recording_file", file);
      formData.append("duration", durationSeconds);
      formData.append("size", file.size);
      formData.append("mime_type", file.type);
      const csrfToken = document.querySelector(
        'meta[name="csrf-token"]',
      )?.content;
      const uploadUrl = `/api/livestreams/${streamId}/save-recording`;
      const response = await fetch(uploadUrl, {
        method: "POST",
        body: formData,
        headers: {
          "x-csrf-token": csrfToken,
        },
      });
      if (response.ok) {
        this.pushEvent("recording_uploaded", { stream_id: streamId });
      } else {
        const error = await response.text();
        this.pushEvent("recording_upload_failed", { error });
      }
    } catch (error) {
      this.pushEvent("recording_upload_failed", { error: error.message });
    }
  },
  startDurationCounter() {
    if (this.durationStarted) return;
    this.durationStarted = true;
    const startTime = Date.now();
    this.durationInterval = setInterval(() => {
      const elapsed = Math.floor((Date.now() - startTime) / 1000);
      const minutes = Math.floor(elapsed / 60);
      const seconds = elapsed % 60;
      const durationEl = document.getElementById("stream-duration");
      if (durationEl) {
        durationEl.textContent = `${minutes}:${seconds.toString().padStart(2, "0")}`;
      }
    }, 1000);
  },
  updated() {
    const streamingVideo = document.getElementById("streaming-video");
    if (streamingVideo && !streamingVideo.srcObject) {
      const stream = this.stream || window.MAZARYN_GLOBAL_CAMERA_STREAM;
      if (stream && stream.active) {
        this.attachStreamToStreaming(stream);
        if (!this.durationStarted) {
          this.startDurationCounter();
        }
      }
    }
    this.attachEventHandlers();
  },
  destroyed() {
    if (this.durationInterval) clearInterval(this.durationInterval);
    if (this.stream) {
      this.stream.getTracks().forEach((track) => track.stop());
    }
  },
};
export default CameraManager;
