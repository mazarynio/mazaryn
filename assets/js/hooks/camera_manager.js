const CameraManager = {
  mounted() {
    console.log("🎥 [CameraManager] HOOK MOUNTED");
    this.stream = null;
    this.mediaRecorder = null;
    this.recordedChunks = [];
    this.isRecording = false;
    this.recordingStartTime = null;
    this.durationInterval = null;
    this.durationStarted = false;
    const streamingVideo = document.getElementById("streaming-video");
    const cameraPreview = document.getElementById("camera-preview");
    if (streamingVideo) {
      console.log("📺 Detected STREAMING page");
      this.isStreaming = true;
      this.handleStreamingPage();
    } else if (cameraPreview) {
      console.log("📹 Detected PREVIEW/SETUP page");
      this.isStreaming = false;
      this.handlePreviewPage();
    }
    setTimeout(() => this.attachEventHandlers(), 100);
  },
  async handlePreviewPage() {
    console.log("🎬 Setting up preview page...");
    const existingStream = window.MAZARYN_GLOBAL_CAMERA_STREAM;
    if (existingStream && existingStream.active) {
      console.log("♻️ Reusing existing camera stream for preview");
      this.stream = existingStream;
      this.attachStreamToPreview(existingStream);
    } else {
      console.log("🆕 Requesting new camera permission for preview");
      await this.requestCameraPermission();
    }
  },
  async handleStreamingPage() {
    console.log("📡 Setting up streaming page...");
    const existingStream = window.MAZARYN_GLOBAL_CAMERA_STREAM;
    if (existingStream && existingStream.active) {
      console.log("✅ Found existing active camera stream!");
      console.log("Stream info:", {
        id: existingStream.id,
        active: existingStream.active,
        videoTracks: existingStream.getVideoTracks().length,
        audioTracks: existingStream.getAudioTracks().length,
      });
      this.stream = existingStream;
      this.attachStreamToStreaming(existingStream);
      this.startDurationCounter();
      const autoRecord = document.querySelector('[data-auto-record="true"]');
      if (autoRecord) {
        console.log(
          "🔴 Auto-record enabled, starting recording in 2 seconds...",
        );
        setTimeout(() => this.startRecording(), 2000);
      }
    } else {
      console.warn("⚠️ No existing stream found, requesting new permission");
      await this.requestCameraPermission();
      if (this.stream) {
        this.startDurationCounter();
        const autoRecord = document.querySelector('[data-auto-record="true"]');
        if (autoRecord) {
          setTimeout(() => this.startRecording(), 2000);
        }
      }
    }
  },
  async requestCameraPermission() {
    console.log("📸 Requesting camera permission...");
    try {
      const stream = await navigator.mediaDevices.getUserMedia({
        video: {
          width: { ideal: 1920 },
          height: { ideal: 1080 },
          facingMode: "user",
        },
        audio: {
          echoCancellation: true,
          noiseSuppression: true,
          autoGainControl: true,
        },
      });
      console.log("✅ Camera permission granted!");
      console.log("Stream details:", {
        id: stream.id,
        active: stream.active,
        videoTracks: stream.getVideoTracks().length,
        audioTracks: stream.getAudioTracks().length,
      });
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
      if (startBtn) {
        startBtn.style.display = "none";
      }
      return true;
    } catch (error) {
      console.error("❌ Camera permission error:", error);
      if (error.name === "NotAllowedError") {
        this.showPermissionPrompt();
      } else {
        this.showError(error);
      }
      return false;
    }
  },
  attachStreamToPreview(stream) {
    const video = document.getElementById("camera-preview");
    if (!video) {
      console.warn("⚠️ Preview video element not found");
      return;
    }
    video.srcObject = stream;
    video.muted = true;
    video.autoplay = true;
    video.playsInline = true;
    video
      .play()
      .then(() => {
        console.log("✅ Preview video playing");
      })
      .catch((err) => {
        console.error("Preview play error:", err);
      });
  },
  attachStreamToStreaming(stream) {
    const video = document.getElementById("streaming-video");
    if (!video) {
      console.warn("⚠️ Streaming video element not found");
      setTimeout(() => this.attachStreamToStreaming(stream), 300);
      return;
    }
    console.log("🔗 Attaching stream to streaming video...");
    video.srcObject = stream;
    video.muted = false;
    video.autoplay = true;
    video.playsInline = true;
    video
      .play()
      .then(() => {
        console.log("✅✅✅ STREAMING VIDEO IS NOW PLAYING! ✅✅✅");
      })
      .catch((err) => {
        console.error("❌ Streaming play error:", err);
        setTimeout(() => {
          video.play().catch((e) => console.error("Retry failed:", e));
        }, 500);
      });
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
          console.log("💾 Storing camera stream globally before navigation");
          window.MAZARYN_GLOBAL_CAMERA_STREAM = this.stream;
        }
        this.pushEvent("setup_stream", {
          title: title,
          description: description,
          category: category,
        });
      });
      continueBtn.dataset.listenerAttached = "true";
    }
    const endStreamBtn = document.querySelector('[phx-click="end_stream"]');
    if (endStreamBtn && !endStreamBtn.dataset.jsListenerAttached) {
      console.log("🔴 Attaching END STREAM handler");
      endStreamBtn.addEventListener(
        "click",
        async (e) => {
          console.log("🛑 END STREAM CLICKED - JS cleanup");
          if (this.isRecording && this.mediaRecorder) {
            try {
              await new Promise((resolve) => {
                this.mediaRecorder.onstop = resolve;
                this.mediaRecorder.stop();
              });
              console.log("Recording stopped");
            } catch (err) {
              console.error("Failed to stop recording:", err);
            }
          }
          if (this.stream) {
            this.stream.getTracks().forEach((track) => track.stop());
            this.stream = null;
            window.MAZARYN_GLOBAL_CAMERA_STREAM = null;
            console.log("Camera stopped");
          }
          if (this.durationInterval) {
            clearInterval(this.durationInterval);
            this.durationInterval = null;
          }
          this.pushEvent("end_stream", {});
        },
        { capture: true },
      );
      endStreamBtn.dataset.jsListenerAttached = "true";
      console.log("✅ End stream handler attached");
    }
  },
  startRecording() {
    if (!this.stream || !this.stream.active) {
      alert("Please start the camera first");
      return;
    }
    if (this.isRecording) {
      console.warn("Already recording");
      return;
    }
    console.log("🔴 Starting recording...");
    this.recordedChunks = [];
    this.recordingStartTime = Date.now();
    const mimeTypes = [
      "video/webm;codecs=vp9,opus",
      "video/webm;codecs=vp8,opus",
      "video/webm;codecs=vp9",
      "video/webm;codecs=vp8",
      "video/webm",
      "video/mp4",
    ];
    const mimeType = mimeTypes.find((type) =>
      MediaRecorder.isTypeSupported(type),
    );
    if (!mimeType) {
      alert("Recording not supported in this browser");
      return;
    }
    console.log("Using MIME type:", mimeType);
    try {
      this.mediaRecorder = new MediaRecorder(this.stream, {
        mimeType: mimeType,
        videoBitsPerSecond: 2500000,
        audioBitsPerSecond: 128000,
      });
      this.mediaRecorder.ondataavailable = (event) => {
        if (event.data && event.data.size > 0) {
          this.recordedChunks.push(event.data);
          console.log(
            `📦 Chunk: ${event.data.size} bytes (Total: ${this.recordedChunks.length})`,
          );
        }
      };
      this.mediaRecorder.onstop = async () => {
        console.log("⏹️ Recording stopped");
        if (this.recordedChunks.length === 0) {
          console.warn("No chunks recorded!");
          return;
        }
        const blob = new Blob(this.recordedChunks, { type: mimeType });
        const durationSeconds = Math.floor(
          (Date.now() - this.recordingStartTime) / 1000,
        );
        console.log(`✅ Recording complete:`, {
          size: blob.size,
          duration: durationSeconds,
          chunks: this.recordedChunks.length,
          mimeType: mimeType,
        });
        if (this.isStreaming) {
          console.log("📤 Uploading recording...");
          await this.uploadRecording(blob, durationSeconds);
        } else {
          this.downloadRecording(blob, "test");
        }
        this.isRecording = false;
        this.pushEvent("stop_recording", {});
      };
      this.mediaRecorder.onerror = (event) => {
        console.error("Recording error:", event.error);
      };
      this.mediaRecorder.start(1000);
      this.isRecording = true;
      this.pushEvent("start_recording", {});
      console.log("✅ MediaRecorder started");
    } catch (error) {
      console.error("Failed to start recording:", error);
      alert("Failed to start recording: " + error.message);
    }
  },
  stopRecording() {
    if (!this.isRecording || !this.mediaRecorder) {
      return;
    }
    console.log("⏹️ Stopping recording...");
    this.mediaRecorder.stop();
  },
  async uploadRecording(blob, durationSeconds) {
    try {
      console.log("📤 Preparing upload...");
      const mockCID = `Qm${Math.random().toString(36).substring(2, 15)}${Math.random().toString(36).substring(2, 15)}`;
      console.log("Simulated IPFS upload, CID:", mockCID);
      if (this.isStreaming) {
        this.pushEvent("save_recording", {
          cid: mockCID,
          size: blob.size,
          duration: durationSeconds,
        });
      }
      this.downloadRecording(blob, "stream");
      console.log("✅ Recording saved!");
    } catch (error) {
      console.error("Upload error:", error);
      this.downloadRecording(blob, "backup");
    }
  },
  downloadRecording(blob, type) {
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `mazaryn-${type}-${Date.now()}.webm`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
    console.log(`💾 Downloaded ${type} recording`);
  },
  startDurationCounter() {
    if (this.durationStarted) {
      console.log("Duration counter already started");
      return;
    }
    this.durationStarted = true;
    const startTime = Date.now();
    this.durationInterval = setInterval(() => {
      const elapsed = Math.floor((Date.now() - startTime) / 1000);
      const minutes = Math.floor(elapsed / 60);
      const seconds = elapsed % 60;
      const formatted = `${minutes}:${seconds.toString().padStart(2, "0")}`;
      const durationEl = document.getElementById("stream-duration");
      if (durationEl) {
        durationEl.textContent = formatted;
      }
    }, 1000);
    console.log("⏱️ Duration counter started");
  },
  showPermissionPrompt() {
    alert(
      `Camera permission required. Please allow camera and microphone access.`,
    );
  },
  showError(error) {
    let message = "Camera error: " + error.message;
    if (error.name === "NotFoundError") {
      message = "No camera found. Please connect a camera and try again.";
    } else if (error.name === "NotReadableError") {
      message = "Camera is already in use by another application.";
    } else if (error.name === "OverconstrainedError") {
      message = "Camera doesn't support the requested resolution.";
    }
    alert(message);
  },
  updated() {
    console.log("🔄 [CameraManager] Hook updated");
    const streamingVideo = document.getElementById("streaming-video");
    if (streamingVideo && !streamingVideo.srcObject) {
      const stream = this.stream || window.MAZARYN_GLOBAL_CAMERA_STREAM;
      if (stream && stream.active) {
        console.log("📺 Re-attaching stream after update");
        this.attachStreamToStreaming(stream);
        if (!this.durationStarted) {
          this.startDurationCounter();
        }
      }
    }
    this.attachEventHandlers();
  },
  destroyed() {
    console.log("🔚 [CameraManager] Hook destroyed");
    if (this.durationInterval) {
      clearInterval(this.durationInterval);
      this.durationInterval = null;
    }
    console.log("ℹ️ Hook destroyed, cleanup handled by End Stream button");
  },
};
export default CameraManager;
