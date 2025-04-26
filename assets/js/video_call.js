const VideoCallHook = {
  mounted() {
    this.localStream = null;
    this.isCalling = false;
    this.retryCount = 0;
    this.maxRetries = 3;
    this.retryDelay = 1000;
    this.canvasContext = null;
    this.renderLoopId = null;

    this.setupDiagnosticLogging();
    this.setupCanvas();
    this.setupEventListeners();
    this.initializeCamera();
  },

  setupDiagnosticLogging() {
    const diagnosticDiv = document.createElement('div');
    diagnosticDiv.id = 'video-call-diagnostic';
    diagnosticDiv.style.cssText = 'max-height: 200px; overflow-y: auto; background: #f8f8f8; border: 1px solid #ddd; padding: 10px; margin: 10px 0; font-family: monospace; font-size: 12px;';
    this.el.insertBefore(diagnosticDiv, this.el.firstChild);
    this.diagnosticDiv = diagnosticDiv;
  },

  log(message, data = {}) {
    const timestamp = new Date().toLocaleTimeString();
    const logEntry = document.createElement('div');
    logEntry.textContent = `[${timestamp}] ${message}${data ? ': ' + JSON.stringify(data) : ''}`;
    this.diagnosticDiv.appendChild(logEntry);
    this.diagnosticDiv.scrollTop = this.diagnosticDiv.scrollHeight;
    console.log(message, data);
  },

  setupCanvas() {
    let canvas = document.getElementById('local-canvas');
    if (!canvas) {
      canvas = document.createElement('canvas');
      canvas.id = 'local-canvas';
      canvas.style.cssText = 'width: 100%; height: 100%; object-fit: cover; display: block;';
      
      const localVideo = document.getElementById('local-video');
      if (localVideo && localVideo.parentElement) {
        localVideo.parentElement.replaceChild(canvas, localVideo);
      } else {
        this.el.appendChild(canvas);
      }
    }

    this.canvas = canvas;
    this.canvasContext = canvas.getContext('2d');

    this.canvas.width = 1280;
    this.canvas.height = 720;
  },

  setupEventListeners() {
    this.handleEvent("start-video-call", ({ call_id }) => {
      this.log("Starting video call", { call_id });
      if (this.isCalling) return;
      this.startVideoCall();
    });

    this.handleEvent("accept-video-call", ({ call_id }) => {
      this.log("Accepting video call", { call_id });
      this.startVideoCall();
    });

    this.handleEvent("end-video-call", () => {
      this.log("Ending video call");
      this.endVideoCall();
    });

    this.el.addEventListener('click', () => this.initializeCamera());
  },

  async initializeCamera() {
    this.log("Initializing camera");

    if (this.localStream) {
      this.localStream.getTracks().forEach(track => track.stop());
      this.localStream = null;
    }

    const constraints = {
      video: {
        width: { ideal: 1280 },
        height: { ideal: 720 },
        facingMode: "user"
      },
      audio: false
    };

    try {
      const stream = await navigator.mediaDevices.getUserMedia(constraints);
      this.localStream = new MediaStream(stream.getVideoTracks());
      this.log("Camera access granted", {
        videoTracks: this.localStream.getVideoTracks().length
      });

      await this.startRendering();
      this.pushEvent("call-status-updated", { status: "connected" });
      this.retryCount = 0;
    } catch (error) {
      this.handleError(error);
      this.retryCount++;
      if (this.retryCount < this.maxRetries) {
        const delay = this.retryDelay * Math.pow(2, this.retryCount);
        this.log("Retrying camera initialization", { attempt: this.retryCount, delay });
        setTimeout(() => this.initializeCamera(), delay);
      } else {
        this.pushEvent("call-error", { message: "Failed to initialize camera after multiple attempts" });
      }
    }
  },

  async startRendering() {
    let video = document.createElement('video');
    video.id = 'hidden-video';
    video.style.display = 'none';
    document.body.appendChild(video);

    video.srcObject = this.localStream;
    video.muted = true;
    video.playsInline = true;

    try {
      await video.play();
      this.log("Hidden video playing", {
        videoWidth: video.videoWidth,
        videoHeight: video.videoHeight
      });

      if (video.videoWidth > 0 && video.videoHeight > 0) {
        this.canvas.width = video.videoWidth;
        this.canvas.height = video.videoHeight;
      }

      this.renderToCanvas(video);
    } catch (error) {
      this.log("Failed to play hidden video", { message: error.message });
      document.body.removeChild(video);
      throw error;
    }
  },

  renderToCanvas(video) {
    if (this.renderLoopId) cancelAnimationFrame(this.renderLoopId);

    const renderFrame = () => {
      if (!this.localStream || !this.canvasContext || !video) {
        this.log("Rendering stopped: missing stream, context, or video");
        return;
      }

      const tracksLive = this.localStream.getVideoTracks().every(track => track.readyState === 'live');
      const hasDimensions = video.videoWidth > 0 && video.videoHeight > 0;

      if (!tracksLive || !hasDimensions) {
        this.log("Stream or video invalid", { tracksLive, videoWidth: video.videoWidth });
        this.initializeCamera(); 
        return;
      }

  
      this.canvasContext.drawImage(video, 0, 0, this.canvas.width, this.canvas.height);
      this.renderLoopId = requestAnimationFrame(renderFrame);
    };

    this.renderLoopId = requestAnimationFrame(renderFrame);

    setInterval(() => {
      if (!this.localStream) return;
      const tracksLive = this.localStream.getVideoTracks().every(track => track.readyState === 'live');
      if (!tracksLive) {
        this.log("Stream unhealthy, reinitializing");
        this.initializeCamera();
      }
    }, 2000);
  },

  handleError(error) {
    let message = "Failed to initialize camera";
    switch (error.name) {
      case "NotAllowedError":
        message = "Camera access denied. Please allow camera access.";
        break;
      case "NotFoundError":
        message = "No camera found. Please connect a camera.";
        break;
      case "NotReadableError":
        message = "Camera in use by another application.";
        break;
    }
    this.pushEvent("call-error", { message });
    this.log("Error", { name: error.name, message: error.message });
  },

  async startVideoCall() {
    if (this.isCalling) return;
    this.isCalling = true;

    this.log("Starting self video call");
    try {
      await this.initializeCamera();
    } finally {
      this.isCalling = false;
    }
  },

  endVideoCall() {
    this.log("Ending video call");

    if (this.localStream) {
      this.localStream.getTracks().forEach(track => track.stop());
      this.localStream = null;
    }

    const video = document.getElementById('hidden-video');
    if (video) {
      video.srcObject = null;
      document.body.removeChild(video);
    }

    if (this.canvasContext) {
      this.canvasContext.clearRect(0, 0, this.canvas.width, this.canvas.height);
    }

    if (this.renderLoopId) {
      cancelAnimationFrame(this.renderLoopId);
      this.renderLoopId = null;
    }

    this.isCalling = false;
    this.retryCount = 0;
    this.log("Video call ended");
  }
};

export default VideoCallHook;