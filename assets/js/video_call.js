const VideoCallHook = {
  mounted() {
    this.localStream = null;
    this.isCalling = false;
    this.retryCount = 0;
    this.maxRetries = 3;
    this.retryDelay = 1000;
    this.canvasContext = null;
    this.renderLoopId = null;
    this.isShutdown = false; 

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
      this.isShutdown = false;
      this.startVideoCall();
    });
  
    this.handleEvent("accept-video-call", ({ call_id }) => {
      this.log("Accepting video call", { call_id });
      this.isShutdown = false;
      this.startVideoCall();
    });
  
    this.handleEvent("end-video-call", () => {
      this.log("End video call event received from server");
      if (this.isShutdown) {
        this.log("Already shutdown, ignoring end-video-call event");
        return;
      }
      
      this.isShutdown = true;
      
      this.forceHideVideoContainer();
      
      this.endVideoCall();
      
      this.pushEvent("call-status-updated", { status: "disconnected" });
    });

    document.querySelectorAll('button[phx-click="end-video-call"]').forEach(btn => {
      btn.addEventListener('click', (e) => {
        e.preventDefault();
        e.stopPropagation();
        this.log("Close button clicked directly");
        if (this.isShutdown) return;
        
        this.isShutdown = true;
        this.forceHideVideoContainer();
        this.endVideoCall();
        this.pushEvent("call-status-updated", { status: "disconnected" });
      });
    });

    this.el.addEventListener('click', () => {
      if (!this.isShutdown) {
        this.initializeCamera();
      }
    });
  },
  
  forceHideVideoContainer() {
    const container = document.getElementById('video-call-container');
    if (container) {
      this.log("Forcefully hiding video container");
      container.style.cssText = 'display: none !important; visibility: hidden !important; opacity: 0 !important;';
      container.setAttribute('aria-hidden', 'true');
    }
  },

  async initializeCamera() {
    if (this.isShutdown) {
      this.log("System is shutdown, not initializing camera");
      return;
    }
    
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
      
      if (this.isShutdown) {
        this.log("Shutdown occurred during camera initialization");
        stream.getTracks().forEach(track => track.stop());
        return;
      }
      
      this.localStream = new MediaStream(stream.getVideoTracks());
      this.log("Camera access granted", {
        videoTracks: this.localStream.getVideoTracks().length
      });

      await this.startRendering();
      
      if (!this.isShutdown) {
        this.pushEvent("call-status-updated", { status: "connected" });
        this.retryCount = 0;
      }
    } catch (error) {
      if (this.isShutdown) return;
      
      this.handleError(error);
      this.retryCount++;
      if (this.retryCount < this.maxRetries) {
        const delay = this.retryDelay * Math.pow(2, this.retryCount);
        this.log("Retrying camera initialization", { attempt: this.retryCount, delay });
        setTimeout(() => {
          if (!this.isShutdown) {
            this.initializeCamera();
          }
        }, delay);
      } else {
        this.pushEvent("call-error", { message: "Failed to initialize camera after multiple attempts" });
      }
    }
  },

  async startRendering() {
    if (this.isShutdown) return;
    
    let video = document.createElement('video');
    video.id = 'hidden-video';
    video.style.display = 'none';
    document.body.appendChild(video);

    video.srcObject = this.localStream;
    video.muted = true;
    video.playsInline = true;

    try {
      await video.play();
      
      if (this.isShutdown) {
        document.body.removeChild(video);
        return;
      }
      
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
      if (document.body.contains(video)) {
        document.body.removeChild(video);
      }
      throw error;
    }
  },

  renderToCanvas(video) {
    if (this.renderLoopId) cancelAnimationFrame(this.renderLoopId);

    const renderFrame = () => {
      if (this.isShutdown || !this.localStream || !this.canvasContext || !video) {
        this.log("Rendering stopped: shutdown or missing resources");
        return;
      }

      const tracksLive = this.localStream.getVideoTracks().every(track => track.readyState === 'live');
      const hasDimensions = video.videoWidth > 0 && video.videoHeight > 0;

      if (!tracksLive || !hasDimensions) {
        this.log("Stream or video invalid", { tracksLive, videoWidth: video.videoWidth });
        if (!this.isShutdown) {
          this.initializeCamera();
        }
        return;
      }

      this.canvasContext.drawImage(video, 0, 0, this.canvas.width, this.canvas.height);
      
      if (!this.isShutdown) {
        this.renderLoopId = requestAnimationFrame(renderFrame);
      }
    };

    this.renderLoopId = requestAnimationFrame(renderFrame);

    this.healthCheckInterval = setInterval(() => {
      if (this.isShutdown) {
        clearInterval(this.healthCheckInterval);
        return;
      }
      
      if (!this.localStream) return;
      
      const tracksLive = this.localStream.getVideoTracks().every(track => track.readyState === 'live');
      if (!tracksLive && !this.isShutdown) {
        this.log("Stream unhealthy, reinitializing");
        this.initializeCamera();
      }
    }, 2000);
  },

  handleError(error) {
    if (this.isShutdown) return;
    
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
    if (this.isCalling || this.isShutdown) return;
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
    
    if (this.healthCheckInterval) {
      clearInterval(this.healthCheckInterval);
      this.healthCheckInterval = null;
    }

    if (this.localStream) {
      this.localStream.getTracks().forEach(track => track.stop());
      this.localStream = null;
    }

    const video = document.getElementById('hidden-video');
    if (video) {
      if (video.srcObject) {
        video.srcObject = null;
      }
      if (document.body.contains(video)) {
        document.body.removeChild(video);
      }
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
  },
  
  destroyed() {
    this.isShutdown = true;
    this.endVideoCall();
  }
};

export default VideoCallHook;