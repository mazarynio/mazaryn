import CameraManager from "./camera_manager.js";

const LivestreamPlayer = {
  mounted() {
    const video = this.el;
    const streamId = video.dataset.streamId;
    const isLive = video.dataset.isLive === "true";

    this.initializePlayer();

    if (isLive) {
      this.startLivePolling();
    }

    this.attachEventListeners();
  },

  initializePlayer() {
    const video = this.el;
    const hlsUrl = video.dataset.hlsUrl;

    if (Hls.isSupported() && hlsUrl) {
      this.hls = new Hls({
        enableWorker: true,
        lowLatencyMode: true,
        backBufferLength: 90,
        maxBufferLength: 30,
        maxMaxBufferLength: 60,
        liveSyncDuration: 3,
        liveMaxLatencyDuration: 5,
        liveDurationInfinity: true,
      });

      this.hls.loadSource(hlsUrl);
      this.hls.attachMedia(video);

      this.hls.on(Hls.Events.MANIFEST_PARSED, () => {
        this.hideLoading();
        video.play().catch((e) => console.log("Autoplay prevented"));
      });

      this.hls.on(Hls.Events.ERROR, (event, data) => {
        if (data.fatal) {
          switch (data.type) {
            case Hls.ErrorTypes.NETWORK_ERROR:
              this.hls.startLoad();
              break;
            case Hls.ErrorTypes.MEDIA_ERROR:
              this.hls.recoverMediaError();
              break;
            default:
              this.showError("Stream error occurred");
              break;
          }
        }
      });
    } else if (video.canPlayType("application/vnd.apple.mpegurl")) {
      video.src = hlsUrl;
      video.addEventListener("loadedmetadata", () => {
        this.hideLoading();
      });
    }
  },

  attachEventListeners() {
    const video = this.el;
    const streamId = video.dataset.streamId;

    video.addEventListener("play", () => {
      this.pushEvent("stream_play", { stream_id: streamId });
    });

    video.addEventListener("pause", () => {
      this.pushEvent("stream_pause", { stream_id: streamId });
    });

    video.addEventListener("waiting", () => {
      this.showLoading();
    });

    video.addEventListener("playing", () => {
      this.hideLoading();
    });

    video.addEventListener("error", () => {
      this.showError("Failed to load stream");
    });
  },

  startLivePolling() {
    this.pollInterval = setInterval(() => {
      const streamId = this.el.dataset.streamId;
      this.pushEvent("check_stream_status", { stream_id: streamId });
    }, 5000);
  },

  showLoading() {
    const container = this.el.closest(".livestream-player-container");
    const loader = container?.querySelector("#stream-loading");
    if (loader) loader.style.display = "flex";
  },

  hideLoading() {
    const container = this.el.closest(".livestream-player-container");
    const loader = container?.querySelector("#stream-loading");
    if (loader) loader.style.display = "none";
  },

  showError(message) {
    const container = this.el.closest(".livestream-player-container");
    const error = container?.querySelector("#stream-error");
    if (error) {
      error.textContent = message;
      error.style.display = "block";
    }
  },

  destroyed() {
    if (this.hls) {
      this.hls.destroy();
    }
    if (this.pollInterval) {
      clearInterval(this.pollInterval);
    }
  },
};

export const LiveChat = {
  mounted() {
    this.scrollToBottom();

    this.handleEvent("new_chat_message", () => {
      setTimeout(() => this.scrollToBottom(), 100);
    });
  },

  updated() {
    this.scrollToBottom();
  },

  scrollToBottom() {
    const chatContainer = this.el.querySelector(".chat-messages");
    if (chatContainer) {
      chatContainer.scrollTop = chatContainer.scrollHeight;
    }
  },
};

export const StreamControls = {
  mounted() {
    const streamId = this.el.dataset.streamId;
    this.isStreaming = this.el.dataset.isStreaming === "true";

    this.attachControls();

    if (this.isStreaming) {
      this.startStatusPolling();
    }
  },

  attachControls() {
    const endButton = this.el.querySelector("#end-stream-btn");
    const startButton = this.el.querySelector("#start-stream-btn");

    if (endButton) {
      endButton.addEventListener("click", () => {
        this.showEndConfirmation();
      });
    }

    if (startButton) {
      startButton.addEventListener("click", () => {
        this.startStream();
      });
    }
  },

  showEndConfirmation() {
    const modal = document.querySelector("#end-stream-modal");
    if (modal) {
      modal.classList.remove("hidden");
    }
  },

  startStream() {
    const streamId = this.el.dataset.streamId;
    this.pushEvent("start_stream", { stream_id: streamId });
  },

  startStatusPolling() {
    this.statusInterval = setInterval(() => {
      const streamId = this.el.dataset.streamId;
      this.pushEvent("poll_stream_status", { stream_id: streamId });
    }, 3000);
  },

  destroyed() {
    if (this.statusInterval) {
      clearInterval(this.statusInterval);
    }
  },
};

export const ViewerCounter = {
  mounted() {
    this.count = parseInt(this.el.dataset.count) || 0;
    this.updateDisplay();

    this.handleEvent("update_viewers", ({ count }) => {
      this.animateCount(count);
    });
  },

  animateCount(newCount) {
    const oldCount = this.count;
    const duration = 500;
    const steps = 20;
    const increment = (newCount - oldCount) / steps;
    let currentStep = 0;

    const interval = setInterval(() => {
      currentStep++;
      this.count = Math.round(oldCount + increment * currentStep);
      this.updateDisplay();

      if (currentStep >= steps) {
        this.count = newCount;
        this.updateDisplay();
        clearInterval(interval);
      }
    }, duration / steps);
  },

  updateDisplay() {
    this.el.textContent = this.formatCount(this.count);
  },

  formatCount(count) {
    if (count >= 1000000) {
      return (count / 1000000).toFixed(1) + "M";
    } else if (count >= 1000) {
      return (count / 1000).toFixed(1) + "K";
    }
    return count.toString();
  },
};

export const ReactionButton = {
  mounted() {
    this.attachListeners();
  },

  attachListeners() {
    const buttons = this.el.querySelectorAll(".reaction-btn");

    buttons.forEach((button) => {
      button.addEventListener("click", (e) => {
        const reactionType = e.currentTarget.dataset.reactionType;
        this.sendReaction(reactionType);
        this.animateReaction(e.currentTarget);
      });
    });
  },

  sendReaction(type) {
    const streamId = this.el.dataset.streamId;
    this.pushEvent("send_reaction", {
      stream_id: streamId,
      reaction_type: type,
    });
  },

  animateReaction(button) {
    button.classList.add("reaction-pulse");
    setTimeout(() => {
      button.classList.remove("reaction-pulse");
    }, 600);
  },
};

export const RTMPInfo = {
  mounted() {
    this.attachCopyListeners();
  },

  attachCopyListeners() {
    const copyButtons = this.el.querySelectorAll(".copy-btn");

    copyButtons.forEach((button) => {
      button.addEventListener("click", (e) => {
        const text = e.currentTarget.dataset.copyText;
        this.copyToClipboard(text, e.currentTarget);
      });
    });
  },

  copyToClipboard(text, button) {
    navigator.clipboard
      .writeText(text)
      .then(() => {
        const originalText = button.innerHTML;
        button.innerHTML =
          '<svg class="w-4 h-4" fill="currentColor" viewBox="0 0 20 20"><path d="M9 2a1 1 0 000 2h2a1 1 0 100-2H9z"></path><path d="M4 5a2 2 0 012-2 3 3 0 003 3h2a3 3 0 003-3 2 2 0 012 2v11a2 2 0 01-2 2H6a2 2 0 01-2-2V5zm3 4a1 1 0 000 2h.01a1 1 0 100-2H7zm3 0a1 1 0 000 2h3a1 1 0 100-2h-3zm-3 4a1 1 0 100 2h.01a1 1 0 100-2H7zm3 0a1 1 0 100 2h3a1 1 0 100-2h-3z"></path></svg>';
        button.classList.add("text-green-500");

        setTimeout(() => {
          button.innerHTML = originalText;
          button.classList.remove("text-green-500");
        }, 2000);
      })
      .catch((err) => {
        console.error("Failed to copy:", err);
      });
  },
};

const StreamingVideo = {
  mounted() {
    console.log("StreamingVideo hook mounted");
    const video = this.el;

    const tryAttachStream = () => {
      if (window.globalCameraStream) {
        console.log("Attaching camera stream to streaming video");
        video.srcObject = window.globalCameraStream;
        video.muted = false;
        video
          .play()
          .then(() => {
            console.log("Streaming video playing successfully");

            if (!window.streamDurationStarted) {
              window.streamDurationStarted = true;
              let seconds = 0;
              setInterval(() => {
                seconds++;
                const minutes = Math.floor(seconds / 60);
                const secs = seconds % 60;
                const durationEl = document.getElementById("stream-duration");
                if (durationEl) {
                  durationEl.textContent = `${minutes}:${String(secs).padStart(2, "0")}`;
                }
              }, 1000);
            }
          })
          .catch((e) => {
            console.error("Error playing streaming video:", e);
          });
      } else {
        console.log("No camera stream available yet");
      }
    };

    tryAttachStream();
    setTimeout(tryAttachStream, 200);
    setTimeout(tryAttachStream, 500);
    setTimeout(tryAttachStream, 1000);
    setTimeout(tryAttachStream, 2000);
  },

  updated() {
    console.log("StreamingVideo updated");
    const video = this.el;
    if (window.globalCameraStream && !video.srcObject) {
      console.log("Reattaching stream on update");
      video.srcObject = window.globalCameraStream;
      video.muted = false;
      video.play().catch((e) => console.error("Play error on update:", e));
    }
  },
};

export default {
  LivestreamPlayer,
  LiveChat,
  StreamControls,
  ViewerCounter,
  ReactionButton,
  RTMPInfo,
  CameraCapture: CameraManager,
  StreamingVideo,
};
