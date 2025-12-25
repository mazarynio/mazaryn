import CameraManager from "./camera_manager.js";

const LivestreamPlayer = {
  mounted() {
    console.log("=" + "=".repeat(80));
    console.log("🎬 [PLAYER] LivestreamPlayer mounted");

    const video = this.el;
    const streamId = video.dataset.streamId;
    const isLive = video.dataset.isLive === "true";
    const hlsUrl = video.dataset.hlsUrl;

    console.log("🎬 [PLAYER] Configuration:");
    console.log("🎬 [PLAYER]   - Stream ID:", streamId);
    console.log("🎬 [PLAYER]   - Is Live:", isLive);
    console.log("🎬 [PLAYER]   - HLS URL:", hlsUrl);
    console.log("🎬 [PLAYER]   - Video element:", video);

    if (!hlsUrl) {
      console.error("❌ [PLAYER] No HLS URL provided!");
      console.error("❌ [PLAYER] Dataset:", video.dataset);
      this.showError("No video source available");
      console.log("=" + "=".repeat(80));
      return;
    }

    if (isLive) {
      console.log("🎬 [PLAYER] Initializing HLS player for LIVE stream");
      this.initializeHLSPlayer(video, hlsUrl);
      this.startLivePolling();
    } else {
      console.log("🎬 [PLAYER] Initializing VOD player for ENDED stream");
      this.initializeVODPlayer(video, hlsUrl);
    }

    this.attachEventListeners();
    console.log("=" + "=".repeat(80));
  },

  initializeHLSPlayer(video, hlsUrl) {
    console.log("🎬 [PLAYER] initializeHLSPlayer called");
    console.log("🎬 [PLAYER]   - URL:", hlsUrl);

    if (Hls.isSupported()) {
      console.log("✅ [PLAYER] HLS.js is supported");
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
        console.log("✅ [PLAYER] HLS manifest parsed");
        this.hideLoading();
        video
          .play()
          .catch((e) => console.log("ℹ️ [PLAYER] Autoplay prevented:", e));
      });
      this.hls.on(Hls.Events.ERROR, (event, data) => {
        console.error("❌ [PLAYER] HLS error:", data);
        if (data.fatal) {
          switch (data.type) {
            case Hls.ErrorTypes.NETWORK_ERROR:
              console.log("🔄 [PLAYER] Network error, restarting...");
              this.hls.startLoad();
              break;
            case Hls.ErrorTypes.MEDIA_ERROR:
              console.log("🔄 [PLAYER] Media error, recovering...");
              this.hls.recoverMediaError();
              break;
            default:
              console.error("❌ [PLAYER] Fatal error, cannot recover");
              this.showError("Stream error occurred");
              break;
          }
        }
      });
    } else if (video.canPlayType("application/vnd.apple.mpegurl")) {
      console.log("✅ [PLAYER] Native HLS support (Safari)");
      video.src = hlsUrl;
      video.addEventListener("loadedmetadata", () => {
        console.log("✅ [PLAYER] Native HLS metadata loaded");
        this.hideLoading();
      });
    } else {
      console.error("❌ [PLAYER] HLS not supported on this browser");
      this.showError("HLS not supported on this browser");
    }
  },

  initializeVODPlayer(video, videoUrl) {
    console.log("🎬 [PLAYER] initializeVODPlayer called");
    console.log("🎬 [PLAYER]   - URL:", videoUrl);
    console.log(
      "🎬 [PLAYER]   - URL starts with https://ipfs.io:",
      videoUrl.startsWith("https://ipfs.io"),
    );

    video.src = videoUrl;
    video.controls = true;

    video.addEventListener("loadedmetadata", () => {
      console.log("✅ [PLAYER] VOD metadata loaded");
      console.log("✅ [PLAYER]   - Duration:", video.duration);
      console.log("✅ [PLAYER]   - Video width:", video.videoWidth);
      console.log("✅ [PLAYER]   - Video height:", video.videoHeight);
      this.hideLoading();
    });

    video.addEventListener("error", (e) => {
      console.error("❌ [PLAYER] Video error:", e);
      console.error("❌ [PLAYER]   - Error code:", video.error?.code);
      console.error("❌ [PLAYER]   - Error message:", video.error?.message);
      this.showError("Failed to load recording");
    });

    video.addEventListener("canplay", () => {
      console.log("✅ [PLAYER] Video can start playing");
    });

    video.addEventListener("playing", () => {
      console.log("✅ [PLAYER] Video is now playing");
    });
  },

  attachEventListeners() {
    const video = this.el;
    const streamId = video.dataset.streamId;
    video.addEventListener("play", () =>
      this.pushEvent("stream_play", { stream_id: streamId }),
    );
    video.addEventListener("pause", () =>
      this.pushEvent("stream_pause", { stream_id: streamId }),
    );
    video.addEventListener("waiting", () => this.showLoading());
    video.addEventListener("playing", () => this.hideLoading());
    video.addEventListener("error", () =>
      this.showError("Failed to load stream"),
    );
  },

  startLivePolling() {
    this.pollInterval = setInterval(() => {
      const streamId = this.el.dataset.streamId;
      this.pushEvent("check_stream_status", { stream_id: streamId });
    }, 5000);
  },

  showLoading() {
    const container = this.el.closest("#player-container");
    const loader = container?.querySelector("#stream-loading");
    if (loader) loader.style.display = "flex";
  },

  hideLoading() {
    const container = this.el.closest("#player-container");
    const loader = container?.querySelector("#stream-loading");
    if (loader) loader.style.display = "none";
  },

  showError(message) {
    const container = this.el.closest("#player-container");
    const error = container?.querySelector("#stream-error");
    if (error) {
      error.textContent = message;
      error.style.display = "block";
    }
  },

  destroyed() {
    if (this.hls) this.hls.destroy();
    if (this.pollInterval) clearInterval(this.pollInterval);
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
    const chatContainer =
      this.el.querySelector("#chat-messages-container") || this.el;
    if (chatContainer) chatContainer.scrollTop = chatContainer.scrollHeight;
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
    if (endButton) {
      endButton.addEventListener("click", () => this.showEndConfirmation());
    }
  },
  showEndConfirmation() {
    const modal = document.querySelector("#end-stream-modal");
    if (modal) modal.classList.remove("hidden");
  },
  startStatusPolling() {
    this.statusInterval = setInterval(() => {
      const streamId = this.el.dataset.streamId;
      this.pushEvent("poll_stream_status", { stream_id: streamId });
    }, 3000);
  },
  destroyed() {
    if (this.statusInterval) clearInterval(this.statusInterval);
  },
};

export const ViewerCounter = {
  mounted() {
    this.count = parseInt(this.el.dataset.count) || 0;
    this.updateDisplay();
    this.handleEvent("update_viewers", ({ count }) => this.animateCount(count));
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
    if (count >= 1000000) return (count / 1000000).toFixed(1) + "M";
    if (count >= 1000) return (count / 1000).toFixed(1) + "K";
    return count.toString();
  },
};

export const ReactionButton = {
  mounted() {
    this.attachListeners();
  },
  attachListeners() {
    const buttons = this.el.querySelectorAll(
      "button[phx-click='send_reaction']",
    );
    buttons.forEach((button) => {
      button.addEventListener("click", (e) => {
        this.animateReaction(e.currentTarget);
      });
    });
  },
  animateReaction(button) {
    button.classList.add("scale-125");
    setTimeout(() => button.classList.remove("scale-125"), 200);
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
      .catch((err) => console.error("Failed to copy:", err));
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
};
