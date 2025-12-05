export const VideoPlayer = {
  mounted() {
    const video = this.el;
    const loadingIndicator = document.getElementById("video-loading");
    const videoId = video.dataset.videoId;
    let watchTimeCounter = 0;
    let lastReportedTime = 0;
    let controlsTimeout = null;

    this.createCustomControls();

    video.addEventListener("loadstart", () => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "flex";
      }
      this.showControls();
    });

    video.addEventListener("loadeddata", () => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "none";
      }
      this.updateDuration();
    });

    video.addEventListener("canplay", () => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "none";
      }
    });

    video.addEventListener("waiting", () => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "flex";
      }
    });

    video.addEventListener("playing", () => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "none";
      }
    });

    video.addEventListener("error", (e) => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "none";
      }
      console.error("Video load error:", e);

      const sources = video.querySelectorAll("source");
      const currentSrc = video.currentSrc;
      let foundCurrent = false;

      for (let source of sources) {
        if (foundCurrent && source.src !== currentSrc) {
          console.log("Trying fallback source:", source.src);
          video.src = source.src;
          video.load();
          break;
        }
        if (source.src === currentSrc) {
          foundCurrent = true;
        }
      }
    });

    video.addEventListener("play", () => {
      this.updatePlayPauseButton();
      this.pushEvent("video_play", { video_id: videoId });
    });

    video.addEventListener("pause", () => {
      this.updatePlayPauseButton();
      this.pushEvent("video_pause", {
        video_id: videoId,
        current_time: Math.floor(video.currentTime),
      });
    });

    video.addEventListener("ended", () => {
      if (watchTimeCounter > 0) {
        this.pushEvent("track_watch_time", {
          video_id: videoId,
          seconds: watchTimeCounter,
        });
      }
      this.pushEvent("video_ended", { video_id: videoId });
      watchTimeCounter = 0;
      lastReportedTime = 0;
      this.updatePlayPauseButton();
    });

    video.addEventListener("timeupdate", () => {
      const currentTime = Math.floor(video.currentTime);

      if (currentTime > lastReportedTime) {
        watchTimeCounter++;
        lastReportedTime = currentTime;

        if (watchTimeCounter % 10 === 0) {
          this.pushEvent("track_watch_time", {
            video_id: videoId,
            seconds: 10,
          });
          watchTimeCounter = 0;
        }
      }

      const percentage = (video.currentTime / video.duration) * 100;
      this.updateProgressBar(percentage);
      this.updateCurrentTime();

      this.pushEvent("video_progress", {
        video_id: videoId,
        percentage: percentage,
        current_time: currentTime,
      });
    });

    video.addEventListener("volumechange", () => {
      localStorage.setItem("video_volume", video.volume);
      localStorage.setItem("video_muted", video.muted);
      this.updateVolumeButton();
      this.updateVolumeSlider();
    });

    const savedVolume = localStorage.getItem("video_volume");
    const savedMuted = localStorage.getItem("video_muted");

    if (savedVolume !== null) {
      video.volume = parseFloat(savedVolume);
    }
    if (savedMuted !== null) {
      video.muted = savedMuted === "true";
    }

    const container = video.closest(".video-player-container");
    if (container) {
      container.addEventListener("mousemove", () => {
        this.showControls();
        this.resetControlsTimeout();
      });

      container.addEventListener("mouseleave", () => {
        if (!video.paused) {
          this.hideControls();
        }
      });
    }

    document.addEventListener("keydown", (e) => {
      if (this.isVideoFocused()) {
        this.handleKeyboard(e);
      }
    });

    video.addEventListener("dblclick", () => {
      this.toggleFullscreen();
    });

    video.addEventListener("click", (e) => {
      if (e.target === video) {
        this.togglePlayPause();
      }
    });

    this.updatePlayPauseButton();
    this.updateVolumeButton();
    this.updateVolumeSlider();
  },

  createCustomControls() {
    const video = this.el;
    const container = video.closest(".video-player-container");
    if (!container) return;

    const controlsHTML = `
      <div class="custom-video-controls">
        <div class="progress-bar-container">
          <div class="progress-bar">
            <div class="progress-bar-filled"></div>
            <div class="progress-bar-hover"></div>
            <div class="progress-bar-scrubber"></div>
          </div>
        </div>

        <div class="controls-bottom">
          <div class="controls-left">
            <button class="control-btn play-pause-btn" title="Play/Pause (Space)">
              <svg class="play-icon" viewBox="0 0 24 24" fill="currentColor">
                <path d="M8 5v14l11-7z"/>
              </svg>
              <svg class="pause-icon" style="display: none;" viewBox="0 0 24 24" fill="currentColor">
                <path d="M6 4h4v16H6V4zm8 0h4v16h-4V4z"/>
              </svg>
            </button>

            <button class="control-btn volume-btn" title="Mute/Unmute (M)">
              <svg class="volume-high-icon" viewBox="0 0 24 24" fill="currentColor">
                <path d="M3 9v6h4l5 5V4L7 9H3zm13.5 3c0-1.77-1.02-3.29-2.5-4.03v8.05c1.48-.73 2.5-2.25 2.5-4.02zM14 3.23v2.06c2.89.86 5 3.54 5 6.71s-2.11 5.85-5 6.71v2.06c4.01-.91 7-4.49 7-8.77s-2.99-7.86-7-8.77z"/>
              </svg>
              <svg class="volume-muted-icon" style="display: none;" viewBox="0 0 24 24" fill="currentColor">
                <path d="M16.5 12c0-1.77-1.02-3.29-2.5-4.03v2.21l2.45 2.45c.03-.2.05-.41.05-.63zm2.5 0c0 .94-.2 1.82-.54 2.64l1.51 1.51C20.63 14.91 21 13.5 21 12c0-4.28-2.99-7.86-7-8.77v2.06c2.89.86 5 3.54 5 6.71zM4.27 3L3 4.27 7.73 9H3v6h4l5 5v-6.73l4.25 4.25c-.67.52-1.42.93-2.25 1.18v2.06c1.38-.31 2.63-.95 3.69-1.81L19.73 21 21 19.73l-9-9L4.27 3zM12 4L9.91 6.09 12 8.18V4z"/>
              </svg>
            </button>

            <div class="volume-slider-container">
              <input type="range" class="volume-slider" min="0" max="100" value="100" />
            </div>

            <div class="time-display">
              <span class="current-time">0:00</span>
              <span class="time-separator">/</span>
              <span class="duration-time">0:00</span>
            </div>
          </div>

          <div class="controls-right">
            <button class="control-btn settings-btn" title="Settings">
              <svg viewBox="0 0 24 24" fill="currentColor">
                <path d="M19.14 12.94c.04-.3.06-.61.06-.94 0-.32-.02-.64-.07-.94l2.03-1.58c.18-.14.23-.41.12-.61l-1.92-3.32c-.12-.22-.37-.29-.59-.22l-2.39.96c-.5-.38-1.03-.7-1.62-.94l-.36-2.54c-.04-.24-.24-.41-.48-.41h-3.84c-.24 0-.43.17-.47.41l-.36 2.54c-.59.24-1.13.57-1.62.94l-2.39-.96c-.22-.08-.47 0-.59.22L2.74 8.87c-.12.21-.08.47.12.61l2.03 1.58c-.05.3-.09.63-.09.94s.02.64.07.94l-2.03 1.58c-.18.14-.23.41-.12.61l1.92 3.32c.12.22.37.29.59.22l2.39-.96c.5.38 1.03.7 1.62.94l.36 2.54c.05.24.24.41.48.41h3.84c.24 0 .44-.17.47-.41l.36-2.54c.59-.24 1.13-.56 1.62-.94l2.39.96c.22.08.47 0 .59-.22l1.92-3.32c.12-.22.07-.47-.12-.61l-2.01-1.58zM12 15.6c-1.98 0-3.6-1.62-3.6-3.6s1.62-3.6 3.6-3.6 3.6 1.62 3.6 3.6-1.62 3.6-3.6 3.6z"/>
              </svg>
            </button>

            <button class="control-btn quality-btn" title="Quality">
              <span class="quality-text">HD</span>
            </button>

            <button class="control-btn speed-btn" title="Playback Speed">
              <span class="speed-text">1x</span>
            </button>

            <button class="control-btn fullscreen-btn" title="Fullscreen (F)">
              <svg class="fullscreen-enter-icon" viewBox="0 0 24 24" fill="currentColor">
                <path d="M7 14H5v5h5v-2H7v-3zm-2-4h2V7h3V5H5v5zm12 7h-3v2h5v-5h-2v3zM14 5v2h3v3h2V5h-5z"/>
              </svg>
              <svg class="fullscreen-exit-icon" style="display: none;" viewBox="0 0 24 24" fill="currentColor">
                <path d="M5 16h3v3h2v-5H5v2zm3-8H5v2h5V5H8v3zm6 11h2v-3h3v-2h-5v5zm2-11V5h-2v5h5V8h-3z"/>
              </svg>
            </button>
          </div>
        </div>
      </div>
    `;

    container.insertAdjacentHTML("beforeend", controlsHTML);
    this.attachControlListeners();
  },

  attachControlListeners() {
    const video = this.el;
    const container = video.closest(".video-player-container");

    const playPauseBtn = container.querySelector(".play-pause-btn");
    playPauseBtn?.addEventListener("click", () => this.togglePlayPause());

    const volumeBtn = container.querySelector(".volume-btn");
    volumeBtn?.addEventListener("click", () => this.toggleMute());

    const volumeSlider = container.querySelector(".volume-slider");
    volumeSlider?.addEventListener("input", (e) => {
      video.volume = e.target.value / 100;
      video.muted = false;
    });

    const fullscreenBtn = container.querySelector(".fullscreen-btn");
    fullscreenBtn?.addEventListener("click", () => this.toggleFullscreen());

    const speedBtn = container.querySelector(".speed-btn");
    speedBtn?.addEventListener("click", () => this.cyclePlaybackSpeed());

    const progressBar = container.querySelector(".progress-bar");
    progressBar?.addEventListener("click", (e) => this.seekVideo(e));

    progressBar?.addEventListener("mousemove", (e) => {
      const rect = progressBar.getBoundingClientRect();
      const percent = ((e.clientX - rect.left) / rect.width) * 100;
      const hoverBar = container.querySelector(".progress-bar-hover");
      if (hoverBar) {
        hoverBar.style.width = percent + "%";
      }
    });

    progressBar?.addEventListener("mouseleave", () => {
      const hoverBar = container.querySelector(".progress-bar-hover");
      if (hoverBar) {
        hoverBar.style.width = "0%";
      }
    });
  },

  togglePlayPause() {
    const video = this.el;
    if (video.paused) {
      video.play();
    } else {
      video.pause();
    }
  },

  updatePlayPauseButton() {
    const video = this.el;
    const container = video.closest(".video-player-container");
    const playIcon = container?.querySelector(".play-icon");
    const pauseIcon = container?.querySelector(".pause-icon");

    if (video.paused) {
      playIcon.style.display = "block";
      pauseIcon.style.display = "none";
    } else {
      playIcon.style.display = "none";
      pauseIcon.style.display = "block";
    }
  },

  toggleMute() {
    const video = this.el;
    video.muted = !video.muted;
  },

  updateVolumeButton() {
    const video = this.el;
    const container = video.closest(".video-player-container");
    const volumeHighIcon = container?.querySelector(".volume-high-icon");
    const volumeMutedIcon = container?.querySelector(".volume-muted-icon");

    if (video.muted || video.volume === 0) {
      volumeHighIcon.style.display = "none";
      volumeMutedIcon.style.display = "block";
    } else {
      volumeHighIcon.style.display = "block";
      volumeMutedIcon.style.display = "none";
    }
  },

  updateVolumeSlider() {
    const video = this.el;
    const container = video.closest(".video-player-container");
    const volumeSlider = container?.querySelector(".volume-slider");

    if (volumeSlider) {
      volumeSlider.value = video.muted ? 0 : video.volume * 100;
    }
  },

  toggleFullscreen() {
    const container = this.el.closest(".video-player-container");

    if (!document.fullscreenElement) {
      container.requestFullscreen?.() ||
        container.webkitRequestFullscreen?.() ||
        container.mozRequestFullScreen?.();
      this.updateFullscreenButton(true);
    } else {
      document.exitFullscreen?.() ||
        document.webkitExitFullscreen?.() ||
        document.mozCancelFullScreen?.();
      this.updateFullscreenButton(false);
    }
  },

  updateFullscreenButton(isFullscreen) {
    const container = this.el.closest(".video-player-container");
    const enterIcon = container?.querySelector(".fullscreen-enter-icon");
    const exitIcon = container?.querySelector(".fullscreen-exit-icon");

    if (isFullscreen) {
      enterIcon.style.display = "none";
      exitIcon.style.display = "block";
    } else {
      enterIcon.style.display = "block";
      exitIcon.style.display = "none";
    }
  },

  cyclePlaybackSpeed() {
    const video = this.el;
    const speeds = [0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2];
    const currentIndex = speeds.indexOf(video.playbackRate);
    const nextIndex = (currentIndex + 1) % speeds.length;
    video.playbackRate = speeds[nextIndex];

    const container = video.closest(".video-player-container");
    const speedText = container?.querySelector(".speed-text");
    if (speedText) {
      speedText.textContent = speeds[nextIndex] + "x";
    }
  },

  seekVideo(e) {
    const video = this.el;
    const progressBar = e.currentTarget;
    const rect = progressBar.getBoundingClientRect();
    const percent = (e.clientX - rect.left) / rect.width;
    video.currentTime = percent * video.duration;
  },

  updateProgressBar(percentage) {
    const container = this.el.closest(".video-player-container");
    const progressFilled = container?.querySelector(".progress-bar-filled");
    const scrubber = container?.querySelector(".progress-bar-scrubber");

    if (progressFilled) {
      progressFilled.style.width = percentage + "%";
    }
    if (scrubber) {
      scrubber.style.left = percentage + "%";
    }
  },

  updateCurrentTime() {
    const video = this.el;
    const container = video.closest(".video-player-container");
    const currentTimeEl = container?.querySelector(".current-time");

    if (currentTimeEl) {
      currentTimeEl.textContent = this.formatTime(video.currentTime);
    }
  },

  updateDuration() {
    const video = this.el;
    const container = video.closest(".video-player-container");
    const durationEl = container?.querySelector(".duration-time");

    if (durationEl && !isNaN(video.duration)) {
      durationEl.textContent = this.formatTime(video.duration);
    }
  },

  formatTime(seconds) {
    if (isNaN(seconds)) return "0:00";

    const hours = Math.floor(seconds / 3600);
    const minutes = Math.floor((seconds % 3600) / 60);
    const secs = Math.floor(seconds % 60);

    if (hours > 0) {
      return `${hours}:${minutes.toString().padStart(2, "0")}:${secs.toString().padStart(2, "0")}`;
    }
    return `${minutes}:${secs.toString().padStart(2, "0")}`;
  },

  showControls() {
    const container = this.el.closest(".video-player-container");
    const controls = container?.querySelector(".custom-video-controls");
    if (controls) {
      controls.classList.add("visible");
    }
  },

  hideControls() {
    const container = this.el.closest(".video-player-container");
    const controls = container?.querySelector(".custom-video-controls");
    if (controls) {
      controls.classList.remove("visible");
    }
  },

  resetControlsTimeout() {
    const video = this.el;
    if (this.controlsTimeout) {
      clearTimeout(this.controlsTimeout);
    }

    if (!video.paused) {
      this.controlsTimeout = setTimeout(() => {
        this.hideControls();
      }, 3000);
    }
  },

  handleKeyboard(e) {
    const video = this.el;

    switch (e.key) {
      case " ":
      case "k":
        e.preventDefault();
        this.togglePlayPause();
        break;
      case "ArrowLeft":
        e.preventDefault();
        video.currentTime = Math.max(0, video.currentTime - 5);
        break;
      case "ArrowRight":
        e.preventDefault();
        video.currentTime = Math.min(video.duration, video.currentTime + 5);
        break;
      case "ArrowUp":
        e.preventDefault();
        video.volume = Math.min(1, video.volume + 0.1);
        break;
      case "ArrowDown":
        e.preventDefault();
        video.volume = Math.max(0, video.volume - 0.1);
        break;
      case "m":
        e.preventDefault();
        this.toggleMute();
        break;
      case "f":
        e.preventDefault();
        this.toggleFullscreen();
        break;
    }
  },

  isVideoFocused() {
    const container = this.el.closest(".video-player-container");
    return (
      container?.contains(document.activeElement) ||
      document.activeElement === document.body
    );
  },

  destroyed() {
    const video = this.el;
    if (this.controlsTimeout) {
      clearTimeout(this.controlsTimeout);
    }
    if (video) {
      video.pause();
      video.src = "";
      video.load();
    }
  },
};
