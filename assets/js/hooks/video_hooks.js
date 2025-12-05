export const VideoPlayer = {
  mounted() {
    const video = this.el;
    const loadingIndicator = document.getElementById("video-loading");
    const videoId = video.dataset.videoId;

    let watchTimeCounter = 0;
    let lastReportedTime = 0;

    video.addEventListener("loadstart", () => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "flex";
      }
    });

    video.addEventListener("loadeddata", () => {
      if (loadingIndicator) {
        loadingIndicator.style.display = "none";
      }
    });

    video.addEventListener("canplay", () => {
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
      this.pushEvent("video_play", { video_id: videoId });
    });

    video.addEventListener("pause", () => {
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
      this.pushEvent("video_progress", {
        video_id: videoId,
        percentage: percentage,
        current_time: currentTime,
      });
    });

    video.addEventListener("volumechange", () => {
      localStorage.setItem("video_volume", video.volume);
      localStorage.setItem("video_muted", video.muted);
    });

    const savedVolume = localStorage.getItem("video_volume");
    const savedMuted = localStorage.getItem("video_muted");

    if (savedVolume !== null) {
      video.volume = parseFloat(savedVolume);
    }

    if (savedMuted !== null) {
      video.muted = savedMuted === "true";
    }
  },

  destroyed() {
    const video = this.el;
    if (video) {
      video.pause();
      video.src = "";
      video.load();
    }
  },
};
