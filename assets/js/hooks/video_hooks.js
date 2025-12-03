export const VideoPlayer = {
  mounted() {
    this.video = this.el;
    this.videoId = this.el.dataset.videoId;

    this.video.addEventListener("play", () => {
      this.pushEvent("video_play", { video_id: this.videoId });
    });

    this.video.addEventListener("pause", () => {
      this.pushEvent("video_pause", {
        video_id: this.videoId,
        current_time: this.video.currentTime,
      });
    });

    this.video.addEventListener("ended", () => {
      this.pushEvent("video_ended", { video_id: this.videoId });
    });

    this.video.addEventListener("timeupdate", () => {
      const percentage = (this.video.currentTime / this.video.duration) * 100;
      this.pushEvent("video_progress", {
        video_id: this.videoId,
        percentage: percentage,
        current_time: this.video.currentTime,
      });
    });
  },
};
