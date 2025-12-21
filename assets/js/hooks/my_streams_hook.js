const MyStreams = {
  mounted() {
    console.log("📊 [MyStreams] Hook mounted");

    this.handleEvent("download_file", ({ url, filename }) => {
      console.log("⬇️ Download requested:", { url, filename });
      this.downloadFile(url, filename);
    });
  },

  downloadFile(url, filename) {
    console.log("📥 Starting download:", url);

    const link = document.createElement("a");
    link.href = url;
    link.download = filename;
    link.target = "_blank";
    link.rel = "noopener noreferrer";

    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);

    console.log("✅ Download initiated");

    this.showNotification(
      "Download started! Check your browser downloads.",
      "success",
    );
  },

  showNotification(message, type = "info") {
    const notification = document.createElement("div");
    notification.className = `fixed bottom-4 right-4 px-6 py-4 rounded-xl shadow-2xl z-50 transform transition-all duration-300 ${
      type === "success"
        ? "bg-green-600"
        : type === "error"
          ? "bg-red-600"
          : "bg-blue-600"
    }`;

    notification.innerHTML = `
      <div class="flex items-center gap-3 text-white">
        <svg class="w-5 h-5" fill="currentColor" viewBox="0 0 20 20">
          ${
            type === "success"
              ? '<path fill-rule="evenodd" d="M10 18a8 8 0 100-16 8 8 0 000 16zm3.707-9.293a1 1 0 00-1.414-1.414L9 10.586 7.707 9.293a1 1 0 00-1.414 1.414l2 2a1 1 0 001.414 0l4-4z" clip-rule="evenodd"/>'
              : '<path fill-rule="evenodd" d="M18 10a8 8 0 11-16 0 8 8 0 0116 0zm-7-4a1 1 0 11-2 0 1 1 0 012 0zM9 9a1 1 0 000 2v3a1 1 0 001 1h1a1 1 0 100-2v-3a1 1 0 00-1-1H9z" clip-rule="evenodd"/>'
          }
        </svg>
        <span class="font-semibold">${message}</span>
      </div>
    `;

    document.body.appendChild(notification);

    setTimeout(() => {
      notification.style.transform = "translateY(0)";
    }, 10);

    setTimeout(() => {
      notification.style.transform = "translateY(100px)";
      notification.style.opacity = "0";
      setTimeout(() => {
        document.body.removeChild(notification);
      }, 300);
    }, 4000);
  },

  updated() {
    console.log("🔄 [MyStreams] Hook updated");
  },
};

export default MyStreams;
