// We import the CSS which is extracted to its own file by esbuild.
// Remove this line if you add a your own CSS build pipeline (e.g postcss).

// If you want to use Phoenix channels, run `mix help phx.gen.channel`
// to get started and then uncomment the line below.
// import "./user_socket.js"

// You can include dependencies in two ways.
//
// The simplest option is to put them in assets/vendor and
// import them using relative paths:
//
//     import "../vendor/some-package.js"
//
// Alternatively, you can `npm install some-package --prefix assets` and import
// them using a path starting with the package name:
//
//     import "some-package"
//

// import Alpine
import Alpine from "alpinejs";
window.Alpine = Alpine;
Alpine.start();

import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";
import VideoCallHook from "./video_call";

let Hooks = {};
let csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

Hooks.EmojiPicker = {
  mounted() {
    console.log("EmojiPicker hook mounted");
    this.el.addEventListener("emoji-click", (event) => {
      console.log("Emoji clicked:", event.detail.unicode);
      const emoji = event.detail.unicode;

      const textarea = document.getElementById("message-input");
      if (textarea) {
        const start = textarea.selectionStart;
        const end = textarea.selectionEnd;
        const currentValue = textarea.value;

        const newValue =
          currentValue.substring(0, start) +
          emoji +
          currentValue.substring(end);
        textarea.value = newValue;

        const newCursorPos = start + emoji.length;
        textarea.setSelectionRange(newCursorPos, newCursorPos);

        textarea.focus();

        textarea.dispatchEvent(new Event("input", { bubbles: true }));

        this.pushEventTo(this.el, "insert-emoji", { emoji: emoji });
      } else {
        this.pushEventTo(this.el, "insert-emoji", { emoji: emoji });
      }
    });
  },

  updated() {
    console.log("EmojiPicker hook updated");
    if (!this._listenerAttached) {
      this.el.addEventListener("emoji-click", (event) => {
        console.log("Emoji clicked:", event.detail.unicode);
        const emoji = event.detail.unicode;

        const textarea = document.getElementById("message-input");
        if (textarea) {
          const start = textarea.selectionStart;
          const end = textarea.selectionEnd;
          const currentValue = textarea.value;

          const newValue =
            currentValue.substring(0, start) +
            emoji +
            currentValue.substring(end);
          textarea.value = newValue;

          const newCursorPos = start + emoji.length;
          textarea.setSelectionRange(newCursorPos, newCursorPos);

          textarea.focus();

          textarea.dispatchEvent(new Event("input", { bubbles: true }));

          this.pushEventTo(this.el, "insert-emoji", { emoji: emoji });
        } else {
          this.pushEventTo(this.el, "insert-emoji", { emoji: emoji });
        }
      });
      this._listenerAttached = true;
    }
  },

  destroyed() {
    this._listenerAttached = false;
  },
};

Hooks.EmojiHandler = {
  mounted() {
    this.initializeEmojiHandler();
  },

  updated() {
    this.initializeEmojiHandler();
  },

  initializeEmojiHandler() {
    const emojiData = JSON.parse(this.el.dataset.emojis);
    const emojiButton = this.el.querySelector("#emoji-button");
    const emojiPanel = this.el.querySelector("#emoji-panel");
    const emojiGrid = this.el.querySelector("#emoji-grid");
    const postContent = this.el.querySelector("#post-content");
    const charCount = this.el.querySelector("#char-count");
    const charProgress = this.el.querySelector("#char-progress");

    if (this.clickHandler) {
      document.removeEventListener("click", this.clickHandler);
    }
    if (this.keydownHandler) {
      document.removeEventListener("keydown", this.keydownHandler);
    }

    this.populateEmojis(emojiData, emojiGrid, postContent, emojiPanel);
    this.updateCharacterCount(postContent, charCount, charProgress);

    const showEmojiPanel = () => {
      emojiPanel.classList.remove("hidden");
      emojiPanel.style.animation = "fadeInUp 0.3s ease-out forwards";
    };

    const hideEmojiPanel = () => {
      emojiPanel.style.animation = "fadeOutDown 0.2s ease-in forwards";
      setTimeout(() => {
        emojiPanel.classList.add("hidden");
      }, 200);
    };

    if (emojiButton) {
      emojiButton.addEventListener("click", (e) => {
        e.stopPropagation();
        if (emojiPanel.classList.contains("hidden")) {
          showEmojiPanel();
        } else {
          hideEmojiPanel();
        }
      });
    }

    this.clickHandler = (e) => {
      const emojiContainer = this.el.querySelector("#emoji-container");
      if (emojiContainer && !emojiContainer.contains(e.target)) {
        hideEmojiPanel();
      }
    };
    document.addEventListener("click", this.clickHandler);

    this.keydownHandler = (e) => {
      if (e.key === "Escape") {
        hideEmojiPanel();
      }
    };
    document.addEventListener("keydown", this.keydownHandler);

    if (postContent) {
      postContent.addEventListener("input", () => {
        this.updateCharacterCount(postContent, charCount, charProgress);
      });
    }

    if (!document.querySelector("#emoji-animations")) {
      this.addAnimationStyles();
    }

    this.handleEvent("insert_emoji", ({ emoji }) => {
      this.insertEmoji(emoji, postContent, charCount, charProgress);
    });
  },

  populateEmojis(emojiData, emojiGrid, postContent, emojiPanel) {
    if (!emojiGrid) return;

    emojiGrid.innerHTML = "";
    emojiData.forEach(({ name, emoji }) => {
      const button = document.createElement("button");
      button.type = "button";
      button.className =
        "w-12 h-12 hover:bg-indigo-50 hover:scale-110 rounded-xl text-2xl transition-all duration-200 flex items-center justify-center group relative";
      button.innerHTML = emoji;
      button.title = name.replace(/_/g, " ");

      const tooltip = document.createElement("div");
      tooltip.className =
        "absolute -top-8 left-1/2 transform -translate-x-1/2 px-2 py-1 bg-gray-900 text-white text-xs rounded opacity-0 group-hover:opacity-100 transition-opacity duration-200 pointer-events-none whitespace-nowrap z-50";
      tooltip.textContent = name.replace(/_/g, " ");
      button.appendChild(tooltip);

      button.addEventListener("click", (e) => {
        e.preventDefault();
        this.pushEvent("select_emoji", { emoji: emoji });
        this.hideEmojiPanel(emojiPanel);
      });

      emojiGrid.appendChild(button);
    });
  },

  insertEmoji(emoji, postContent, charCount, charProgress) {
    if (!postContent) return;

    const start = postContent.selectionStart;
    const end = postContent.selectionEnd;
    const text = postContent.value;

    postContent.value = text.substring(0, start) + emoji + text.substring(end);
    postContent.focus();
    postContent.setSelectionRange(start + emoji.length, start + emoji.length);

    postContent.dispatchEvent(new Event("input", { bubbles: true }));
    this.updateCharacterCount(postContent, charCount, charProgress);
  },

  updateCharacterCount(postContent, charCount, charProgress) {
    if (!postContent || !charCount || !charProgress) return;

    const count = postContent.value.length;
    const maxChars = 280;
    const percentage = (count / maxChars) * 100;

    charCount.textContent = `${count} characters`;
    charProgress.style.width = `${Math.min(percentage, 100)}%`;

    if (count > maxChars) {
      charCount.classList.add("text-red-500");
      charProgress.classList.remove(
        "bg-gradient-to-r",
        "from-indigo-500",
        "to-purple-500",
      );
      charProgress.classList.add("bg-red-500");
    } else {
      charCount.classList.remove("text-red-500");
      charProgress.classList.remove("bg-red-500");
      charProgress.classList.add(
        "bg-gradient-to-r",
        "from-indigo-500",
        "to-purple-500",
      );
    }
  },

  hideEmojiPanel(emojiPanel) {
    if (!emojiPanel) return;

    emojiPanel.style.animation = "fadeOutDown 0.2s ease-in forwards";
    setTimeout(() => {
      emojiPanel.classList.add("hidden");
    }, 200);
  },

  addAnimationStyles() {
    const style = document.createElement("style");
    style.id = "emoji-animations";
    style.textContent = `
      @keyframes fadeInUp {
        from {
          opacity: 0;
          transform: translateY(10px) scale(0.95);
        }
        to {
          opacity: 1;
          transform: translateY(0) scale(1);
        }
      }

      @keyframes fadeOutDown {
        from {
          opacity: 1;
          transform: translateY(0) scale(1);
        }
        to {
          opacity: 0;
          transform: translateY(10px) scale(0.95);
        }
      }
    `;
    document.head.appendChild(style);
  },

  destroyed() {
    if (this.clickHandler) {
      document.removeEventListener("click", this.clickHandler);
    }
    if (this.keydownHandler) {
      document.removeEventListener("keydown", this.keydownHandler);
    }
  },
};

export default Hooks;

Hooks.Notifications = {
  mounted() {
    this.handleNotificationVisibility();
    this.el.addEventListener("click", () => {
      this.el.classList.add("hidden");
      this.pushEvent("mark_notifications_read", {});
    });

    this.handleEvent("new_notification", () => {
      this.el.classList.remove("hidden");
    });
  },

  updated() {
    this.handleNotificationVisibility();
  },

  handleNotificationVisibility() {
    if (window.location.pathname === "/en/notifications") {
      this.el.classList.add("hidden");
    } else {
    }
  },
};

Hooks.ChatNotifications = {
  mounted() {
    this.ensureBadgeExists();
    this.updateBadgeVisibility();

    this.handleEvent("new_chat_message", () => {
      let count = parseInt(this.badgeEl.getAttribute("data-count") || "0");
      this.badgeEl.setAttribute("data-count", count + 1);
      this.updateBadgeVisibility(true);
    });

    this.el.addEventListener("click", () => {
      this.badgeEl.setAttribute("data-count", "0");
      this.updateBadgeVisibility(false);
      this.pushEvent("mark_chat_messages_read", {});
    });
  },

  ensureBadgeExists() {
    this.badgeEl = this.el.querySelector(".chat-notification-badge");

    if (!this.badgeEl) {
      this.badgeEl = document.createElement("span");
      this.badgeEl.classList.add("chat-notification-badge");
      this.badgeEl.setAttribute("data-count", "0");

      this.badgeEl.style.position = "absolute";
      this.badgeEl.style.top = "-5px";
      this.badgeEl.style.right = "-5px";
      this.badgeEl.style.backgroundColor = "red";
      this.badgeEl.style.borderRadius = "50%";
      this.badgeEl.style.width = "10px";
      this.badgeEl.style.height = "10px";
      this.badgeEl.style.display = "none";
      this.el.style.position = "relative";
      this.el.appendChild(this.badgeEl);
    }
  },

  updateBadgeVisibility(forceShow = null) {
    const count = parseInt(this.badgeEl.getAttribute("data-count") || "0");

    if (forceShow === true || (forceShow === null && count > 0)) {
      this.badgeEl.style.display = "block";
      if (count > 1) {
        this.badgeEl.textContent = count;
        this.badgeEl.style.width = "16px";
        this.badgeEl.style.height = "16px";
        this.badgeEl.style.display = "flex";
        this.badgeEl.style.justifyContent = "center";
        this.badgeEl.style.alignItems = "center";
        this.badgeEl.style.fontSize = "10px";
        this.badgeEl.style.color = "white";
      } else {
        this.badgeEl.textContent = "";
        this.badgeEl.style.width = "10px";
        this.badgeEl.style.height = "10px";
      }
    } else {
      this.badgeEl.style.display = "none";
    }
  },
};

Hooks.VideoCall = VideoCallHook;

let liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
  dom: {
    onBeforeElUpdated(from, to) {
      if (from._x_dataStack) {
        window.Alpine.clone(from, to);
      }
    },
  },
  hooks: Hooks,
});

// Connect if there are any LiveViews on the page
liveSocket.connect();

// Show progress bar on live navigation and form submits
topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (_info) => topbar.show(300));
window.addEventListener("phx:page-loading-stop", (_info) => topbar.hide());

// Expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;
