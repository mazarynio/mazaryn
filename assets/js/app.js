import "../css/app.css";
import { VideoPlayer } from "./hooks/video_hooks";
import LivestreamHooks from "./hooks/livestream_hooks";
import MyStreams from "./hooks/my_streams_hook";

import "phoenix_html";

import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";

import Alpine from "alpinejs";

window.Alpine = Alpine;
Alpine.start();

import VideoCallHook from "./video_call";
import NotebookHooks from "./hooks/notebook_hooks";

let csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

let Hooks = {
  CharCounter: {
    mounted() {
      const textarea = this.el;
      const componentId = textarea.id.replace("post-content-", "");

      const updateCount = () => {
        const count = textarea.value.length;
        const remaining = 280 - count;

        const circle = document.querySelector(`#char-circle-${componentId}`);
        const number = document.querySelector(`#char-number-${componentId}`);
        const submitBtn = document.querySelector(`#publish-btn-${componentId}`);

        if (circle && number) {
          const percentage = count / 280;
          const circumference = 2 * Math.PI * 20;
          const offset = circumference * (1 - percentage);

          circle.style.strokeDasharray = circumference;
          circle.style.strokeDashoffset = offset;
          number.textContent = remaining;

          circle.classList.remove(
            "text-red-500",
            "text-orange-500",
            "text-indigo-500",
          );
          number.classList.remove("text-red-500", "text-gray-600");

          if (remaining < 0) {
            circle.classList.add("text-red-500");
            number.classList.add("text-red-500");
            if (submitBtn) {
              submitBtn.disabled = true;
              submitBtn.classList.add("opacity-50", "cursor-not-allowed");
            }
          } else if (remaining < 20) {
            circle.classList.add("text-orange-500");
            number.classList.add("text-gray-600");
            if (submitBtn) {
              submitBtn.disabled = false;
              submitBtn.classList.remove("opacity-50", "cursor-not-allowed");
            }
          } else {
            circle.classList.add("text-indigo-500");
            number.classList.add("text-gray-600");
            if (submitBtn) {
              submitBtn.disabled = false;
              submitBtn.classList.remove("opacity-50", "cursor-not-allowed");
            }
          }
        }
      };

      textarea.addEventListener("input", updateCount);
      textarea.addEventListener("change", updateCount);

      setTimeout(updateCount, 100);

      this.handleEvent("insert_emoji", ({ emoji, component_id }) => {
        if (component_id === componentId) {
          const start = textarea.selectionStart || 0;
          const end = textarea.selectionEnd || 0;
          const text = textarea.value;

          const newLength = text.length - (end - start) + emoji.length;
          if (newLength <= 280) {
            textarea.value =
              text.substring(0, start) + emoji + text.substring(end);
            textarea.focus();

            const newPosition = start + emoji.length;
            textarea.setSelectionRange(newPosition, newPosition);

            textarea.dispatchEvent(new Event("input", { bubbles: true }));
            updateCount();
          }
        }
      });

      this.handleEvent("clear_textarea", ({ component_id }) => {
        if (component_id === componentId) {
          textarea.value = "";
          updateCount();
        }
      });

      this._updateCount = updateCount;
    },
    updated() {
      if (this._updateCount) {
        setTimeout(this._updateCount, 100);
      }
    },
    destroyed() {
      this._updateCount = null;
    },
  },

  MessageCharCounter: {
    mounted() {
      const textarea = this.el;
      const componentId = textarea.id.replace("message-input-", "");

      const updateCount = () => {
        const count = textarea.value.length;
        const maxChars = 280;
        const percentage = (count / maxChars) * 100;

        const charCount = document.getElementById(`char-count-${componentId}`);
        const charProgress = document.getElementById(
          `char-progress-${componentId}`,
        );

        if (charCount && charProgress) {
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
        }
      };

      textarea.addEventListener("input", updateCount);
      textarea.addEventListener("change", updateCount);

      setTimeout(updateCount, 100);

      this.handleEvent("insert_emoji", ({ emoji, component_id }) => {
        if (component_id === componentId) {
          const start = textarea.selectionStart || 0;
          const end = textarea.selectionEnd || 0;
          const text = textarea.value;

          const newLength = text.length - (end - start) + emoji.length;
          if (newLength <= 280) {
            textarea.value =
              text.substring(0, start) + emoji + text.substring(end);
            textarea.focus();

            const newPosition = start + emoji.length;
            textarea.setSelectionRange(newPosition, newPosition);

            textarea.dispatchEvent(new Event("input", { bubbles: true }));
            updateCount();
          }
        }
      });

      this.handleEvent("insert_sticker", ({ sticker, component_id }) => {
        if (component_id === componentId) {
          const start = textarea.selectionStart || 0;
          const end = textarea.selectionEnd || 0;
          const text = textarea.value;
          const stickerTag = `![sticker](${sticker})`;

          textarea.value =
            text.substring(0, start) + stickerTag + text.substring(end);
          textarea.focus();

          const newPosition = start + stickerTag.length;
          textarea.setSelectionRange(newPosition, newPosition);

          textarea.dispatchEvent(new Event("input", { bubbles: true }));
          updateCount();
        }
      });

      this._updateCount = updateCount;
    },
    updated() {
      if (this._updateCount) {
        setTimeout(this._updateCount, 100);
      }
    },
    destroyed() {
      this._updateCount = null;
    },
  },

  EmojiPicker: {
    mounted() {
      this.handleEmojiClick = (event) => {
        const emoji = event.detail.unicode;
        const textarea = document.getElementById("message-input");
        if (textarea) {
          const start = textarea.selectionStart;
          const end = textarea.selectionEnd;
          const value = textarea.value;
          textarea.value =
            value.substring(0, start) + emoji + value.substring(end);
          textarea.setSelectionRange(
            start + emoji.length,
            start + emoji.length,
          );
          textarea.focus();
          textarea.dispatchEvent(new Event("input", { bubbles: true }));
        }
        this.pushEvent("insert-emoji", { emoji });
      };
      this.el.addEventListener("emoji-click", this.handleEmojiClick);
    },
    destroyed() {
      if (this.handleEmojiClick) {
        this.el.removeEventListener("emoji-click", this.handleEmojiClick);
      }
    },
  },

  EmojiHandler: {
    mounted() {
      this.initEmojiHandler();
    },
    updated() {
      this.initEmojiHandler();
    },
    initEmojiHandler() {
      const { emojis: emojiDataStr } = this.el.dataset;
      if (!emojiDataStr) return;
      const emojiData = JSON.parse(emojiDataStr);
      const emojiButton = this.el.querySelector("#emoji-button");
      const emojiPanel = this.el.querySelector("#emoji-panel");
      const emojiGrid = this.el.querySelector("#emoji-grid");
      const postContent = this.el.querySelector("#post-content");
      const charCount = this.el.querySelector("#char-count");
      const charProgress = this.el.querySelector("#char-progress");

      if (this.clickHandler)
        document.removeEventListener("click", this.clickHandler);
      if (this.keydownHandler)
        document.removeEventListener("keydown", this.keydownHandler);
      if (this.inputHandler)
        postContent?.removeEventListener("input", this.inputHandler);

      this.populateEmojis(emojiData, emojiGrid);

      this.updateCharacterCount(postContent, charCount, charProgress);

      const togglePanel = (e) => {
        e.stopPropagation();
        if (emojiPanel.classList.contains("hidden")) {
          emojiPanel.classList.remove("hidden");
          emojiPanel.style.animation = "fadeInUp 0.3s ease-out forwards";
        } else {
          emojiPanel.style.animation = "fadeOutDown 0.2s ease-in forwards";
          setTimeout(() => emojiPanel.classList.add("hidden"), 200);
        }
      };
      if (emojiButton) emojiButton.addEventListener("click", togglePanel);

      this.clickHandler = (e) => {
        const container = this.el.querySelector("#emoji-container");
        if (container && !container.contains(e.target)) {
          emojiPanel.style.animation = "fadeOutDown 0.2s ease-in forwards";
          setTimeout(() => emojiPanel.classList.add("hidden"), 200);
        }
      };
      document.addEventListener("click", this.clickHandler);

      this.keydownHandler = (e) => {
        if (e.key === "Escape") {
          emojiPanel.style.animation = "fadeOutDown 0.2s ease-in forwards";
          setTimeout(() => emojiPanel.classList.add("hidden"), 200);
        }
      };
      document.addEventListener("keydown", this.keydownHandler);

      if (postContent) {
        this.inputHandler = () =>
          this.updateCharacterCount(postContent, charCount, charProgress);
        postContent.addEventListener("input", this.inputHandler);
      }

      if (!document.querySelector("#emoji-animations")) {
        const style = document.createElement("style");
        style.id = "emoji-animations";
        style.textContent = `
          @keyframes fadeInUp { from { opacity: 0; transform: translateY(10px) scale(0.95); } to { opacity: 1; transform: translateY(0) scale(1); } }
          @keyframes fadeOutDown { from { opacity: 1; transform: translateY(0) scale(1); } to { opacity: 0; transform: translateY(10px) scale(0.95); } }
        `;
        document.head.appendChild(style);
      }

      this.handleEvent("insert_emoji", ({ emoji }) => {
        this.insertEmoji(emoji, postContent, charCount, charProgress);
      });
    },
    populateEmojis(emojiData, emojiGrid) {
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
          this.pushEvent("select_emoji", { emoji });
          this.hideEmojiPanel(this.el.querySelector("#emoji-panel"));
        });
        emojiGrid.appendChild(button);
      });
    },
    insertEmoji(emoji, postContent, charCount, charProgress) {
      if (!postContent) return;
      const start = postContent.selectionStart;
      const end = postContent.selectionEnd;
      const text = postContent.value;
      postContent.value =
        text.substring(0, start) + emoji + text.substring(end);
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
      setTimeout(() => emojiPanel.classList.add("hidden"), 200);
    },
    destroyed() {
      if (this.clickHandler)
        document.removeEventListener("click", this.clickHandler);
      if (this.keydownHandler)
        document.removeEventListener("keydown", this.keydownHandler);
      if (this.inputHandler) {
        const postContent = this.el.querySelector("#post-content");
        postContent?.removeEventListener("input", this.inputHandler);
      }
    },
  },

  Notifications: {
    mounted() {
      this.updateVisibility();
      this.el.addEventListener("click", () => {
        this.el.classList.add("hidden");
        this.pushEvent("mark_notifications_read", {});
      });
      this.handleEvent("new_notification", () =>
        this.el.classList.remove("hidden"),
      );
    },
    updated() {
      this.updateVisibility();
    },
    updateVisibility() {
      this.el.classList.toggle(
        "hidden",
        window.location.pathname === "/en/notifications",
      );
    },
  },

  ChatNotifications: {
    mounted() {
      this.badgeEl =
        this.el.querySelector(".chat-notification-badge") || this.createBadge();
      this.updateBadgeVisibility();
      this.handleEvent("new_chat_message", () => {
        let count = parseInt(this.badgeEl.dataset.count || "0") + 1;
        this.badgeEl.dataset.count = count;
        this.updateBadgeVisibility(true);
      });
      this.el.addEventListener("click", () => {
        this.badgeEl.dataset.count = "0";
        this.updateBadgeVisibility(false);
        this.pushEvent("mark_chat_messages_read", {});
      });
    },
    createBadge() {
      const badge = document.createElement("span");
      badge.className = "chat-notification-badge";
      badge.dataset.count = "0";
      Object.assign(badge.style, {
        position: "absolute",
        top: "-5px",
        right: "-5px",
        backgroundColor: "red",
        borderRadius: "50%",
        width: "10px",
        height: "10px",
        display: "none",
      });
      this.el.style.position = "relative";
      this.el.appendChild(badge);
      return badge;
    },
    updateBadgeVisibility(forceShow = null) {
      const count = parseInt(this.badgeEl.dataset.count || "0");
      const show = forceShow ?? count > 0;
      this.badgeEl.style.display = show ? "block" : "none";
      if (show && count > 1) {
        this.badgeEl.textContent = count;
        Object.assign(this.badgeEl.style, {
          width: "16px",
          height: "16px",
          display: "flex",
          justifyContent: "center",
          alignItems: "center",
          fontSize: "10px",
          color: "white",
        });
      } else if (show) {
        this.badgeEl.textContent = "";
        this.badgeEl.style.width = "10px";
        this.badgeEl.style.height = "10px";
      }
    },
  },

  VideoCall: VideoCallHook,
  VideoPlayer: VideoPlayer,
  MyStreams: MyStreams,

  ...LivestreamHooks,

  ...NotebookHooks,
};

topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (_info) => topbar.show(300));
window.addEventListener("phx:page-loading-stop", (_info) => topbar.hide());

let liveSocket = new LiveSocket("/live", Socket, {
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
  dom: {
    onBeforeElUpdated(from, to) {
      if (from.__x) {
        window.Alpine.clone(from, to);
      }
    },
  },
});

liveSocket.connect();

window.liveSocket = liveSocket;
