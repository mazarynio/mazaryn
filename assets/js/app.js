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

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html";
// Establish Phoenix Socket and LiveView configuration.
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";
import VideoCallHook from "./video_call";

let Hooks = {};
let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");

Hooks.Notifications = {
  mounted() {
    this.handleNotificationVisibility();
    this.el.addEventListener("click", () => {
      //we hide the notification here
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
    // Check if the user is on the notifications page
    if (window.location.pathname === "/en/notifications") {
      this.el.classList.add("hidden");
    } else {
    }
  }
};

//Here is our chat notification hook
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
      
      //Here we are Styling the badge
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
        // here i adjusted the size to accommodate text
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
  }
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

// Show progress bar on live navigation and form submits
topbar.config({barColors: {0: "#29d"}, shadowColor: "rgba(0, 0, 0, .3)"})
window.addEventListener("phx:page-loading-start", _info => topbar.show(300))
window.addEventListener("phx:page-loading-stop", _info => topbar.hide())

// connect if there are any LiveViews on the page
liveSocket.connect();

// expose liveSocket on window for web console debug logs and latency simulation:
// >> liveSocket.enableDebug()
// >> liveSocket.enableLatencySim(1000)  // enabled for duration of browser session
// >> liveSocket.disableLatencySim()
window.liveSocket = liveSocket;