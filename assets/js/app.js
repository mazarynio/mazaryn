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
// Add this before your liveSocket call.
window.Alpine = Alpine;
Alpine.start();

// Include phoenix_html to handle method=PUT/DELETE in forms and buttons.
import "phoenix_html";
// Establish Phoenix Socket and LiveView configuration.
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";

let Hooks = {};
let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content");

// Updated notification handling
Hooks.Notifications = {
  mounted() {
    this.handleNotificationVisibility();
    
    // Create or find the notification badge
    this.ensureNotificationBadgeExists();
    
    // Add click listener that will persist
    this.el.addEventListener("click", () => {
      // Mark notifications as read on the server
      this.pushEvent("mark_notifications_read", {});
      
      // Reset the notification count locally
      this.el.setAttribute("data-count", "0");
      this.updateNotificationBadge();
    });
    
    // Listen for new notifications from the server
    this.handleEvent("new_notification", (payload) => {
      // Update count if provided in payload
      if (payload && payload.count) {
        this.el.setAttribute("data-count", payload.count);
      } else {
        // Increment count if no specific count provided
        const currentCount = parseInt(this.el.getAttribute("data-count") || "0");
        this.el.setAttribute("data-count", currentCount + 1);
      }
      
      // Update the badge
      this.updateNotificationBadge();
      this.el.classList.remove("hidden");
    });
    
    // Get initial notification count
    this.pushEvent("get_notification_count", {}, (reply) => {
      if (reply && reply.count !== undefined) {
        this.el.setAttribute("data-count", reply.count);
        this.updateNotificationBadge();
      }
    });
  },
  
  updated() {
    // Re-check visibility on updates
    this.handleNotificationVisibility();
    this.updateNotificationBadge();
  },
  
  handleNotificationVisibility() {
    // Check if the user is on the notifications page
    if (window.location.pathname === "/en/notifications") {
      // Automatically mark notifications as read when on the notifications page
      this.pushEvent("mark_notifications_read", {});
      
      // Reset notification count
      this.el.setAttribute("data-count", "0");
      this.updateNotificationBadge();
      
      // Hide the notification icon on notifications page
      this.el.classList.add("hidden");
    } else {
      // On other pages, show only if there are unread notifications
      const count = parseInt(this.el.getAttribute("data-count") || "0");
      if (count > 0) {
        this.el.classList.remove("hidden");
      }
    }
  },
  
  ensureNotificationBadgeExists() {
    this.notificationBadge = this.el.querySelector(".notification-badge");
    
    if (!this.notificationBadge) {
      // Create the badge element
      this.notificationBadge = document.createElement("span");
      this.notificationBadge.classList.add("notification-badge");
      
      // Style the badge
      this.notificationBadge.style.position = "absolute";
      this.notificationBadge.style.top = "-5px";
      this.notificationBadge.style.right = "-5px";
      this.notificationBadge.style.backgroundColor = "red";
      this.notificationBadge.style.color = "white";
      this.notificationBadge.style.borderRadius = "50%";
      this.notificationBadge.style.minWidth = "16px";
      this.notificationBadge.style.height = "16px";
      this.notificationBadge.style.fontSize = "10px";
      this.notificationBadge.style.fontWeight = "bold";
      this.notificationBadge.style.display = "flex";
      this.notificationBadge.style.justifyContent = "center";
      this.notificationBadge.style.alignItems = "center";
      this.notificationBadge.style.padding = "0 4px";
      
      // Ensure parent has relative positioning
      this.el.style.position = "relative";
      this.el.appendChild(this.notificationBadge);
    }
  },
  
  updateNotificationBadge() {
    const count = parseInt(this.el.getAttribute("data-count") || "0");
    
    if (count > 0) {
      // Show badge with count
      this.notificationBadge.style.display = "flex";
      this.notificationBadge.textContent = count;
    } else {
      // Hide badge if no unread notifications
      this.notificationBadge.style.display = "none";
    }
  }
};

// Chat notification hook
Hooks.ChatNotifications = {
  mounted() {
    // Create badge element if it doesn't exist
    this.ensureBadgeExists();
    
    // Initially hide badge unless there are unread messages
    this.updateBadgeVisibility();
    
    // Get initial chat message count
    this.pushEvent("get_chat_message_count", {}, (reply) => {
      if (reply && reply.count !== undefined) {
        this.badgeEl.setAttribute("data-count", reply.count);
        this.updateBadgeVisibility();
      }
    });
    
    // Listen for new chat messages
    this.handleEvent("new_chat_message", (payload) => {
      // Update count if provided in payload
      if (payload && payload.count) {
        this.badgeEl.setAttribute("data-count", payload.count);
      } else {
        // Increment badge count
        let count = parseInt(this.badgeEl.getAttribute("data-count") || "0");
        this.badgeEl.setAttribute("data-count", count + 1);
      }
      
      // Show badge
      this.updateBadgeVisibility(true);
    });
    
    // Clear notification when clicking on chat icon
    this.el.addEventListener("click", () => {
      this.badgeEl.setAttribute("data-count", "0");
      this.updateBadgeVisibility(false);
      
      // Notify server that messages are being viewed
      this.pushEvent("mark_chat_messages_read", {});
    });
  },
  
  ensureBadgeExists() {
    // Check if badge already exists
    this.badgeEl = this.el.querySelector(".chat-notification-badge");
    
    if (!this.badgeEl) {
      // Create badge element
      this.badgeEl = document.createElement("span");
      this.badgeEl.classList.add("chat-notification-badge");
      this.badgeEl.setAttribute("data-count", "0");
      
      // Style the badge
      this.badgeEl.style.position = "absolute";
      this.badgeEl.style.top = "-5px";
      this.badgeEl.style.right = "-5px";
      this.badgeEl.style.backgroundColor = "red";
      this.badgeEl.style.borderRadius = "50%";
      this.badgeEl.style.minWidth = "10px";
      this.badgeEl.style.height = "10px";
      this.badgeEl.style.display = "none";
      
      // Ensure parent has relative positioning for proper badge placement
      this.el.style.position = "relative";
      
      // Add badge to chat icon
      this.el.appendChild(this.badgeEl);
    }
  },
  
  updateBadgeVisibility(forceShow = null) {
    const count = parseInt(this.badgeEl.getAttribute("data-count") || "0");
    
    if (forceShow === true || (forceShow === null && count > 0)) {
      this.badgeEl.style.display = "block";
      
      // Optionally display count number for multiple notifications
      if (count > 1) {
        this.badgeEl.textContent = count;
        // Adjust size to accommodate text
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
  
  updated() {
    // Check if we need to update badge on LiveView updates
    this.updateBadgeVisibility();
  }
};

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
