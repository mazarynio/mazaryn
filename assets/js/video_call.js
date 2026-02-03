const VideoCallHook = {
  mounted() {
    console.log("🎥 ========================================");
    console.log("🎥 [MOUNTED] VideoCall hook mounted");
    console.log("🎥 [MOUNTED] Call ID:", this.el.dataset.callId);
    console.log("🎥 [MOUNTED] User ID:", this.el.dataset.userId);
    console.log(
      "🎥 [MOUNTED] Recipient ID:",
      this.el.dataset.currentRecipientId,
    );
    console.log("🎥 [MOUNTED] Is Caller:", this.el.dataset.isCaller);
    console.log("🎥 [MOUNTED] Username:", this.el.dataset.username);
    console.log(
      "🎥 [MOUNTED] Recipient Username:",
      this.el.dataset.currentRecipientUsername,
    );
    console.log("🎥 ========================================");

    this.localStream = null;
    this.remoteStream = null;
    this.peerConnection = null;
    this.websocket = null;
    this.isShutdown = false;
    this.isCaller = this.el.dataset.isCaller === "true";
    this.callId = this.el.dataset.callId;
    this.userId = this.el.dataset.userId;
    this.recipientId = this.el.dataset.currentRecipientId;
    this.reconnectAttempts = 0;
    this.maxReconnectAttempts = 3;
    this.reconnectDelay = 2000;
    this.iceCandidateQueue = [];
    this.isNegotiating = false;

    this.setupEventListeners();
  },

  setupEventListeners() {
    console.log("🎯 [EVENT LISTENERS] Setting up event listeners");

    this.handleEvent("start-video-call", ({ call_id, call_link }) => {
      console.log("📞 ========================================");
      console.log("📞 [START CALL EVENT] Received start-video-call event");
      console.log("📞 [START CALL EVENT] Call ID:", call_id);
      console.log("📞 [START CALL EVENT] Call Link:", call_link);
      console.log("📞 ========================================");

      this.callId = call_id;
      this.isCaller = true;
      this.initializeWebRTC();
    });

    this.handleEvent("accept-video-call", ({ call_id, call_link }) => {
      console.log("✅ ========================================");
      console.log("✅ [ACCEPT CALL EVENT] Received accept-video-call event");
      console.log("✅ [ACCEPT CALL EVENT] Call ID:", call_id);
      console.log("✅ [ACCEPT CALL EVENT] Call Link:", call_link);
      console.log("✅ ========================================");

      this.callId = call_id;
      this.isCaller = false;
      this.initializeWebRTC();
    });

    this.handleEvent("end-video-call", () => {
      console.log("🔴 [END CALL EVENT] Received end-video-call event");
      this.endVideoCall();
      this.pushEvent("call-status-updated", { status: "disconnected" });
    });

    this.handleEvent(
      "incoming-call",
      ({ call_id, call_link, caller_username }) => {
        console.log("📲 ========================================");
        console.log("📲 [INCOMING CALL EVENT] Received incoming-call event");
        console.log("📲 [INCOMING CALL EVENT] Call ID:", call_id);
        console.log("📲 [INCOMING CALL EVENT] Call Link:", call_link);
        console.log("📲 [INCOMING CALL EVENT] Caller:", caller_username);
        console.log("📲 ========================================");
      },
    );

    document
      .querySelectorAll('button[phx-click="end-video-call"]')
      .forEach((btn) => {
        btn.addEventListener("click", (e) => {
          console.log("🔴 [BUTTON CLICK] End call button clicked");
          e.preventDefault();
          e.stopPropagation();
          this.endVideoCall();
          this.pushEvent("call-status-updated", { status: "disconnected" });
        });
      });

    console.log("✅ [EVENT LISTENERS] Event listeners setup complete");
  },

  async initializeWebRTC() {
    console.log("🎥 ========================================");
    console.log("🎥 [INIT WebRTC] Initializing WebRTC");
    console.log("🎥 [INIT WebRTC] Is Shutdown:", this.isShutdown);
    console.log("🎥 [INIT WebRTC] Call ID:", this.callId);
    console.log("🎥 [INIT WebRTC] Is Caller:", this.isCaller);

    if (this.isShutdown) {
      console.log("⚠️ [INIT WebRTC] System is shutdown, aborting");
      return;
    }

    if (!this.callId) {
      console.error("❌ [INIT WebRTC] No call ID provided");
      this.pushEvent("call-error", { message: "No call ID provided" });
      return;
    }

    try {
      const localVideo = document.getElementById("local-video");
      const remoteVideo = document.getElementById("remote-video");

      console.log("🔍 [INIT WebRTC] Local video element:", localVideo);
      console.log("🔍 [INIT WebRTC] Remote video element:", remoteVideo);

      if (!localVideo || !remoteVideo) {
        console.error("❌ [INIT WebRTC] Video elements not found!");
        console.error("❌ [INIT WebRTC] Local video:", localVideo);
        console.error("❌ [INIT WebRTC] Remote video:", remoteVideo);
        this.pushEvent("call-error", { message: "Video elements not found" });
        return;
      }

      console.log("📹 [INIT WebRTC] Requesting user media...");

      const constraints = {
        video: {
          width: { ideal: 1280 },
          height: { ideal: 720 },
          facingMode: "user",
        },
        audio: {
          echoCancellation: true,
          noiseSuppression: true,
          autoGainControl: true,
        },
      };

      const stream = await navigator.mediaDevices.getUserMedia(constraints);

      console.log("✅ [INIT WebRTC] Got user media stream");
      console.log(
        "📹 [INIT WebRTC] Video tracks:",
        stream.getVideoTracks().length,
      );
      console.log(
        "🎤 [INIT WebRTC] Audio tracks:",
        stream.getAudioTracks().length,
      );
      console.log(
        "📹 [INIT WebRTC] Video track settings:",
        stream.getVideoTracks()[0]?.getSettings(),
      );
      console.log(
        "🎤 [INIT WebRTC] Audio track settings:",
        stream.getAudioTracks()[0]?.getSettings(),
      );

      this.localStream = stream;
      localVideo.srcObject = stream;

      await localVideo.play().catch((e) => {
        console.error("❌ [INIT WebRTC] Failed to play local video:", e);
      });

      console.log("✅ [INIT WebRTC] Local video source set and playing");

      this.setupPeerConnection();
      this.connectSignaling();

      console.log("🎥 ========================================");
    } catch (error) {
      console.error("❌ ========================================");
      console.error("❌ [INIT WebRTC] Failed to get user media");
      console.error("❌ [INIT WebRTC] Error:", error);
      console.error("❌ [INIT WebRTC] Error name:", error.name);
      console.error("❌ [INIT WebRTC] Error message:", error.message);
      console.error("❌ [INIT WebRTC] Error stack:", error.stack);
      console.error("❌ ========================================");

      let errorMessage = "Failed to access camera/microphone: ";
      if (error.name === "NotAllowedError") {
        errorMessage +=
          "Permission denied. Please allow camera and microphone access.";
      } else if (error.name === "NotFoundError") {
        errorMessage += "No camera or microphone found.";
      } else if (error.name === "NotReadableError") {
        errorMessage += "Camera or microphone is already in use.";
      } else {
        errorMessage += error.message;
      }

      this.pushEvent("call-error", { message: errorMessage });
    }
  },

  setupPeerConnection() {
    console.log("🔗 [PEER CONNECTION] Setting up peer connection");

    const configuration = {
      iceServers: [
        { urls: "stun:stun.l.google.com:19302" },
        { urls: "stun:stun1.l.google.com:19302" },
        { urls: "stun:stun2.l.google.com:19302" },
        { urls: "stun:stun3.l.google.com:19302" },
        { urls: "stun:stun4.l.google.com:19302" },
      ],
      iceCandidatePoolSize: 10,
      bundlePolicy: "max-bundle",
      rtcpMuxPolicy: "require",
    };

    this.peerConnection = new RTCPeerConnection(configuration);
    console.log("✅ [PEER CONNECTION] RTCPeerConnection created");
    console.log(
      "🔗 [PEER CONNECTION] Connection state:",
      this.peerConnection.connectionState,
    );
    console.log(
      "🔗 [PEER CONNECTION] Signaling state:",
      this.peerConnection.signalingState,
    );

    this.localStream.getTracks().forEach((track) => {
      console.log(
        "➕ [PEER CONNECTION] Adding track:",
        track.kind,
        track.label,
        track.enabled,
      );
      const sender = this.peerConnection.addTrack(track, this.localStream);
      console.log("✅ [PEER CONNECTION] Track added, sender:", sender);
    });

    this.peerConnection.ontrack = (event) => {
      console.log("📥 ========================================");
      console.log("📥 [PEER CONNECTION] Received remote track");
      console.log("📥 [PEER CONNECTION] Track kind:", event.track.kind);
      console.log("📥 [PEER CONNECTION] Track label:", event.track.label);
      console.log("📥 [PEER CONNECTION] Track enabled:", event.track.enabled);
      console.log("📥 [PEER CONNECTION] Track muted:", event.track.muted);
      console.log("📥 [PEER CONNECTION] Streams:", event.streams.length);

      const remoteVideo = document.getElementById("remote-video");
      if (remoteVideo && event.streams[0]) {
        console.log("✅ [PEER CONNECTION] Setting remote video source");
        remoteVideo.srcObject = event.streams[0];
        this.remoteStream = event.streams[0];

        remoteVideo
          .play()
          .then(() => {
            console.log("✅ [PEER CONNECTION] Remote video playing");
          })
          .catch((e) => {
            console.error(
              "❌ [PEER CONNECTION] Failed to play remote video:",
              e,
            );
          });
      } else {
        console.error(
          "❌ [PEER CONNECTION] Remote video element not found or no streams",
        );
      }
      console.log("📥 ========================================");
    };

    this.peerConnection.onicecandidate = (event) => {
      if (event.candidate) {
        console.log("🧊 [ICE] New ICE candidate:", event.candidate.candidate);
        console.log("🧊 [ICE] Candidate type:", event.candidate.type);
        console.log("🧊 [ICE] Candidate protocol:", event.candidate.protocol);

        if (this.websocket && this.websocket.readyState === WebSocket.OPEN) {
          console.log("🧊 [ICE] Sending ICE candidate via WebSocket");
          this.websocket.send(
            JSON.stringify({
              type: "ice-candidate",
              candidate: event.candidate,
              userId: this.userId,
            }),
          );
        } else {
          console.log("🧊 [ICE] Queueing ICE candidate (WebSocket not ready)");
          this.iceCandidateQueue.push(event.candidate);
        }
      } else {
        console.log("🧊 [ICE] ICE gathering complete");
      }
    };

    this.peerConnection.onconnectionstatechange = () => {
      console.log(
        "🔄 [CONNECTION STATE] State changed to:",
        this.peerConnection.connectionState,
      );
      console.log(
        "🔄 [CONNECTION STATE] ICE state:",
        this.peerConnection.iceConnectionState,
      );
      console.log(
        "🔄 [CONNECTION STATE] Signaling state:",
        this.peerConnection.signalingState,
      );

      if (this.peerConnection.connectionState === "connected") {
        console.log("✅ [CONNECTION STATE] Peer connection established!");
        this.pushEvent("call-status-updated", { status: "connected" });
        this.reconnectAttempts = 0;
      } else if (this.peerConnection.connectionState === "failed") {
        console.error("❌ [CONNECTION STATE] Peer connection failed");
        this.handleConnectionFailure();
      } else if (this.peerConnection.connectionState === "disconnected") {
        console.log("🔴 [CONNECTION STATE] Peer connection disconnected");
        this.handleConnectionFailure();
      }
    };

    this.peerConnection.oniceconnectionstatechange = () => {
      console.log(
        "🧊 [ICE CONNECTION STATE]:",
        this.peerConnection.iceConnectionState,
      );

      if (this.peerConnection.iceConnectionState === "failed") {
        console.error(
          "❌ [ICE CONNECTION STATE] ICE connection failed, attempting restart",
        );
        this.peerConnection.restartIce();
      }
    };

    this.peerConnection.onicegatheringstatechange = () => {
      console.log(
        "🧊 [ICE GATHERING STATE]:",
        this.peerConnection.iceGatheringState,
      );
    };

    this.peerConnection.onnegotiationneeded = async () => {
      console.log("🔄 [NEGOTIATION] Negotiation needed");
      console.log("🔄 [NEGOTIATION] Is negotiating:", this.isNegotiating);
      console.log(
        "🔄 [NEGOTIATION] Signaling state:",
        this.peerConnection.signalingState,
      );

      if (
        this.isNegotiating ||
        this.peerConnection.signalingState !== "stable"
      ) {
        console.log(
          "⚠️ [NEGOTIATION] Already negotiating or not stable, skipping",
        );
        return;
      }

      this.isNegotiating = true;

      try {
        if (this.isCaller) {
          console.log("📞 [NEGOTIATION] Creating new offer as caller");
          await this.createOffer();
        }
      } catch (error) {
        console.error("❌ [NEGOTIATION] Error during negotiation:", error);
      } finally {
        this.isNegotiating = false;
      }
    };

    console.log("✅ [PEER CONNECTION] Setup complete");
  },

  handleConnectionFailure() {
    console.log("⚠️ [CONNECTION FAILURE] Handling connection failure");
    console.log(
      "⚠️ [CONNECTION FAILURE] Reconnect attempts:",
      this.reconnectAttempts,
    );

    if (this.reconnectAttempts < this.maxReconnectAttempts) {
      this.reconnectAttempts++;
      console.log(
        `🔄 [CONNECTION FAILURE] Attempting reconnection ${this.reconnectAttempts}/${this.maxReconnectAttempts}`,
      );

      setTimeout(() => {
        if (!this.isShutdown) {
          console.log("🔄 [CONNECTION FAILURE] Restarting ICE");
          this.peerConnection.restartIce();
        }
      }, this.reconnectDelay);
    } else {
      console.error(
        "❌ [CONNECTION FAILURE] Max reconnection attempts reached",
      );
      this.pushEvent("call-error", {
        message: "Connection failed after multiple attempts",
      });
    }
  },

  connectSignaling() {
    const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    const wsUrl = `${protocol}//localhost:2020/ws/signaling/${this.callId}`;

    console.log("🌐 ========================================");
    console.log("🌐 [WEBSOCKET] Connecting to signaling server");
    console.log("🌐 [WEBSOCKET] URL:", wsUrl);
    console.log("🌐 [WEBSOCKET] Is Caller:", this.isCaller);
    console.log("🌐 [WEBSOCKET] User ID:", this.userId);

    this.websocket = new WebSocket(wsUrl);

    this.websocket.onopen = async () => {
      console.log("✅ [WEBSOCKET] Connected to signaling server");

      const joinMessage = {
        type: "join",
        userId: this.userId,
        callId: this.callId,
      };

      console.log("📤 [WEBSOCKET] Sending join message:", joinMessage);
      this.websocket.send(JSON.stringify(joinMessage));

      while (this.iceCandidateQueue.length > 0) {
        const candidate = this.iceCandidateQueue.shift();
        console.log("🧊 [WEBSOCKET] Sending queued ICE candidate");
        this.websocket.send(
          JSON.stringify({
            type: "ice-candidate",
            candidate: candidate,
            userId: this.userId,
          }),
        );
      }

      if (this.isCaller) {
        console.log("📞 [WEBSOCKET] I'm the caller, creating offer...");
        await this.createOffer();
      } else {
        console.log("📞 [WEBSOCKET] I'm the receiver, waiting for offer...");
      }

      console.log("🌐 ========================================");
    };

    this.websocket.onmessage = async (event) => {
      try {
        const message = JSON.parse(event.data);
        console.log("📨 [WEBSOCKET MESSAGE] Received message");
        console.log("📨 [WEBSOCKET MESSAGE] Type:", message.type);
        console.log("📨 [WEBSOCKET MESSAGE] Full message:", message);

        switch (message.type) {
          case "offer":
            console.log("📨 [WEBSOCKET] Received offer");
            await this.handleOffer(message.offer);
            break;
          case "answer":
            console.log("📨 [WEBSOCKET] Received answer");
            await this.handleAnswer(message.answer);
            break;
          case "ice-candidate":
            console.log("📨 [WEBSOCKET] Received ICE candidate");
            await this.handleIceCandidate(message.candidate);
            break;
          default:
            console.log("📨 [WEBSOCKET] Unknown message type:", message.type);
        }
      } catch (error) {
        console.error("❌ [WEBSOCKET] Error handling message:", error);
        console.error("❌ [WEBSOCKET] Error stack:", error.stack);
      }
    };

    this.websocket.onerror = (error) => {
      console.error("❌ ========================================");
      console.error("❌ [WEBSOCKET] WebSocket error:", error);
      console.error("❌ ========================================");
      this.pushEvent("call-error", { message: "Signaling connection error" });
    };

    this.websocket.onclose = (event) => {
      console.log("🔴 ========================================");
      console.log("🔴 [WEBSOCKET] WebSocket closed");
      console.log("🔴 [WEBSOCKET] Code:", event.code);
      console.log("🔴 [WEBSOCKET] Reason:", event.reason);
      console.log("🔴 [WEBSOCKET] Was clean:", event.wasClean);
      console.log("🔴 ========================================");
    };
  },

  async createOffer() {
    console.log("📝 [OFFER] Creating offer...");
    console.log(
      "📝 [OFFER] Signaling state:",
      this.peerConnection.signalingState,
    );

    try {
      const offerOptions = {
        offerToReceiveAudio: true,
        offerToReceiveVideo: true,
      };

      const offer = await this.peerConnection.createOffer(offerOptions);
      console.log("✅ [OFFER] Offer created");
      console.log("📝 [OFFER] SDP:", offer.sdp);

      await this.peerConnection.setLocalDescription(offer);
      console.log("✅ [OFFER] Local description set");
      console.log(
        "📝 [OFFER] Signaling state after setting:",
        this.peerConnection.signalingState,
      );

      const offerMessage = {
        type: "offer",
        offer: offer,
        userId: this.userId,
      };

      console.log("📤 [OFFER] Sending offer");
      this.websocket.send(JSON.stringify(offerMessage));
    } catch (error) {
      console.error("❌ [OFFER] Error creating offer:", error);
      console.error("❌ [OFFER] Error stack:", error.stack);
      throw error;
    }
  },

  async handleOffer(offer) {
    console.log("📥 [OFFER] Handling received offer");
    console.log(
      "📥 [OFFER] Signaling state:",
      this.peerConnection.signalingState,
    );
    console.log("📥 [OFFER] Offer SDP:", offer.sdp);

    try {
      if (this.peerConnection.signalingState !== "stable") {
        console.log("⚠️ [OFFER] Not in stable state, waiting...");
        await new Promise((resolve) => setTimeout(resolve, 100));
      }

      await this.peerConnection.setRemoteDescription(
        new RTCSessionDescription(offer),
      );
      console.log("✅ [OFFER] Remote description set");
      console.log(
        "📥 [OFFER] Signaling state after setting:",
        this.peerConnection.signalingState,
      );

      const answer = await this.peerConnection.createAnswer();
      console.log("✅ [ANSWER] Answer created");
      console.log("📥 [ANSWER] Answer SDP:", answer.sdp);

      await this.peerConnection.setLocalDescription(answer);
      console.log("✅ [ANSWER] Local description set");
      console.log(
        "📥 [ANSWER] Signaling state after setting:",
        this.peerConnection.signalingState,
      );

      const answerMessage = {
        type: "answer",
        answer: answer,
        userId: this.userId,
      };

      console.log("📤 [ANSWER] Sending answer");
      this.websocket.send(JSON.stringify(answerMessage));
    } catch (error) {
      console.error("❌ [OFFER] Error handling offer:", error);
      console.error("❌ [OFFER] Error stack:", error.stack);
      throw error;
    }
  },

  async handleAnswer(answer) {
    console.log("📥 [ANSWER] Handling received answer");
    console.log(
      "📥 [ANSWER] Signaling state:",
      this.peerConnection.signalingState,
    );
    console.log("📥 [ANSWER] Answer SDP:", answer.sdp);

    try {
      if (this.peerConnection.signalingState !== "have-local-offer") {
        console.log(
          "⚠️ [ANSWER] Not in have-local-offer state, current state:",
          this.peerConnection.signalingState,
        );
      }

      await this.peerConnection.setRemoteDescription(
        new RTCSessionDescription(answer),
      );
      console.log("✅ [ANSWER] Remote description set");
      console.log(
        "📥 [ANSWER] Signaling state after setting:",
        this.peerConnection.signalingState,
      );
    } catch (error) {
      console.error("❌ [ANSWER] Error handling answer:", error);
      console.error("❌ [ANSWER] Error stack:", error.stack);
      throw error;
    }
  },

  async handleIceCandidate(candidate) {
    console.log("🧊 [ICE CANDIDATE] Handling received ICE candidate");
    console.log("🧊 [ICE CANDIDATE] Candidate:", candidate.candidate);
    console.log(
      "🧊 [ICE CANDIDATE] Remote description set:",
      !!this.peerConnection.remoteDescription,
    );

    try {
      if (!this.peerConnection.remoteDescription) {
        console.log(
          "⚠️ [ICE CANDIDATE] No remote description yet, queueing candidate",
        );
        this.iceCandidateQueue.push(candidate);
        return;
      }

      await this.peerConnection.addIceCandidate(new RTCIceCandidate(candidate));
      console.log("✅ [ICE CANDIDATE] ICE candidate added");
    } catch (error) {
      console.error("❌ [ICE CANDIDATE] Error adding ICE candidate:", error);
      console.error("❌ [ICE CANDIDATE] Error stack:", error.stack);
    }
  },

  endVideoCall() {
    console.log("🔴 ========================================");
    console.log("🔴 [END CALL] Ending video call");

    this.isShutdown = true;

    if (this.localStream) {
      console.log("🔴 [END CALL] Stopping local stream");
      this.localStream.getTracks().forEach((track) => {
        console.log("🔴 [END CALL] Stopping track:", track.kind, track.label);
        track.stop();
      });
      this.localStream = null;
    }

    if (this.peerConnection) {
      console.log("🔴 [END CALL] Closing peer connection");
      console.log(
        "🔴 [END CALL] Connection state before close:",
        this.peerConnection.connectionState,
      );
      this.peerConnection.close();
      this.peerConnection = null;
    }

    if (this.websocket) {
      console.log("🔴 [END CALL] Closing WebSocket");
      console.log("🔴 [END CALL] WebSocket state:", this.websocket.readyState);
      this.websocket.close();
      this.websocket = null;
    }

    const localVideo = document.getElementById("local-video");
    const remoteVideo = document.getElementById("remote-video");

    if (localVideo) {
      console.log("🔴 [END CALL] Clearing local video");
      localVideo.srcObject = null;
    }
    if (remoteVideo) {
      console.log("🔴 [END CALL] Clearing remote video");
      remoteVideo.srcObject = null;
    }

    this.iceCandidateQueue = [];
    this.isNegotiating = false;
    this.reconnectAttempts = 0;

    console.log("✅ [END CALL] Video call ended successfully");
    console.log("🔴 ========================================");
  },

  destroyed() {
    console.log("💀 [DESTROYED] VideoCall hook destroyed");
    this.endVideoCall();
  },
};

export default VideoCallHook;
