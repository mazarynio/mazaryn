const VideoCallHook = {
    mounted() {
      this.peerConnection = null;
      this.localStream = null;
      this.remoteStream = null;
      this.ws = null;
  
      this.handleEvent("start-video-call", ({ call_id, call_link }) => {
        this.startVideoCall(call_id, call_link);
      });
  
      this.handleEvent("incoming-call", ({ call_id, call_link, caller_username }) => {
      });
  
      this.handleEvent("accept-video-call", ({ call_id, call_link }) => {
        this.acceptVideoCall(call_id, call_link);
      });
  
      this.handleEvent("end-video-call", ({ call_id }) => {
        this.endVideoCall();
      });
    },
  
    async startVideoCall(call_id, call_link) {
      try {
        this.localStream = await navigator.mediaDevices.getUserMedia({ video: true, audio: true });
        document.getElementById("local-video").srcObject = this.localStream;
        this.remoteStream = new MediaStream();
        document.getElementById("remote-video").srcObject = this.remoteStream;
  
        const configuration = {
          iceServers: [{ urls: "stun:stun.l.google.com:19302" }]
        };
        this.peerConnection = new RTCPeerConnection(configuration);
  
        this.localStream.getTracks().forEach(track => {
          this.peerConnection.addTrack(track, this.localStream);
        });
  
        this.peerConnection.ontrack = (event) => {
          event.streams[0].getTracks().forEach(track => {
            this.remoteStream.addTrack(track);
          });
        };
  
        this.peerConnection.onicecandidate = (event) => {
          if (event.candidate && this.ws) {
            this.ws.send(JSON.stringify({ type: "ice", candidate: event.candidate }));
          }
        };
  
        this.ws = new WebSocket(call_link);
        this.ws.onmessage = (event) => {
          const message = JSON.parse(event.data);
          if (message.type === "offer") {
            this.peerConnection.setRemoteDescription(new RTCSessionDescription(message));
            this.createAnswer();
          } else if (message.type === "answer") {
            this.peerConnection.setRemoteDescription(new RTCSessionDescription(message));
          } else if (message.type === "ice") {
            this.peerConnection.addIceCandidate(new RTCIceCandidate(message.candidate));
          }
        };
  
        const offer = await this.peerConnection.createOffer();
        await this.peerConnection.setLocalDescription(offer);
        this.ws.send(JSON.stringify(offer));
      } catch (error) {
        console.error("Error starting video call:", error);
        this.pushEvent("call-error", { message: "Failed to start video call" });
      }
    },
  
    async acceptVideoCall(call_id, call_link) {
      try {
        this.localStream = await navigator.mediaDevices.getUserMedia({ video: true, audio: true });
        document.getElementById("local-video").srcObject = this.localStream;
        this.remoteStream = new MediaStream();
        document.getElementById("remote-video").srcObject = this.remoteStream;
  
        const configuration = {
          iceServers: [{ urls: "stun:stun.l.google.com:19302" }]
        };
        this.peerConnection = new RTCPeerConnection(configuration);
  
        this.localStream.getTracks().forEach(track => {
          this.peerConnection.addTrack(track, this.localStream);
        });
  
        this.peerConnection.ontrack = (event) => {
          event.streams[0].getTracks().forEach(track => {
            this.remoteStream.addTrack(track);
          });
        };
  
        this.peerConnection.onicecandidate = (event) => {
          if (event.candidate && this.ws) {
            this.ws.send(JSON.stringify({ type: "ice", candidate: event.candidate }));
          }
        };
  
        this.ws = new WebSocket(call_link);
        this.ws.onmessage = (event) => {
          const message = JSON.parse(event.data);
          if (message.type === "offer") {
            this.peerConnection.setRemoteDescription(new RTCSessionDescription(message));
            this.createAnswer();
          } else if (message.type === "answer") {
            this.peerConnection.setRemoteDescription(new RTCSessionDescription(message));
          } else if (message.type === "ice") {
            this.peerConnection.addIceCandidate(new RTCIceCandidate(message.candidate));
          }
        };
      } catch (error) {
        console.error("Error accepting video call:", error);
        this.pushEvent("call-error", { message: "Failed to accept video call" });
      }
    },
  
    async createAnswer() {
      try {
        const answer = await this.peerConnection.createAnswer();
        await this.peerConnection.setLocalDescription(answer);
        this.ws.send(JSON.stringify(answer));
      } catch (error) {
        console.error("Error creating answer:", error);
        this.pushEvent("call-error", { message: "Failed to create answer" });
      }
    },
  
    endVideoCall() {
      if (this.localStream) {
        this.localStream.getTracks().forEach(track => track.stop());
      }
      if (this.peerConnection) {
        this.peerConnection.close();
      }
      if (this.ws) {
        this.ws.close();
      }
      document.getElementById("local-video").srcObject = null;
      document.getElementById("remote-video").srcObject = null;
      this.peerConnection = null;
      this.localStream = null;
      this.remoteStream = null;
      this.ws = null;
    }
  };
  
  export default VideoCallHook;