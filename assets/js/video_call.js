const VideoCallHook = {
  mounted() {
    console.log("VideoCallHook mounted with elements:", {
      el: this.el,
      dataUsername: this.el.dataset.username,
      dataCurrentRecipientUsername: this.el.dataset.currentRecipientUsername
    });
    
    this.addDiagnosticElements();
    
    this.peerConnection = null;
    this.localStream = null;
    this.remoteStream = null;
    this.ws = null;
    this.isSelfCall = false;
    this.isCalling = false;

    this.handleEvent("start-video-call", ({ call_id, call_link }) => {
      this.logToPage("Received start-video-call event", { call_id, call_link });
      if (this.isCalling) {
        this.logToPage("Call already in progress, ignoring start-video-call");
        return;
      }
      this.startVideoCall(call_id, call_link);
    });

    this.handleEvent("incoming-call", ({ call_id, call_link, caller_username }) => {
      this.logToPage("Received incoming-call event", { call_id, call_link, caller_username });
      if (caller_username === this.el.dataset.username) {
        this.logToPage("Self call detected in incoming-call handler");
        this.isSelfCall = true;
        this.acceptVideoCall(call_id, call_link);
      } else {
        this.pushEvent("incoming-call-received", { call_id, call_link, caller_username });
      }
    });

    this.handleEvent("accept-video-call", ({ call_id, call_link }) => {
      this.logToPage("Received accept-video-call event", { call_id, call_link });
      this.acceptVideoCall(call_id, call_link);
    });

    this.handleEvent("end-video-call", ({ call_id }) => {
      this.logToPage("Received end-video-call event", { call_id });
      this.endVideoCall();
    });
  },

  addDiagnosticElements() {

    const diagnosticDiv = document.createElement('div');
    diagnosticDiv.id = 'video-call-diagnostic';
    diagnosticDiv.style.cssText = 'max-height: 200px; overflow-y: auto; background-color: #f8f8f8; border: 1px solid #ddd; padding: 10px; margin: 10px 0; font-family: monospace; font-size: 12px;';
    
    const testLocalStreamBtn = document.createElement('button');
    testLocalStreamBtn.textContent = 'Test Local Stream';
    testLocalStreamBtn.style.cssText = 'margin-right: 10px; padding: 5px; background: #4CAF50; color: white; border: none;';
    testLocalStreamBtn.onclick = () => this.testLocalStream();
    
    const testRemoteStreamBtn = document.createElement('button');
    testRemoteStreamBtn.textContent = 'Test Remote Stream';
    testRemoteStreamBtn.style.cssText = 'margin-right: 10px; padding: 5px; background: #2196F3; color: white; border: none;';
    testRemoteStreamBtn.onclick = () => this.testRemoteStream();
    
    const toggleVideoBtn = document.createElement('button');
    toggleVideoBtn.textContent = 'Force Play Videos';
    toggleVideoBtn.style.cssText = 'padding: 5px; background: #FF9800; color: white; border: none;';
    toggleVideoBtn.onclick = () => this.forcePlayVideos();
    
    const buttonContainer = document.createElement('div');
    buttonContainer.style.cssText = 'margin: 10px 0;';
    buttonContainer.appendChild(testLocalStreamBtn);
    buttonContainer.appendChild(testRemoteStreamBtn);
    buttonContainer.appendChild(toggleVideoBtn);
    
    this.el.insertBefore(diagnosticDiv, this.el.firstChild);
    this.el.insertBefore(buttonContainer, this.el.firstChild);
    
    this.diagnosticDiv = diagnosticDiv;
  },
  
  logToPage(message, data) {
    if (!this.diagnosticDiv) return;
    
    const timestamp = new Date().toLocaleTimeString();
    const logEntry = document.createElement('div');
    
    let logText = `[${timestamp}] ${message}`;
    if (data) {
      try {
        logText += ": " + JSON.stringify(data);
      } catch (e) {
        logText += ": [Object cannot be stringified]";
      }
    }
    
    logEntry.textContent = logText;
    this.diagnosticDiv.appendChild(logEntry);
    this.diagnosticDiv.scrollTop = this.diagnosticDiv.scrollHeight;
    
    console.log(message, data);
  },
  
  async testLocalStream() {
    this.logToPage("Testing local stream...");
    try {
      const stream = await navigator.mediaDevices.getUserMedia({ video: true, audio: true });
      this.logToPage("Got local stream", {
        videoTracks: stream.getVideoTracks().length,
        audioTracks: stream.getAudioTracks().length
      });
      
      const localVideo = document.getElementById("local-video");
      if (localVideo) {
        localVideo.srcObject = stream;
        localVideo.play().then(() => {
          this.logToPage("Local video started playing");
        }).catch(err => {
          this.logToPage("Error playing local video", err.message);
        });
      } else {
        this.logToPage("Local video element not found");
      }
      
      // Stop the stream after testing
      setTimeout(() => {
        stream.getTracks().forEach(track => track.stop());
        if (localVideo) localVideo.srcObject = null;
        this.logToPage("Test stream stopped");
      }, 10000);
    } catch (error) {
      this.logToPage("Error testing local stream", error.message);
    }
  },
  
  async testRemoteStream() {
    this.logToPage("Testing remote stream with local camera as source...");
    try {
      const stream = await navigator.mediaDevices.getUserMedia({ video: true, audio: false });
      this.logToPage("Got test stream", {
        videoTracks: stream.getVideoTracks().length
      });
      
      const remoteVideo = document.getElementById("remote-video");
      if (remoteVideo) {
        remoteVideo.srcObject = stream;
        remoteVideo.play().then(() => {
          this.logToPage("Remote video started playing");
        }).catch(err => {
          this.logToPage("Error playing remote video", err.message);
        });
      } else {
        this.logToPage("Remote video element not found");
      }
      
      // Stop the stream after testing
      setTimeout(() => {
        stream.getTracks().forEach(track => track.stop());
        if (remoteVideo) remoteVideo.srcObject = null;
        this.logToPage("Test stream stopped");
      }, 10000);
    } catch (error) {
      this.logToPage("Error testing remote stream", error.message);
    }
  },
  
  forcePlayVideos() {
    this.logToPage("Attempting to force play videos...");
    const localVideo = document.getElementById("local-video");
    const remoteVideo = document.getElementById("remote-video");
    
    if (localVideo && localVideo.srcObject) {
      localVideo.play().then(() => {
        this.logToPage("Forced local video to play");
      }).catch(err => {
        this.logToPage("Error forcing local video to play", err.message);
      });
    } else {
      this.logToPage("Local video has no source to play");
    }
    
    if (remoteVideo && remoteVideo.srcObject) {
      remoteVideo.play().then(() => {
        this.logToPage("Forced remote video to play");
      }).catch(err => {
        this.logToPage("Error forcing remote video to play", err.message);
      });
    } else {
      this.logToPage("Remote video has no source to play");
    }
  },

  async startVideoCall(call_id, call_link) {
    try {
      this.logToPage("Starting video call", { call_id, call_link });
      this.isCalling = true;
      this.endVideoCall(); 

      // Check available devices
      const devices = await navigator.mediaDevices.enumerateDevices();
      const videoDevices = devices.filter(device => device.kind === "videoinput");
      this.logToPage("Available devices", {
        videoDevices: videoDevices.length,
        audioDevices: devices.filter(device => device.kind === "audioinput").length
      });
      
      if (videoDevices.length === 0) {
        this.logToPage("No video devices found");
        this.pushEvent("call-error", { message: "No camera detected. Please connect a camera." });
        this.isCalling = false;
        return;
      }

      this.logToPage("Requesting media");
      this.localStream = await navigator.mediaDevices.getUserMedia({ 
        video: { 
          width: { ideal: 640 },
          height: { ideal: 480 }
        }, 
        audio: true 
      });
      
      const videoTracks = this.localStream.getVideoTracks();
      const audioTracks = this.localStream.getAudioTracks();
      this.logToPage("Local stream acquired", {
        videoTracks: videoTracks.length,
        audioTracks: audioTracks.length
      });

      if (videoTracks.length === 0) {
        this.logToPage("No video tracks in stream");
        this.pushEvent("call-error", { message: "Camera stream has no video. Check camera settings." });
        this.isCalling = false;
        return;
      }

      const localVideo = document.getElementById("local-video");
      const remoteVideo = document.getElementById("remote-video");
      if (!localVideo || !remoteVideo) {
        this.logToPage("Video elements not found", { localVideo: !!localVideo, remoteVideo: !!remoteVideo });
        this.pushEvent("call-error", { message: "Video elements not found" });
        this.isCalling = false;
        return;
      }

      this.logToPage("Setting local video source");
      localVideo.srcObject = this.localStream;
      localVideo.play().catch(err => {
        this.logToPage("Error playing local video", err.message);
      });
      
      this.remoteStream = new MediaStream();
      this.logToPage("Setting remote video source");
      remoteVideo.srcObject = this.remoteStream;
      remoteVideo.play().catch(err => {
        this.logToPage("Error playing remote video", err.message);
      });

      const isSelfCall = this.el.dataset.username === this.el.dataset.currentRecipientUsername;
      this.logToPage("Self-call check", { 
        isSelfCall,
        username: this.el.dataset.username,
        recipientUsername: this.el.dataset.currentRecipientUsername 
      });
      
      if (isSelfCall) {
        this.logToPage("Self-call detected, mirroring local stream");
        this.isSelfCall = true;
        
        videoTracks.forEach(track => {
          const clonedTrack = track.clone();
          this.logToPage("Adding cloned video track to remote stream", {
            trackId: clonedTrack.id,
            enabled: clonedTrack.enabled
          });
          this.remoteStream.addTrack(clonedTrack);
        });
        
        this.logToPage("Self-call setup complete");
        this.pushEvent("call-status-updated", { status: "connected" });
        this.isCalling = false;
        return;
      }

      
      this.isCalling = false;
    } catch (error) {
      this.logToPage("Error starting video call", { 
        name: error.name, 
        message: error.message 
      });
      
      let errorMessage = error.message;
      if (error.name === "NotAllowedError") {
        errorMessage = "Camera access denied. Please allow camera access in your browser.";
      } else if (error.name === "NotFoundError") {
        errorMessage = "No camera found. Please connect a camera.";
      } else if (error.name === "NotReadableError") {
        errorMessage = "Camera is in use by another application.";
      }
      
      this.pushEvent("call-error", { message: `Failed to start video call: ${errorMessage}` });
      this.isCalling = false;
    }
  },

  
  async acceptVideoCall(call_id, call_link) {
    try {
      this.logToPage("Accepting video call", { call_id, call_link });
      
      const isSelfCall = this.el.dataset.username === this.el.dataset.currentRecipientUsername;
      this.logToPage("Self-call check in acceptVideoCall", { 
        isSelfCall,
        username: this.el.dataset.username,
        recipientUsername: this.el.dataset.currentRecipientUsername 
      });
      
      if (isSelfCall) {
        this.logToPage("Self-call detected in acceptVideoCall");
        this.localStream = await navigator.mediaDevices.getUserMedia({ 
          video: { 
            width: { ideal: 640 },
            height: { ideal: 480 }
          }, 
          audio: true 
        });
        
        this.logToPage("Local stream acquired for self-call", {
          videoTracks: this.localStream.getVideoTracks().length,
          audioTracks: this.localStream.getAudioTracks().length
        });

        const localVideo = document.getElementById("local-video");
        const remoteVideo = document.getElementById("remote-video");
        if (!localVideo || !remoteVideo) {
          this.logToPage("Video elements not found", { localVideo: !!localVideo, remoteVideo: !!remoteVideo });
          this.pushEvent("call-error", { message: "Video elements not found" });
          return;
        }

        localVideo.srcObject = this.localStream;
        localVideo.play().catch(err => {
          this.logToPage("Error playing local video", err.message);
        });
        
        this.remoteStream = new MediaStream();
        
        this.localStream.getVideoTracks().forEach(track => {
          const clonedTrack = track.clone();
          this.logToPage("Adding cloned video track to remote stream", {
            trackId: clonedTrack.id,
            enabled: clonedTrack.enabled
          });
          this.remoteStream.addTrack(clonedTrack);
        });
        
        remoteVideo.srcObject = this.remoteStream;
        remoteVideo.play().catch(err => {
          this.logToPage("Error playing remote video", err.message);
        });
        
        this.logToPage("Self-call accept complete");
        this.pushEvent("call-status-updated", { status: "connected" });
        return;
      }

      
    } catch (error) {
      this.logToPage("Error accepting video call", { 
        name: error.name, 
        message: error.message 
      });
      this.pushEvent("call-error", { message: `Failed to accept video call: ${error.message}` });
    }
  },

  async createAnswer() {

  },

  endVideoCall() {
    this.logToPage("Ending video call");
    if (this.localStream) {
      this.localStream.getTracks().forEach(track => {
        this.logToPage(`Stopping track: ${track.kind}`, { id: track.id });
        track.stop();
      });
    }
    if (this.peerConnection) {
      this.peerConnection.close();
    }
    if (this.ws) {
      this.ws.close();
    }
    const localVideo = document.getElementById("local-video");
    const remoteVideo = document.getElementById("remote-video");
    if (localVideo) localVideo.srcObject = null;
    if (remoteVideo) remoteVideo.srcObject = null;
    this.peerConnection = null;
    this.localStream = null;
    this.remoteStream = null;
    this.ws = null;
    this.isSelfCall = false;
    this.isCalling = false;
    this.logToPage("Video call ended and resources cleaned up");
  }
};

export default VideoCallHook;