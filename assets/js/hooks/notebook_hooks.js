const NotebookHooks = {
  CodeEditor: {
    mounted() {
      this.setupEditor();
      this.setupKeyboardShortcuts();
      this.autoResize();
    },

    updated() {
      this.autoResize();
    },

    setupEditor() {
      this.el.style.height = "auto";
      this.el.style.height = this.el.scrollHeight + "px";

      this.el.addEventListener("input", () => {
        this.autoResize();
      });

      this.el.addEventListener("blur", () => {
        const cellId = this.el.id.replace("cell-", "");
        this.pushEvent("update_cell", {
          cell_id: cellId,
          content: this.el.value,
        });
      });
    },

    setupKeyboardShortcuts() {
      this.el.addEventListener("keydown", (e) => {
        if (e.key === "Enter" && e.shiftKey) {
          e.preventDefault();
          const cellId = this.el.id.replace("cell-", "");
          this.pushEvent("execute_cell", { cell_id: cellId });
        }

        if (e.key === "Tab") {
          e.preventDefault();
          const start = this.el.selectionStart;
          const end = this.el.selectionEnd;
          const value = this.el.value;

          this.el.value =
            value.substring(0, start) + "    " + value.substring(end);
          this.el.selectionStart = this.el.selectionEnd = start + 4;

          this.el.dispatchEvent(new Event("input"));
        }

        if (e.key === "Escape") {
          this.el.blur();
        }

        if (e.key === "s" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.pushEvent("save_notebook", {});
        }
      });
    },

    autoResize() {
      this.el.style.height = "auto";
      this.el.style.height = Math.max(60, this.el.scrollHeight) + "px";
    },
  },

  NotebookWebSocket: {
    mounted() {
      const sessionId = this.el.dataset.sessionId;
      if (!sessionId) return;

      this.connect(sessionId);
    },

    destroyed() {
      this.disconnect();
    },

    connect(sessionId) {
      const wsUrl = `ws://localhost:2020/ws/notebooks/${sessionId}`;
      this.ws = new WebSocket(wsUrl);

      this.ws.onopen = () => {
        console.log("WebSocket connected to kernel");
        this.pushEvent("kernel_status", { status: "connected" });
        this.startHeartbeat();
      };

      this.ws.onmessage = (event) => {
        const message = JSON.parse(event.data);
        this.handleMessage(message);
      };

      this.ws.onerror = (error) => {
        console.error("WebSocket error:", error);
        this.pushEvent("kernel_status", { status: "error" });
      };

      this.ws.onclose = () => {
        console.log("WebSocket disconnected");
        this.pushEvent("kernel_status", { status: "disconnected" });
        this.stopHeartbeat();
      };
    },

    disconnect() {
      this.stopHeartbeat();
      if (this.ws) {
        this.ws.close();
        this.ws = null;
      }
    },

    handleMessage(message) {
      switch (message.type) {
        case "ExecutionStarted":
          this.pushEvent("execution_started", {
            cell_id: message.cell_id,
            execution_id: message.execution_id,
          });
          break;

        case "Output":
          this.pushEvent("cell_output", {
            cell_id: message.cell_id,
            execution_id: message.execution_id,
            output: message.output,
          });
          break;

        case "ExecutionComplete":
          this.pushEvent("execution_complete", {
            cell_id: message.cell_id,
            execution_id: message.execution_id,
            status: message.status,
            execution_time_ms: message.execution_time_ms,
          });
          break;

        case "Error":
          this.pushEvent("execution_error", {
            cell_id: message.cell_id,
            error: message.error,
          });
          break;

        case "Pong":
          break;

        default:
          console.log("Unknown message type:", message.type);
      }
    },

    startHeartbeat() {
      this.heartbeat = setInterval(() => {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
          this.ws.send(JSON.stringify({ type: "Ping" }));
        }
      }, 5000);
    },

    stopHeartbeat() {
      if (this.heartbeat) {
        clearInterval(this.heartbeat);
        this.heartbeat = null;
      }
    },

    executeCell(cellId, code) {
      if (this.ws && this.ws.readyState === WebSocket.OPEN) {
        this.ws.send(
          JSON.stringify({
            type: "ExecuteCode",
            cell_id: cellId,
            code: code,
          }),
        );
      }
    },
  },

  AutoSave: {
    mounted() {
      this.timeout = null;

      this.handleEvent("trigger_autosave", () => {
        if (this.timeout) clearTimeout(this.timeout);

        this.timeout = setTimeout(() => {
          this.pushEvent("save_notebook", {});
        }, 2000);
      });
    },

    destroyed() {
      if (this.timeout) {
        clearTimeout(this.timeout);
      }
    },
  },

  MarkdownRenderer: {
    mounted() {
      this.render();
    },

    updated() {
      this.render();
    },

    render() {
      const content = this.el.dataset.content;
      if (content && window.marked) {
        this.el.innerHTML = window.marked.parse(content);
      }
    },
  },

  PlotRenderer: {
    mounted() {
      this.renderPlot();
    },

    updated() {
      this.renderPlot();
    },

    renderPlot() {
      const plotData = JSON.parse(this.el.dataset.plot || "{}");

      if (plotData.format === "plotly" && window.Plotly) {
        window.Plotly.newPlot(this.el, plotData.data, plotData.layout, {
          responsive: true,
        });
      } else if (plotData.format === "matplotlib" && plotData.image) {
        const img = document.createElement("img");
        img.src = plotData.image;
        img.className = "max-w-full h-auto rounded-lg";
        this.el.innerHTML = "";
        this.el.appendChild(img);
      }
    },
  },

  CodeHighlight: {
    mounted() {
      this.highlight();
    },

    updated() {
      this.highlight();
    },

    highlight() {
      const codeBlocks = this.el.querySelectorAll("pre code");
      codeBlocks.forEach((block) => {
        if (window.hljs) {
          window.hljs.highlightElement(block);
        }
      });
    },
  },

  CellFocus: {
    mounted() {
      this.setupFocusHandling();
    },

    setupFocusHandling() {
      this.el.addEventListener("click", (e) => {
        const cellId = this.el.dataset.cellId;
        if (cellId) {
          this.pushEvent("select_cell", { cell_id: cellId });
        }
      });

      const textarea = this.el.querySelector("textarea");
      if (textarea) {
        textarea.addEventListener("focus", () => {
          const cellId = this.el.dataset.cellId;
          if (cellId) {
            this.pushEvent("select_cell", { cell_id: cellId });
          }
        });
      }
    },
  },

  NotebookShortcuts: {
    mounted() {
      this.setupGlobalShortcuts();
    },

    destroyed() {
      document.removeEventListener("keydown", this.keydownHandler);
    },

    setupGlobalShortcuts() {
      this.keydownHandler = (e) => {
        if (e.target.tagName === "TEXTAREA" || e.target.tagName === "INPUT") {
          return;
        }

        if (e.key === "s" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.pushEvent("save_notebook", {});
        }

        if (e.key === "r" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.pushEvent("run_all_cells", {});
        }

        if (e.key === "a" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.pushEvent("add_cell_below", { type: "code" });
        }

        if (e.key === "m" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.pushEvent("add_cell_below", { type: "markdown" });
        }
      };

      document.addEventListener("keydown", this.keydownHandler);
    },
  },

  FileUpload: {
    mounted() {
      this.setupFileInput();
    },

    setupFileInput() {
      this.el.addEventListener("change", (e) => {
        const file = e.target.files[0];
        if (!file) return;

        const reader = new FileReader();
        reader.onload = (event) => {
          const content = event.target.result;
          this.pushEvent("file_uploaded", {
            filename: file.name,
            content: content,
            type: file.type,
          });
        };

        if (file.name.endsWith(".ipynb")) {
          reader.readAsText(file);
        } else {
          reader.readAsDataURL(file);
        }
      });
    },
  },
};

export default NotebookHooks;
