const NotebookHooks = {
  CodeEditor: {
    mounted() {
      this.setupEditor();
      this.setupKeyboardShortcuts();
      this.setupSyntaxHighlighting();
      this.autoResize();
      this.setupLineNumbers();
    },

    updated() {
      this.autoResize();
    },

    setupEditor() {
      this.el.style.height = "auto";
      this.el.style.height = Math.max(80, this.el.scrollHeight) + "px";

      this.el.addEventListener("input", () => {
        this.autoResize();
        this.updateLineNumbers();
        this.triggerAutoSave();
      });

      this.el.addEventListener("blur", () => {
        const cellId = this.el.id.replace("cell-", "");
        this.pushEvent("update_cell", {
          cell_id: cellId,
          content: this.el.value,
        });
      });

      this.el.addEventListener("focus", () => {
        this.addFocusAnimation();
      });

      this.setupAutocomplete();
    },

    setupKeyboardShortcuts() {
      this.el.addEventListener("keydown", (e) => {
        if (e.key === "Enter" && e.shiftKey) {
          e.preventDefault();
          const cellId = this.el.id.replace("cell-", "");
          this.pushEvent("execute_cell", { cell_id: cellId });
          this.addExecutionFlash();
        }

        if (e.key === "Tab") {
          e.preventDefault();
          const start = this.el.selectionStart;
          const end = this.el.selectionEnd;
          const value = this.el.value;

          if (e.shiftKey) {
            const lines = value.substring(0, start).split("\n");
            const currentLine = lines[lines.length - 1];
            if (currentLine.startsWith("    ")) {
              const lineStart = start - currentLine.length;
              this.el.value =
                value.substring(0, lineStart) +
                currentLine.substring(4) +
                value.substring(start);
              this.el.selectionStart = this.el.selectionEnd = start - 4;
            }
          } else {
            this.el.value =
              value.substring(0, start) + "    " + value.substring(end);
            this.el.selectionStart = this.el.selectionEnd = start + 4;
          }

          this.el.dispatchEvent(new Event("input"));
        }

        if (e.key === "Escape") {
          this.el.blur();
        }

        if (e.key === "s" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.pushEvent("save_notebook", {});
          this.showSaveNotification();
        }

        if (e.key === "[" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.indentSelection(-1);
        }

        if (e.key === "]" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.indentSelection(1);
        }

        if (e.key === "/" && (e.ctrlKey || e.metaKey)) {
          e.preventDefault();
          this.toggleComment();
        }
      });
    },

    setupSyntaxHighlighting() {
      this.el.addEventListener("input", () => {
        this.highlightSyntax();
      });
    },

    setupLineNumbers() {
      const lineNumbersDiv = document.createElement("div");
      lineNumbersDiv.className = "line-numbers";
      lineNumbersDiv.style.cssText = `
        position: absolute;
        left: 0;
        top: 0;
        padding: 1rem 0.5rem;
        font-family: 'JetBrains Mono', monospace;
        font-size: 0.75rem;
        color: #cbd5e1;
        text-align: right;
        user-select: none;
        pointer-events: none;
      `;

      if (this.el.parentElement) {
        this.el.parentElement.style.position = "relative";
        this.el.style.paddingLeft = "3rem";
      }

      this.updateLineNumbers();
    },

    updateLineNumbers() {
      const lines = this.el.value.split("\n").length;
      const lineNumbersDiv =
        this.el.parentElement?.querySelector(".line-numbers");

      if (lineNumbersDiv) {
        lineNumbersDiv.innerHTML = Array.from(
          { length: lines },
          (_, i) => `<div>${i + 1}</div>`,
        ).join("");
      }
    },

    highlightSyntax() {
      const keywords = [
        "def",
        "class",
        "import",
        "from",
        "if",
        "else",
        "elif",
        "for",
        "while",
        "return",
        "yield",
        "try",
        "except",
        "finally",
        "with",
        "as",
        "lambda",
        "pass",
        "break",
        "continue",
      ];
    },

    autoResize() {
      this.el.style.height = "auto";
      this.el.style.height = Math.max(80, this.el.scrollHeight) + "px";
    },

    addFocusAnimation() {
      this.el.parentElement?.classList.add("code-editor-focused");
      setTimeout(() => {
        this.el.parentElement?.classList.remove("code-editor-focused");
      }, 300);
    },

    addExecutionFlash() {
      const flash = document.createElement("div");
      flash.className =
        "absolute inset-0 bg-indigo-500 opacity-20 pointer-events-none animate-flash";
      this.el.parentElement?.appendChild(flash);
      setTimeout(() => flash.remove(), 500);
    },

    triggerAutoSave() {
      if (this.autoSaveTimeout) clearTimeout(this.autoSaveTimeout);
      this.autoSaveTimeout = setTimeout(() => {
        this.pushEvent("trigger_autosave", {});
      }, 2000);
    },

    showSaveNotification() {
      const notification = document.createElement("div");
      notification.className =
        "fixed top-20 right-6 bg-green-500 text-white px-4 py-2 rounded-lg shadow-lg z-50 animate-slide-in";
      notification.textContent = "Notebook saved!";
      document.body.appendChild(notification);
      setTimeout(() => notification.remove(), 2000);
    },

    indentSelection(direction) {
      const start = this.el.selectionStart;
      const end = this.el.selectionEnd;
      const value = this.el.value;
      const selectedText = value.substring(start, end);
      const lines = selectedText.split("\n");

      const indentedLines = lines.map((line) => {
        if (direction > 0) {
          return "    " + line;
        } else {
          return line.startsWith("    ") ? line.substring(4) : line;
        }
      });

      this.el.value =
        value.substring(0, start) +
        indentedLines.join("\n") +
        value.substring(end);

      this.el.dispatchEvent(new Event("input"));
    },

    toggleComment() {
      const start = this.el.selectionStart;
      const end = this.el.selectionEnd;
      const value = this.el.value;
      const selectedText = value.substring(start, end);
      const lines = selectedText.split("\n");

      const allCommented = lines.every((line) => line.trim().startsWith("#"));

      const toggledLines = lines.map((line) => {
        if (allCommented) {
          return line.replace(/^\s*#\s?/, "");
        } else {
          return "# " + line;
        }
      });

      this.el.value =
        value.substring(0, start) +
        toggledLines.join("\n") +
        value.substring(end);

      this.el.dispatchEvent(new Event("input"));
    },

    setupAutocomplete() {
      let autocompleteDiv = null;

      this.el.addEventListener("input", (e) => {
        const cursorPos = this.el.selectionStart;
        const textBeforeCursor = this.el.value.substring(0, cursorPos);
        const lastWord = textBeforeCursor.split(/\s/).pop();

        if (lastWord.length > 2) {
          const suggestions = this.getSuggestions(lastWord);

          if (suggestions.length > 0) {
            if (!autocompleteDiv) {
              autocompleteDiv = document.createElement("div");
              autocompleteDiv.className =
                "absolute bg-white border border-slate-200 rounded-lg shadow-xl z-50 py-1 max-h-48 overflow-y-auto";
              autocompleteDiv.style.cssText = `
                font-family: 'JetBrains Mono', monospace;
                font-size: 0.875rem;
              `;
            }

            autocompleteDiv.innerHTML = suggestions
              .map(
                (s) =>
                  `<div class="px-3 py-1.5 hover:bg-indigo-50 cursor-pointer text-slate-700">${s}</div>`,
              )
              .join("");

            const rect = this.el.getBoundingClientRect();
            autocompleteDiv.style.top = rect.bottom + window.scrollY + "px";
            autocompleteDiv.style.left = rect.left + window.scrollX + "px";

            if (!autocompleteDiv.parentElement) {
              document.body.appendChild(autocompleteDiv);
            }
          } else if (autocompleteDiv) {
            autocompleteDiv.remove();
            autocompleteDiv = null;
          }
        } else if (autocompleteDiv) {
          autocompleteDiv.remove();
          autocompleteDiv = null;
        }
      });
    },

    getSuggestions(word) {
      const pythonKeywords = [
        "print",
        "import",
        "from",
        "def",
        "class",
        "if",
        "else",
        "elif",
        "for",
        "while",
        "return",
        "yield",
        "try",
        "except",
        "finally",
        "with",
        "as",
        "lambda",
      ];

      const commonLibraries = [
        "numpy",
        "pandas",
        "matplotlib",
        "seaborn",
        "sklearn",
        "tensorflow",
        "torch",
      ];

      return [...pythonKeywords, ...commonLibraries].filter((k) =>
        k.startsWith(word.toLowerCase()),
      );
    },
  },

  NotebookWebSocket: {
    mounted() {
      const sessionId = this.el.dataset.sessionId;
      if (!sessionId) return;

      this.connect(sessionId);
      this.setupReconnection();
    },

    destroyed() {
      this.disconnect();
    },

    connect(sessionId) {
      const wsUrl = `ws://localhost:2020/ws/notebooks/${sessionId}`;
      this.ws = new WebSocket(wsUrl);

      this.ws.onopen = () => {
        console.log("✅ WebSocket connected to kernel");
        this.pushEvent("kernel_status", { status: "connected" });
        this.startHeartbeat();
        this.showConnectionStatus("connected");
      };

      this.ws.onmessage = (event) => {
        const message = JSON.parse(event.data);
        this.handleMessage(message);
      };

      this.ws.onerror = (error) => {
        console.error("❌ WebSocket error:", error);
        this.pushEvent("kernel_status", { status: "error" });
        this.showConnectionStatus("error");
      };

      this.ws.onclose = () => {
        console.log("🔌 WebSocket disconnected");
        this.pushEvent("kernel_status", { status: "disconnected" });
        this.stopHeartbeat();
        this.showConnectionStatus("disconnected");
        this.attemptReconnect();
      };
    },

    disconnect() {
      this.stopHeartbeat();
      if (this.reconnectInterval) {
        clearInterval(this.reconnectInterval);
      }
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
          this.animateCellExecution(message.cell_id, "start");
          break;

        case "Output":
          this.pushEvent("cell_output", {
            cell_id: message.cell_id,
            execution_id: message.execution_id,
            output: message.output,
          });
          this.animateOutput(message.cell_id);
          break;

        case "ExecutionComplete":
          this.pushEvent("execution_complete", {
            cell_id: message.cell_id,
            execution_id: message.execution_id,
            status: message.status,
            execution_time_ms: message.execution_time_ms,
          });
          this.animateCellExecution(message.cell_id, "complete");
          this.showExecutionTime(message.cell_id, message.execution_time_ms);
          break;

        case "Error":
          this.pushEvent("execution_error", {
            cell_id: message.cell_id,
            error: message.error,
          });
          this.animateCellExecution(message.cell_id, "error");
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

    setupReconnection() {
      this.reconnectAttempts = 0;
      this.maxReconnectAttempts = 5;
    },

    attemptReconnect() {
      if (this.reconnectAttempts < this.maxReconnectAttempts) {
        this.reconnectAttempts++;
        const delay = Math.min(
          1000 * Math.pow(2, this.reconnectAttempts),
          30000,
        );

        console.log(
          `🔄 Attempting to reconnect (${this.reconnectAttempts}/${this.maxReconnectAttempts}) in ${delay}ms`,
        );

        setTimeout(() => {
          const sessionId = this.el.dataset.sessionId;
          if (sessionId) {
            this.connect(sessionId);
          }
        }, delay);
      } else {
        console.error("❌ Max reconnection attempts reached");
        this.showConnectionStatus("failed");
      }
    },

    showConnectionStatus(status) {
      const statusMap = {
        connected: {
          color: "bg-green-500",
          text: "Connected to kernel",
          icon: "✓",
        },
        disconnected: {
          color: "bg-yellow-500",
          text: "Disconnected from kernel",
          icon: "⚠",
        },
        error: { color: "bg-red-500", text: "Connection error", icon: "✕" },
        failed: {
          color: "bg-red-600",
          text: "Connection failed",
          icon: "✕",
        },
      };

      const config = statusMap[status];
      if (!config) return;

      const notification = document.createElement("div");
      notification.className = `fixed bottom-6 right-6 ${config.color} text-white px-4 py-3 rounded-lg shadow-lg z-50 flex items-center space-x-2 animate-slide-up`;
      notification.innerHTML = `
        <span class="font-bold">${config.icon}</span>
        <span>${config.text}</span>
      `;

      document.body.appendChild(notification);
      setTimeout(() => notification.remove(), 3000);
    },

    animateCellExecution(cellId, stage) {
      const cellContainer = document.getElementById(`cell-container-${cellId}`);
      if (!cellContainer) return;

      switch (stage) {
        case "start":
          cellContainer.classList.add(
            "ring-2",
            "ring-indigo-400",
            "ring-opacity-50",
          );
          break;
        case "complete":
          cellContainer.classList.remove(
            "ring-2",
            "ring-indigo-400",
            "ring-opacity-50",
          );
          cellContainer.classList.add(
            "ring-2",
            "ring-green-400",
            "ring-opacity-50",
          );
          setTimeout(() => {
            cellContainer.classList.remove(
              "ring-2",
              "ring-green-400",
              "ring-opacity-50",
            );
          }, 1000);
          break;
        case "error":
          cellContainer.classList.remove(
            "ring-2",
            "ring-indigo-400",
            "ring-opacity-50",
          );
          cellContainer.classList.add(
            "ring-2",
            "ring-red-400",
            "ring-opacity-50",
          );
          setTimeout(() => {
            cellContainer.classList.remove(
              "ring-2",
              "ring-red-400",
              "ring-opacity-50",
            );
          }, 2000);
          break;
      }
    },

    animateOutput(cellId) {
      const cellContainer = document.getElementById(`cell-container-${cellId}`);
      if (!cellContainer) return;

      const outputs = cellContainer.querySelectorAll("[class*='output']");
      outputs.forEach((output) => {
        output.classList.add("animate-fade-in");
      });
    },

    showExecutionTime(cellId, timeMs) {
      const cellContainer = document.getElementById(`cell-container-${cellId}`);
      if (!cellContainer) return;

      let timeDisplay = cellContainer.querySelector(".execution-time");
      if (!timeDisplay) {
        timeDisplay = document.createElement("div");
        timeDisplay.className =
          "execution-time text-xs text-slate-400 mt-1 font-mono";
        const gutter = cellContainer.querySelector("[class*='w-20']");
        if (gutter) {
          gutter.appendChild(timeDisplay);
        }
      }

      timeDisplay.textContent = `${timeMs}ms`;
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
      this.saveIndicator = this.createSaveIndicator();

      this.handleEvent("trigger_autosave", () => {
        if (this.timeout) clearTimeout(this.timeout);

        this.showSaving();

        this.timeout = setTimeout(() => {
          this.pushEvent("save_notebook", {});
        }, 2000);
      });
    },

    destroyed() {
      if (this.timeout) {
        clearTimeout(this.timeout);
      }
      if (this.saveIndicator) {
        this.saveIndicator.remove();
      }
    },

    createSaveIndicator() {
      const indicator = document.createElement("div");
      indicator.className =
        "fixed top-6 right-6 bg-white border border-slate-200 rounded-lg shadow-lg px-4 py-2 z-50 hidden";
      indicator.innerHTML = `
        <div class="flex items-center space-x-2">
          <div class="w-2 h-2 bg-blue-500 rounded-full animate-pulse"></div>
          <span class="text-sm text-slate-700">Saving...</span>
        </div>
      `;
      document.body.appendChild(indicator);
      return indicator;
    },

    showSaving() {
      if (this.saveIndicator) {
        this.saveIndicator.classList.remove("hidden");
        setTimeout(() => {
          this.saveIndicator.classList.add("hidden");
        }, 2000);
      }
    },
  },

  MarkdownRenderer: {
    mounted() {
      this.render();
      this.setupEditMode();
    },

    updated() {
      this.render();
    },

    setupEditMode() {
      this.isEditing = false;

      this.el.addEventListener("dblclick", () => {
        this.enterEditMode();
      });
    },

    render() {
      const content = this.el.dataset.content;
      if (content && window.marked && !this.isEditing) {
        marked.setOptions({
          breaks: true,
          gfm: true,
          highlight: function (code, lang) {
            if (window.hljs && lang) {
              try {
                return window.hljs.highlight(code, { language: lang }).value;
              } catch (e) {
                return code;
              }
            }
            return code;
          },
        });

        this.el.innerHTML = marked.parse(content);
        this.el.classList.add(
          "prose",
          "prose-sm",
          "max-w-none",
          "prose-indigo",
        );
      }
    },

    enterEditMode() {
      this.isEditing = true;
      const content = this.el.dataset.content;

      const textarea = document.createElement("textarea");
      textarea.value = content;
      textarea.className = this.el.querySelector("textarea")?.className || "";
      textarea.addEventListener("blur", () => {
        this.isEditing = false;
        this.render();
      });

      this.el.innerHTML = "";
      this.el.appendChild(textarea);
      textarea.focus();
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
        window.Plotly.newPlot(
          this.el,
          plotData.data,
          {
            ...plotData.layout,
            paper_bgcolor: "rgba(0,0,0,0)",
            plot_bgcolor: "rgba(0,0,0,0)",
          },
          {
            responsive: true,
            displayModeBar: true,
            modeBarButtonsToRemove: ["toImage"],
          },
        );
      } else if (plotData.format === "matplotlib" && plotData.image) {
        const img = document.createElement("img");
        img.src = plotData.image;
        img.className = "max-w-full h-auto rounded-xl shadow-md";
        this.el.innerHTML = "";
        this.el.appendChild(img);
      }
    },
  },

  CellFocus: {
    mounted() {
      this.setupFocusHandling();
      this.setupDragDrop();
    },

    setupFocusHandling() {
      this.el.addEventListener("click", (e) => {
        const cellId = this.el.dataset.cellId;
        if (cellId) {
          this.pushEvent("select_cell", { cell_id: cellId });
          this.highlightCell();
        }
      });

      const textarea = this.el.querySelector("textarea");
      if (textarea) {
        textarea.addEventListener("focus", () => {
          const cellId = this.el.dataset.cellId;
          if (cellId) {
            this.pushEvent("select_cell", { cell_id: cellId });
            this.highlightCell();
          }
        });
      }
    },

    setupDragDrop() {
      this.el.draggable = true;

      this.el.addEventListener("dragstart", (e) => {
        e.dataTransfer.effectAllowed = "move";
        e.dataTransfer.setData("text/html", this.el.innerHTML);
        this.el.classList.add("opacity-50");
      });

      this.el.addEventListener("dragend", () => {
        this.el.classList.remove("opacity-50");
      });

      this.el.addEventListener("dragover", (e) => {
        if (e.preventDefault) {
          e.preventDefault();
        }
        e.dataTransfer.dropEffect = "move";
        this.el.classList.add("border-t-4", "border-indigo-500");
        return false;
      });

      this.el.addEventListener("dragleave", () => {
        this.el.classList.remove("border-t-4", "border-indigo-500");
      });

      this.el.addEventListener("drop", (e) => {
        if (e.stopPropagation) {
          e.stopPropagation();
        }
        this.el.classList.remove("border-t-4", "border-indigo-500");
        return false;
      });
    },

    highlightCell() {
      this.el.classList.add("ring-4", "ring-indigo-100");
      setTimeout(() => {
        this.el.classList.remove("ring-4", "ring-indigo-100");
      }, 300);
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

        if (e.key === "r" && (e.ctrlKey || e.metaKey) && e.shiftKey) {
          e.preventDefault();
          this.pushEvent("run_all_cells", {});
        }

        if (e.key === "a" && e.altKey) {
          e.preventDefault();
          this.pushEvent("add_cell_below", { type: "code" });
        }

        if (e.key === "m" && e.altKey) {
          e.preventDefault();
          this.pushEvent("add_cell_below", { type: "markdown" });
        }

        if (e.key === "d" && e.altKey && e.shiftKey) {
          e.preventDefault();
          this.pushEvent("delete_selected_cell", {});
        }
      };

      document.addEventListener("keydown", this.keydownHandler);
    },
  },
};

export default NotebookHooks;
