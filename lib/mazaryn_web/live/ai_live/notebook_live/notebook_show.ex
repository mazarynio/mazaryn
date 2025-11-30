defmodule MazarynWeb.AiLive.NotebookShow do
  use MazarynWeb, :live_view
  alias Account.Users
  alias Core.NotebookClient
  alias Mazaryn.Schema.Notebook
  require Logger

  @impl true
  def mount(%{"id" => notebook_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("=== NotebookShow Mount Started ===")
    Logger.info("Notebook ID: #{inspect(notebook_id)}")
    Logger.info("Session UUID: #{inspect(session_uuid)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      Logger.info("User found: #{inspect(user.id)}")

      Logger.info("Fetching notebook from database...")

      case NotebookClient.get_notebook_by_id(notebook_id) do
        {:error, :notebook_not_found} ->
          Logger.error("Notebook not found with ID: #{notebook_id}")

          {:ok,
           socket
           |> put_flash(:error, "Notebook not found")
           |> redirect(to: "/en/ai/notebooks")}

        {:error, reason} ->
          Logger.error("Error fetching notebook: #{inspect(reason)}")

          {:ok,
           socket
           |> put_flash(:error, "Error loading notebook: #{inspect(reason)}")
           |> redirect(to: "/en/ai/notebooks")}

        notebook when is_tuple(notebook) ->
          Logger.info("Notebook fetched successfully: #{inspect(notebook)}")
          Logger.info("Converting notebook to changeset...")

          case Notebook.erl_changeset(notebook) do
            %Ecto.Changeset{valid?: true} = changeset ->
              notebook_struct = Ecto.Changeset.apply_changes(changeset)
              Logger.info("Notebook converted successfully")
              Logger.info("Notebook struct: #{inspect(notebook_struct)}")

              user_id = to_string(user.id)

              can_view =
                Notebook.is_public?(notebook_struct) or
                  Notebook.is_owner?(notebook_struct, user_id) or
                  Notebook.is_collaborator?(notebook_struct, user_id)

              Logger.info("Can view check: #{can_view}")
              Logger.info("Is public: #{Notebook.is_public?(notebook_struct)}")
              Logger.info("Is owner: #{Notebook.is_owner?(notebook_struct, user_id)}")

              Logger.info(
                "Is collaborator: #{Notebook.is_collaborator?(notebook_struct, user_id)}"
              )

              if can_view do
                if connected?(socket) do
                  Logger.info("Socket connected, creating kernel session...")

                  language_atom =
                    case notebook_struct.language do
                      lang when is_binary(lang) -> String.to_atom(lang)
                      lang when is_atom(lang) -> lang
                      _ -> :python
                    end

                  Logger.info("Language: #{inspect(language_atom)}")

                  try do
                    result = :notebook_client.create_session(notebook_id, user_id, language_atom)
                    Logger.info("Kernel session creation result: #{inspect(result)}")

                    case result do
                      {:ok, session_id, kernel_id} ->
                        Logger.info(
                          "Session created - Session ID: #{session_id}, Kernel ID: #{kernel_id}"
                        )

                        Phoenix.PubSub.subscribe(Mazaryn.PubSub, "notebook:#{notebook_id}")

                        socket =
                          socket
                          |> assign(user: user)
                          |> assign(notebook: notebook_struct)
                          |> assign(notebook_id: notebook_id)
                          |> assign(session_id: session_id)
                          |> assign(kernel_id: kernel_id)
                          |> assign(cells: load_cells(notebook_struct))
                          |> assign(executing_cells: MapSet.new())
                          |> assign(show_add_cell_menu: false)
                          |> assign(add_cell_position: nil)
                          |> assign(selected_cell: nil)
                          |> assign(kernel_status: :idle)
                          |> assign(locale: "en")
                          |> assign(show_settings: false)
                          |> assign(show_share_modal: false)
                          |> assign(show_export_modal: false)
                          |> assign(auto_save_enabled: true)
                          |> assign(last_saved: nil)
                          |> assign(unsaved_changes: false)
                          |> assign(cell_toolbar_visible: nil)
                          |> assign(execution_history: [])
                          |> schedule_auto_save()

                        Logger.info("Socket assigned successfully, mount complete")
                        {:ok, socket}

                      error ->
                        Logger.error("Failed to create kernel session: #{inspect(error)}")

                        {:ok,
                         socket
                         |> put_flash(:error, "Failed to create kernel session")
                         |> redirect(to: "/en/ai/notebooks")}
                    end
                  catch
                    kind, reason ->
                      Logger.error(
                        "Exception creating kernel session: #{kind} - #{inspect(reason)}"
                      )

                      Logger.error("Stacktrace: #{inspect(__STACKTRACE__)}")

                      {:ok,
                       socket
                       |> put_flash(:error, "Error initializing notebook")
                       |> redirect(to: "/en/ai/notebooks")}
                  end
                else
                  Logger.info("Socket not connected, skipping kernel creation")

                  socket =
                    socket
                    |> assign(user: user)
                    |> assign(notebook: notebook_struct)
                    |> assign(notebook_id: notebook_id)
                    |> assign(session_id: nil)
                    |> assign(kernel_id: nil)
                    |> assign(cells: load_cells(notebook_struct))
                    |> assign(executing_cells: MapSet.new())
                    |> assign(show_add_cell_menu: false)
                    |> assign(add_cell_position: nil)
                    |> assign(selected_cell: nil)
                    |> assign(kernel_status: :idle)
                    |> assign(locale: "en")
                    |> assign(show_settings: false)
                    |> assign(show_share_modal: false)
                    |> assign(show_export_modal: false)
                    |> assign(auto_save_enabled: true)
                    |> assign(last_saved: nil)
                    |> assign(unsaved_changes: false)
                    |> assign(cell_toolbar_visible: nil)
                    |> assign(execution_history: [])

                  {:ok, socket}
                end
              else
                Logger.warning(
                  "User #{user_id} does not have permission to view notebook #{notebook_id}"
                )

                {:ok,
                 socket
                 |> put_flash(:error, "You don't have permission to view this notebook")
                 |> redirect(to: "/en/ai/notebooks")}
              end

            changeset ->
              Logger.error("Invalid changeset: #{inspect(changeset)}")
              Logger.error("Changeset errors: #{inspect(changeset.errors)}")

              {:ok,
               socket
               |> put_flash(:error, "Failed to load notebook - invalid data format")
               |> redirect(to: "/en/ai/notebooks")}
          end

        other ->
          Logger.error("Unexpected notebook fetch result: #{inspect(other)}")

          {:ok,
           socket
           |> put_flash(:error, "Unexpected error loading notebook")
           |> redirect(to: "/en/ai/notebooks")}
      end
    else
      {:error, reason} ->
        Logger.error("User session error: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"id" => notebook_id}, %{"user_id" => user_id}, socket) do
    Logger.info("Mount with user_id: #{user_id}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        mount(%{"id" => notebook_id}, %{"session_uuid" => "legacy"}, assign(socket, user: user))

      {:error, reason} ->
        Logger.error("User not found: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(_params, url, socket) do
    {:noreply, assign(socket, current_path: URI.parse(url).path)}
  end

  @impl true
  def handle_event("add_cell", %{"type" => type, "position" => position}, socket) do
    Logger.info("Adding cell - Type: #{type}, Position: #{position}")
    cell_id = generate_cell_id()
    position_int = String.to_integer(position)

    new_cell = %{
      id: cell_id,
      type: String.to_atom(type),
      content: "",
      outputs: [],
      execution_count: nil,
      metadata: %{},
      created_at: DateTime.utc_now()
    }

    cells = List.insert_at(socket.assigns.cells, position_int, new_cell)
    Logger.info("Cell added successfully. Total cells: #{length(cells)}")

    {:noreply,
     socket
     |> assign(cells: cells)
     |> assign(show_add_cell_menu: false)
     |> assign(selected_cell: cell_id)
     |> assign(unsaved_changes: true)
     |> broadcast_cell_update()}
  end

  @impl true
  def handle_event("update_cell", %{"cell_id" => cell_id, "content" => content}, socket) do
    Logger.debug("Updating cell #{cell_id}")

    cells =
      Enum.map(socket.assigns.cells, fn cell ->
        if cell.id == cell_id do
          %{cell | content: content}
        else
          cell
        end
      end)

    {:noreply,
     socket
     |> assign(cells: cells)
     |> assign(unsaved_changes: true)}
  end

  @impl true
  def handle_event("execute_cell", %{"cell_id" => cell_id}, socket) do
    Logger.info("Executing cell #{cell_id}")
    cell = Enum.find(socket.assigns.cells, fn c -> c.id == cell_id end)

    if cell && cell.type == :code do
      executing_cells = MapSet.put(socket.assigns.executing_cells, cell_id)
      Logger.info("Cell execution started. Code: #{String.slice(cell.content, 0, 100)}")

      send(self(), {:execute_cell_async, cell_id, cell.content})

      {:noreply,
       socket
       |> assign(executing_cells: executing_cells)
       |> assign(kernel_status: :busy)}
    else
      Logger.warning("Cannot execute cell #{cell_id} - not a code cell or not found")
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    Logger.info("Deleting cell #{cell_id}")
    cells = Enum.reject(socket.assigns.cells, fn cell -> cell.id == cell_id end)
    Logger.info("Cell deleted. Remaining cells: #{length(cells)}")

    {:noreply,
     socket
     |> assign(cells: cells)
     |> assign(unsaved_changes: true)
     |> broadcast_cell_update()}
  end

  @impl true
  def handle_event("move_cell_up", %{"cell_id" => cell_id}, socket) do
    Logger.debug("Moving cell up: #{cell_id}")
    cells = move_cell(socket.assigns.cells, cell_id, :up)

    {:noreply,
     socket
     |> assign(cells: cells)
     |> assign(unsaved_changes: true)}
  end

  @impl true
  def handle_event("move_cell_down", %{"cell_id" => cell_id}, socket) do
    Logger.debug("Moving cell down: #{cell_id}")
    cells = move_cell(socket.assigns.cells, cell_id, :down)

    {:noreply,
     socket
     |> assign(cells: cells)
     |> assign(unsaved_changes: true)}
  end

  @impl true
  def handle_event("toggle_add_cell_menu", %{"position" => position}, socket) do
    position_int = String.to_integer(position)

    {:noreply,
     socket
     |> assign(show_add_cell_menu: !socket.assigns.show_add_cell_menu)
     |> assign(add_cell_position: position_int)}
  end

  @impl true
  def handle_event("close_add_cell_menu", _params, socket) do
    {:noreply, assign(socket, show_add_cell_menu: false)}
  end

  @impl true
  def handle_event("select_cell", %{"cell_id" => cell_id}, socket) do
    {:noreply, assign(socket, selected_cell: cell_id)}
  end

  @impl true
  def handle_event("show_cell_toolbar", %{"cell_id" => cell_id}, socket) do
    {:noreply, assign(socket, cell_toolbar_visible: cell_id)}
  end

  @impl true
  def handle_event("hide_cell_toolbar", _params, socket) do
    {:noreply, assign(socket, cell_toolbar_visible: nil)}
  end

  @impl true
  def handle_event("run_all_cells", _params, socket) do
    Logger.info("Running all cells")

    code_cells =
      socket.assigns.cells
      |> Enum.filter(fn cell -> cell.type == :code end)
      |> Enum.map(& &1.id)

    Logger.info("Found #{length(code_cells)} code cells to execute")
    executing_cells = MapSet.new(code_cells)

    send(self(), :execute_all_cells)

    {:noreply,
     socket
     |> assign(executing_cells: executing_cells)
     |> assign(kernel_status: :busy)}
  end

  @impl true
  def handle_event("run_cells_above", %{"cell_id" => cell_id}, socket) do
    Logger.info("Running cells above #{cell_id}")
    cell_index = Enum.find_index(socket.assigns.cells, fn c -> c.id == cell_id end)

    if cell_index do
      code_cells =
        socket.assigns.cells
        |> Enum.take(cell_index)
        |> Enum.filter(fn cell -> cell.type == :code end)
        |> Enum.map(& &1.id)

      executing_cells = MapSet.new(code_cells)

      send(self(), {:execute_cells_list, code_cells})

      {:noreply,
       socket
       |> assign(executing_cells: executing_cells)
       |> assign(kernel_status: :busy)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("run_cells_below", %{"cell_id" => cell_id}, socket) do
    Logger.info("Running cells below #{cell_id}")
    cell_index = Enum.find_index(socket.assigns.cells, fn c -> c.id == cell_id end)

    if cell_index do
      code_cells =
        socket.assigns.cells
        |> Enum.drop(cell_index + 1)
        |> Enum.filter(fn cell -> cell.type == :code end)
        |> Enum.map(& &1.id)

      executing_cells = MapSet.new(code_cells)

      send(self(), {:execute_cells_list, code_cells})

      {:noreply,
       socket
       |> assign(executing_cells: executing_cells)
       |> assign(kernel_status: :busy)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("clear_cell_output", %{"cell_id" => cell_id}, socket) do
    Logger.info("Clearing output for cell #{cell_id}")

    cells =
      Enum.map(socket.assigns.cells, fn cell ->
        if cell.id == cell_id do
          %{cell | outputs: [], execution_count: nil}
        else
          cell
        end
      end)

    {:noreply, assign(socket, cells: cells)}
  end

  @impl true
  def handle_event("clear_all_outputs", _params, socket) do
    Logger.info("Clearing all outputs")

    cells =
      Enum.map(socket.assigns.cells, fn cell ->
        %{cell | outputs: [], execution_count: nil}
      end)

    {:noreply, assign(socket, cells: cells)}
  end

  @impl true
  def handle_event("save_notebook", _params, socket) do
    Logger.info("Save notebook requested")
    send(self(), :save_notebook_async)
    {:noreply, put_flash(socket, :info, "Saving notebook...")}
  end

  @impl true
  def handle_event("toggle_settings", _params, socket) do
    {:noreply, assign(socket, show_settings: !socket.assigns.show_settings)}
  end

  @impl true
  def handle_event("toggle_share_modal", _params, socket) do
    {:noreply, assign(socket, show_share_modal: !socket.assigns.show_share_modal)}
  end

  @impl true
  def handle_event("toggle_export_modal", _params, socket) do
    {:noreply, assign(socket, show_export_modal: !socket.assigns.show_export_modal)}
  end

  @impl true
  def handle_event("toggle_auto_save", _params, socket) do
    new_state = !socket.assigns.auto_save_enabled
    Logger.info("Auto-save toggled: #{new_state}")

    socket =
      if new_state do
        schedule_auto_save(socket)
      else
        socket
      end

    {:noreply, assign(socket, auto_save_enabled: new_state)}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info({:execute_cell_async, cell_id, code}, socket) do
    Logger.info("Async cell execution started for #{cell_id}")
    Logger.debug("Session ID: #{inspect(socket.assigns.session_id)}")
    Logger.debug("Code length: #{String.length(code)} characters")

    case :notebook_client.execute_code(socket.assigns.session_id, code, cell_id) do
      {:ok, result} ->
        Logger.info("Cell execution successful for #{cell_id}")
        Logger.debug("Result: #{inspect(result)}")

        cells =
          Enum.map(socket.assigns.cells, fn cell ->
            if cell.id == cell_id do
              %{
                cell
                | outputs: parse_outputs(result.outputs),
                  execution_count: socket.assigns.notebook.execution_count + 1
              }
            else
              cell
            end
          end)

        executing_cells = MapSet.delete(socket.assigns.executing_cells, cell_id)
        kernel_status = if MapSet.size(executing_cells) == 0, do: :idle, else: :busy

        execution_entry = %{
          cell_id: cell_id,
          timestamp: DateTime.utc_now(),
          duration: result[:execution_time_ms] || 0,
          status: :success
        }

        {:noreply,
         socket
         |> assign(cells: cells)
         |> assign(executing_cells: executing_cells)
         |> assign(kernel_status: kernel_status)
         |> assign(
           execution_history: [execution_entry | Enum.take(socket.assigns.execution_history, 49)]
         )}

      {:error, reason} ->
        Logger.error("Failed to execute cell #{cell_id}: #{inspect(reason)}")
        executing_cells = MapSet.delete(socket.assigns.executing_cells, cell_id)

        {:noreply,
         socket
         |> assign(executing_cells: executing_cells)
         |> put_flash(:error, "Execution failed: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_info(:execute_all_cells, socket) do
    Logger.info("Executing all cells sequentially")

    code_cells =
      socket.assigns.cells
      |> Enum.filter(fn cell -> cell.type == :code end)

    Enum.each(code_cells, fn cell ->
      send(self(), {:execute_cell_async, cell.id, cell.content})
    end)

    {:noreply, socket}
  end

  @impl true
  def handle_info({:execute_cells_list, cell_ids}, socket) do
    Logger.info("Executing cells list: #{inspect(cell_ids)}")

    Enum.each(cell_ids, fn cell_id ->
      cell = Enum.find(socket.assigns.cells, fn c -> c.id == cell_id end)

      if cell do
        send(self(), {:execute_cell_async, cell.id, cell.content})
      end
    end)

    {:noreply, socket}
  end

  @impl true
  def handle_info(:save_notebook_async, socket) do
    Logger.info("Saving notebook #{socket.assigns.notebook_id}")
    cells_json = Jason.encode!(socket.assigns.cells)
    Logger.debug("Cells JSON length: #{String.length(cells_json)} characters")

    case NotebookClient.update_notebook_content(
           socket.assigns.notebook_id,
           to_string(socket.assigns.user.id),
           cells_json
         ) do
      :ok ->
        Logger.info("Notebook saved successfully")

        {:noreply,
         socket
         |> assign(last_saved: DateTime.utc_now())
         |> assign(unsaved_changes: false)
         |> put_flash(:info, "Notebook saved successfully")}

      {:error, reason} ->
        Logger.error("Failed to save notebook: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to save notebook: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_info(:auto_save_tick, socket) do
    if socket.assigns.auto_save_enabled && socket.assigns.unsaved_changes do
      Logger.debug("Auto-save triggered")
      send(self(), :save_notebook_async)
    end

    {:noreply, schedule_auto_save(socket)}
  end

  @impl true
  def terminate(reason, socket) do
    Logger.info("NotebookShow terminating: #{inspect(reason)}")

    if socket.assigns[:session_id] do
      Logger.info("Closing kernel session: #{socket.assigns.session_id}")
      :notebook_client.close_session(socket.assigns.session_id)
    end

    :ok
  end

  defp schedule_auto_save(socket) do
    Process.send_after(self(), :auto_save_tick, 30_000)
    socket
  end

  defp broadcast_cell_update(socket) do
    Phoenix.PubSub.broadcast(
      Mazaryn.PubSub,
      "notebook:#{socket.assigns.notebook_id}",
      {:cell_updated, socket.assigns.cells}
    )

    socket
  end

  defp load_cells(notebook) do
    Logger.info("Loading cells for notebook")

    case notebook.content_cid do
      nil ->
        Logger.info("No content CID, creating default cell")
        [create_default_cell()]

      _cid ->
        Logger.info("Fetching content from CID: #{notebook.content_cid}")

        case NotebookClient.get_notebook_content(notebook.id) do
          {:error, reason} ->
            Logger.error("Failed to load notebook content: #{inspect(reason)}")
            [create_default_cell()]

          content when is_binary(content) or is_list(content) ->
            content_str = if is_list(content), do: to_string(content), else: content
            Logger.debug("Content loaded, length: #{String.length(content_str)}")

            case Jason.decode(content_str) do
              {:ok, cells} when is_list(cells) ->
                Logger.info("Successfully parsed #{length(cells)} cells from JSON")
                Enum.map(cells, &parse_cell/1)

              {:error, reason} ->
                Logger.error("Failed to parse cells JSON: #{inspect(reason)}")
                [create_default_cell()]

              _ ->
                Logger.error("Unexpected JSON decode result")
                [create_default_cell()]
            end
        end
    end
  end

  defp create_default_cell do
    %{
      id: generate_cell_id(),
      type: :code,
      content: "",
      outputs: [],
      execution_count: nil,
      metadata: %{},
      created_at: DateTime.utc_now()
    }
  end

  defp parse_cell(cell) do
    %{
      id: cell["id"] || generate_cell_id(),
      type: String.to_atom(cell["type"] || "code"),
      content: cell["content"] || "",
      outputs: cell["outputs"] || [],
      execution_count: cell["execution_count"],
      metadata: cell["metadata"] || %{},
      created_at: parse_datetime(cell["created_at"])
    }
  end

  defp parse_datetime(nil), do: DateTime.utc_now()

  defp parse_datetime(dt_string) when is_binary(dt_string) do
    case DateTime.from_iso8601(dt_string) do
      {:ok, dt, _} -> dt
      _ -> DateTime.utc_now()
    end
  end

  defp parse_datetime(_), do: DateTime.utc_now()

  defp parse_outputs(outputs) when is_list(outputs) do
    Enum.map(outputs, fn output ->
      %{
        type: output[:output_type] || :stream,
        data: parse_output_data(output[:data])
      }
    end)
  end

  defp parse_outputs(_), do: []

  defp parse_output_data({:text, text}), do: %{type: :text, value: to_string(text)}
  defp parse_output_data({:html, html}), do: %{type: :html, value: to_string(html)}
  defp parse_output_data({:json, json}), do: %{type: :json, value: json}
  defp parse_output_data(data), do: %{type: :unknown, value: inspect(data)}

  defp generate_cell_id do
    "cell_#{:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)}"
  end

  defp move_cell(cells, cell_id, direction) do
    index = Enum.find_index(cells, fn cell -> cell.id == cell_id end)

    if index do
      new_index =
        case direction do
          :up -> max(0, index - 1)
          :down -> min(length(cells) - 1, index + 1)
        end

      if new_index != index do
        cell = Enum.at(cells, index)
        cells = List.delete_at(cells, index)
        List.insert_at(cells, new_index, cell)
      else
        cells
      end
    else
      cells
    end
  end

  @impl true
  def handle_event("update_cell", %{"cell_id" => cell_id, "value" => content}, socket) do
    Logger.debug("Updating cell #{cell_id} with value")

    cells =
      Enum.map(socket.assigns.cells, fn cell ->
        if cell.id == cell_id do
          %{cell | content: content}
        else
          cell
        end
      end)

    {:noreply,
     socket
     |> assign(cells: cells)
     |> assign(unsaved_changes: true)}
  end

  @impl true
  def handle_event("trigger_autosave", _params, socket) do
    Logger.debug("Auto-save triggered from hook")

    if socket.assigns.auto_save_enabled && socket.assigns.unsaved_changes do
      send(self(), :save_notebook_async)
    end

    {:noreply, socket}
  end

  defp language_badge_color("python"), do: "bg-blue-100 text-blue-700 border border-blue-200"
  defp language_badge_color("r"), do: "bg-purple-100 text-purple-700 border border-purple-200"
  defp language_badge_color("julia"), do: "bg-pink-100 text-pink-700 border border-pink-200"
  defp language_badge_color("scala"), do: "bg-red-100 text-red-700 border border-red-200"
  defp language_badge_color("sql"), do: "bg-green-100 text-green-700 border border-green-200"
  defp language_badge_color(_), do: "bg-slate-100 text-slate-700 border border-slate-200"
end
