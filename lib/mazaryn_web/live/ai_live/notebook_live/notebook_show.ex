defmodule MazarynWeb.AiLive.NotebookShow do
  use MazarynWeb, :live_view
  alias Account.Users
  alias Core.NotebookClient
  alias Mazaryn.Schema.Notebook
  require Logger

  import MazarynWeb.AiLive.NotebookShowHelpers

  @impl true
  def mount(%{"id" => notebook_id}, %{"session_uuid" => session_uuid}, socket) do
    Logger.info("=== NotebookShow Mount Started ===")
    Logger.info("Notebook ID: #{inspect(notebook_id)}")
    Logger.info("Session UUID: #{inspect(session_uuid)}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      Logger.info("User found: #{inspect(user.id)}")

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
           |> put_flash(:error, "Error loading notebook")
           |> redirect(to: "/en/ai/notebooks")}

        notebook when is_tuple(notebook) ->
          Logger.info("Notebook fetched successfully")

          case Notebook.erl_changeset(notebook) do
            %Ecto.Changeset{valid?: true} = changeset ->
              notebook_struct = Ecto.Changeset.apply_changes(changeset)
              user_id = to_string(user.id)

              can_view =
                Notebook.is_public?(notebook_struct) or
                  Notebook.is_owner?(notebook_struct, user_id) or
                  Notebook.is_collaborator?(notebook_struct, user_id)

              if can_view do
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

                if connected?(socket) do
                  Logger.info("Socket connected - user can connect kernel manually")
                  Phoenix.PubSub.subscribe(Mazaryn.PubSub, "notebook:#{notebook_id}")
                  {:ok, schedule_auto_save(socket)}
                else
                  {:ok, socket}
                end
              else
                Logger.warning("User #{user_id} does not have permission")

                {:ok,
                 socket
                 |> put_flash(:error, "You don't have permission to view this notebook")
                 |> redirect(to: "/en/ai/notebooks")}
              end

            changeset ->
              Logger.error("Invalid changeset: #{inspect(changeset.errors)}")

              {:ok,
               socket
               |> put_flash(:error, "Failed to load notebook")
               |> redirect(to: "/en/ai/notebooks")}
          end

        other ->
          Logger.error("Unexpected result: #{inspect(other)}")

          {:ok,
           socket
           |> put_flash(:error, "Error loading notebook")
           |> redirect(to: "/en/ai/notebooks")}
      end
    else
      {:error, reason} ->
        Logger.error("User session error: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired. Please log in again.")
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
         |> put_flash(:error, "User not found. Please log in.")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(_params, url, socket) do
    {:noreply, assign(socket, current_path: URI.parse(url).path)}
  end

  @impl true
  def handle_event("connect_kernel", _params, socket) do
    notebook_id = socket.assigns.notebook_id
    user_id = to_string(socket.assigns.user.id)
    language = get_notebook_language(socket.assigns.notebook)

    Logger.info("Attempting to connect kernel: language=#{language}")

    case NotebookClient.create_session(notebook_id, user_id, language) do
      {:ok, session_id, kernel_id} ->
        Logger.info("Kernel connected: session=#{session_id}, kernel=#{kernel_id}")

        {:noreply,
         socket
         |> assign(:session_id, session_id)
         |> assign(:kernel_id, kernel_id)
         |> assign(:kernel_status, :idle)
         |> put_flash(:info, "Kernel connected successfully")}

      {:error, reason} ->
        Logger.error("Failed to connect kernel: #{inspect(reason)}")

        error_message = case reason do
          :kernel_creation_failed -> "Failed to start kernel. Make sure #{language} is installed."
          _ -> "Failed to connect kernel: #{inspect(reason)}"
        end

        {:noreply,
         socket
         |> put_flash(:error, error_message)}
    end
  end

  @impl true
  def handle_event("disconnect_kernel", _params, socket) do
    case socket.assigns.session_id do
      nil ->
        {:noreply, socket}

      session_id ->
        NotebookClient.close_session(session_id)

        {:noreply,
         socket
         |> assign(:session_id, nil)
         |> assign(:kernel_id, nil)
         |> assign(:kernel_status, :idle)
         |> put_flash(:info, "Kernel disconnected")}
    end
  end

  @impl true
  def handle_event("restart_kernel", _params, socket) do
    case socket.assigns.session_id do
      nil ->
        {:noreply, put_flash(socket, :error, "No kernel connected")}

      session_id ->
        case NotebookClient.restart_kernel(session_id) do
          :ok ->
            {:noreply,
             socket
             |> assign(:kernel_status, :idle)
             |> put_flash(:info, "Kernel restarted successfully")}

          {:error, reason} ->
            {:noreply, put_flash(socket, :error, "Failed to restart kernel: #{inspect(reason)}")}
        end
    end
  end

  @impl true
  def handle_event("interrupt_kernel", _params, socket) do
    case socket.assigns.session_id do
      nil ->
        {:noreply, put_flash(socket, :error, "No kernel connected")}

      session_id ->
        case NotebookClient.interrupt_kernel(session_id) do
          :ok ->
            {:noreply, put_flash(socket, :info, "Kernel interrupted")}

          {:error, reason} ->
            {:noreply, put_flash(socket, :error, "Failed to interrupt: #{inspect(reason)}")}
        end
    end
  end

  @impl true
  def handle_event("add_cell", %{"type" => type, "position" => position}, socket) do
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
  def handle_event("update_cell", %{"cell_id" => cell_id, "value" => content}, socket) do
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
    case socket.assigns.session_id do
      nil ->
        {:noreply, put_flash(socket, :error, "Please connect kernel first")}

      _session_id ->
        cell = Enum.find(socket.assigns.cells, fn c -> c.id == cell_id end)

        if cell && cell.type == :code do
          executing_cells = MapSet.put(socket.assigns.executing_cells, cell_id)
          send(self(), {:execute_cell_async, cell_id, cell.content})

          {:noreply,
           socket
           |> assign(executing_cells: executing_cells)
           |> assign(kernel_status: :busy)}
        else
          {:noreply, socket}
        end
    end
  end

  @impl true
  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    cells = Enum.reject(socket.assigns.cells, fn cell -> cell.id == cell_id end)

    final_cells = if cells == [] do
      [create_default_cell()]
    else
      cells
    end

    {:noreply,
     socket
     |> assign(cells: final_cells)
     |> assign(unsaved_changes: true)
     |> broadcast_cell_update()}
  end

  @impl true
  def handle_event("move_cell_up", %{"cell_id" => cell_id}, socket) do
    cells = move_cell(socket.assigns.cells, cell_id, :up)
    {:noreply, assign(socket, cells: cells, unsaved_changes: true)}
  end

  @impl true
  def handle_event("move_cell_down", %{"cell_id" => cell_id}, socket) do
    cells = move_cell(socket.assigns.cells, cell_id, :down)
    {:noreply, assign(socket, cells: cells, unsaved_changes: true)}
  end

  @impl true
  def handle_event("toggle_add_cell_menu", %{"position" => position}, socket) do
    position_int = String.to_integer(position)
    {:noreply, assign(socket, show_add_cell_menu: !socket.assigns.show_add_cell_menu, add_cell_position: position_int)}
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
    case socket.assigns.session_id do
      nil ->
        {:noreply, put_flash(socket, :error, "Please connect kernel first")}

      _session_id ->
        code_cells =
          socket.assigns.cells
          |> Enum.filter(fn cell -> cell.type == :code end)
          |> Enum.map(& &1.id)

        executing_cells = MapSet.new(code_cells)
        send(self(), :execute_all_cells)

        {:noreply,
         socket
         |> assign(executing_cells: executing_cells)
         |> assign(kernel_status: :busy)}
    end
  end

  @impl true
  def handle_event("run_cells_above", %{"cell_id" => cell_id}, socket) do
    case socket.assigns.session_id do
      nil ->
        {:noreply, put_flash(socket, :error, "Please connect kernel first")}

      _session_id ->
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
  end

  @impl true
  def handle_event("run_cells_below", %{"cell_id" => cell_id}, socket) do
    case socket.assigns.session_id do
      nil ->
        {:noreply, put_flash(socket, :error, "Please connect kernel first")}

      _session_id ->
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
  end

  @impl true
  def handle_event("clear_cell_output", %{"cell_id" => cell_id}, socket) do
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
    cells = Enum.map(socket.assigns.cells, fn cell -> %{cell | outputs: [], execution_count: nil} end)
    {:noreply, assign(socket, cells: cells)}
  end

  @impl true
  def handle_event("save_notebook", _params, socket) do
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
    socket = if new_state, do: schedule_auto_save(socket), else: socket
    {:noreply, assign(socket, auto_save_enabled: new_state)}
  end

  @impl true
  def handle_event("trigger_autosave", _params, socket) do
    if socket.assigns.auto_save_enabled && socket.assigns.unsaved_changes do
      send(self(), :save_notebook_async)
    end
    {:noreply, socket}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info({:execute_cell_async, cell_id, code}, socket) do
    case NotebookClient.execute_code(socket.assigns.session_id, code, cell_id) do
      {:ok, result} ->
        execution_count = get_next_execution_count(socket)

        cells =
          Enum.map(socket.assigns.cells, fn cell ->
            if cell.id == cell_id do
              %{cell | outputs: parse_outputs(result.outputs), execution_count: execution_count}
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
         |> assign(execution_history: [execution_entry | Enum.take(socket.assigns.execution_history, 49)])}

      {:error, reason} ->
        Logger.error("Execution failed: #{inspect(reason)}")
        executing_cells = MapSet.delete(socket.assigns.executing_cells, cell_id)
        kernel_status = if MapSet.size(executing_cells) == 0, do: :idle, else: :busy

        {:noreply,
         socket
         |> assign(executing_cells: executing_cells)
         |> assign(kernel_status: kernel_status)
         |> put_flash(:error, "Execution failed")}
    end
  end

  @impl true
  def handle_info(:execute_all_cells, socket) do
    code_cells = socket.assigns.cells |> Enum.filter(fn cell -> cell.type == :code end)
    Enum.each(code_cells, fn cell -> send(self(), {:execute_cell_async, cell.id, cell.content}) end)
    {:noreply, socket}
  end

  @impl true
  def handle_info({:execute_cells_list, cell_ids}, socket) do
    Enum.each(cell_ids, fn cell_id ->
      cell = Enum.find(socket.assigns.cells, fn c -> c.id == cell_id end)
      if cell, do: send(self(), {:execute_cell_async, cell.id, cell.content})
    end)
    {:noreply, socket}
  end

  @impl true
  def handle_info(:save_notebook_async, socket) do
    cells_json = Jason.encode!(socket.assigns.cells)

    case NotebookClient.update_notebook_content(socket.assigns.notebook_id, to_string(socket.assigns.user.id), cells_json) do
      :ok ->
        {:noreply, socket |> assign(last_saved: DateTime.utc_now(), unsaved_changes: false) |> put_flash(:info, "Saved")}
      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Save failed")}
    end
  end

  @impl true
  def handle_info(:auto_save_tick, socket) do
    if socket.assigns.auto_save_enabled && socket.assigns.unsaved_changes do
      send(self(), :save_notebook_async)
    end
    {:noreply, schedule_auto_save(socket)}
  end

  @impl true
  def handle_info({:cell_updated, _cells}, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_info(_msg, socket) do
    {:noreply, socket}
  end

  @impl true
  def terminate(reason, socket) do
    if socket.assigns[:session_id] do
      NotebookClient.close_session(socket.assigns.session_id)
    end
    :ok
  end

  defp schedule_auto_save(socket) do
    Process.send_after(self(), :auto_save_tick, 30_000)
    socket
  end

  defp broadcast_cell_update(socket) do
    Phoenix.PubSub.broadcast(Mazaryn.PubSub, "notebook:#{socket.assigns.notebook_id}", {:cell_updated, socket.assigns.cells})
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
          {:ok, content} when is_binary(content) ->
            Logger.debug("Content loaded as binary, length: #{String.length(content)}")
            parse_json_content(content)

          {:ok, content} when is_list(content) ->
            content_str = List.to_string(content)
            Logger.debug("Content loaded as charlist, converting to string")
            parse_json_content(content_str)

          {:error, reason} ->
            Logger.error("Failed to load notebook content: #{inspect(reason)}")
            [create_default_cell()]

          content when is_binary(content) ->
            Logger.debug("Content loaded directly as binary")
            parse_json_content(content)

          content when is_list(content) ->
            content_str = List.to_string(content)
            Logger.debug("Content loaded directly as charlist")
            parse_json_content(content_str)

          other ->
            Logger.error("Unexpected content format: #{inspect(other)}")
            [create_default_cell()]
        end
    end
  end

  defp parse_json_content(content_str) do
    case Jason.decode(content_str) do
      {:ok, cells} when is_list(cells) ->
        Logger.info("Successfully parsed #{length(cells)} cells from JSON")
        Enum.map(cells, &parse_cell/1)

      {:error, reason} ->
        Logger.error("Failed to parse cells JSON: #{inspect(reason)}")
        Logger.error("Content preview: #{String.slice(content_str, 0, 200)}")
        [create_default_cell()]

      _ ->
        Logger.error("Unexpected JSON decode result")
        [create_default_cell()]
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
      outputs: parse_saved_outputs(cell["outputs"] || []),
      execution_count: cell["execution_count"],
      metadata: cell["metadata"] || %{},
      created_at: parse_datetime(cell["created_at"])
    }
  end

  defp parse_saved_outputs(outputs) when is_list(outputs) do
    Enum.map(outputs, fn output ->
      case output do
        %{"type" => type, "data" => data} ->
          %{
            type: String.to_atom(type),
            data: parse_saved_output_data(data)
          }
        %{type: type, data: data} ->
          %{type: type, data: data}
        _ ->
          %{type: :text, data: %{type: :text, value: inspect(output)}}
      end
    end)
  end
  defp parse_saved_outputs(_), do: []

  defp parse_saved_output_data(%{"type" => type, "value" => value}) do
    %{type: String.to_atom(type), value: value}
  end
  defp parse_saved_output_data(%{type: type, value: value}) do
    %{type: type, value: value}
  end
  defp parse_saved_output_data(data), do: %{type: :unknown, value: inspect(data)}

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
      %{type: output[:output_type] || :stream, data: parse_output_data(output[:data])}
    end)
  end
  defp parse_outputs(_), do: []

  defp parse_output_data(%{type: :text, value: text}), do: %{type: :text, value: text}
  defp parse_output_data(%{type: :html, value: html}), do: %{type: :html, value: html}
  defp parse_output_data(%{type: :json, value: json}), do: %{type: :json, value: json}
  defp parse_output_data({:text, text}), do: %{type: :text, value: to_string(text)}
  defp parse_output_data({:html, html}), do: %{type: :html, value: to_string(html)}
  defp parse_output_data({:json, json}), do: %{type: :json, value: json}
  defp parse_output_data(data), do: %{type: :unknown, value: inspect(data)}

  defp generate_cell_id, do: "cell_#{:crypto.strong_rand_bytes(8) |> Base.encode16(case: :lower)}"

  defp move_cell(cells, cell_id, direction) do
    index = Enum.find_index(cells, fn cell -> cell.id == cell_id end)
    if index do
      new_index = case direction do
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

  defp get_next_execution_count(socket) do
    socket.assigns.cells |> Enum.map(& &1.execution_count) |> Enum.reject(&is_nil/1) |> Enum.max(fn -> 0 end) |> Kernel.+(1)
  end

  defp get_notebook_language(notebook) do
    case notebook.language do
      lang when is_binary(lang) -> String.to_atom(lang)
      lang when is_atom(lang) -> lang
      _ -> :python
    end
  end
end
