defmodule MazarynWeb.AiLive.NotebookShow do
  use MazarynWeb, :live_view
  alias Account.Users
  alias Core.NotebookClient
  alias Mazaryn.Schema.Notebook
  require Logger

  @impl true
  def mount(%{"id" => notebook_id}, %{"session_uuid" => session_uuid}, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      case NotebookClient.get_notebook_by_id(notebook_id) do
        {:error, :notebook_not_found} ->
          {:ok,
           socket
           |> put_flash(:error, "Notebook not found")
           |> redirect(to: "/en/ai/notebooks")}

        notebook when is_tuple(notebook) ->
          case Notebook.erl_changeset(notebook) do
            %Ecto.Changeset{valid?: true} = changeset ->
              notebook_struct = Ecto.Changeset.apply_changes(changeset)
              user_id = to_string(user.id)

              can_view =
                Notebook.is_public?(notebook_struct) or
                  Notebook.is_owner?(notebook_struct, user_id) or
                  Notebook.is_collaborator?(notebook_struct, user_id)

              if can_view do
                if connected?(socket) do
                  language_atom =
                    case notebook_struct.language do
                      lang when is_binary(lang) -> String.to_atom(lang)
                      lang when is_atom(lang) -> lang
                      _ -> :python
                    end

                  {:ok, session_id, kernel_id} =
                    :notebook_client.create_session(notebook_id, user_id, language_atom)

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

                  {:ok, socket}
                else
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

                  {:ok, socket}
                end
              else
                {:ok,
                 socket
                 |> put_flash(:error, "You don't have permission to view this notebook")
                 |> redirect(to: "/en/ai/notebooks")}
              end

            _ ->
              {:ok,
               socket
               |> put_flash(:error, "Failed to load notebook")
               |> redirect(to: "/en/ai/notebooks")}
          end
      end
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"id" => notebook_id}, %{"user_id" => user_id}, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        mount(%{"id" => notebook_id}, %{"session_uuid" => "legacy"}, assign(socket, user: user))

      {:error, _reason} ->
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
    cell_id = generate_cell_id()
    position_int = String.to_integer(position)

    new_cell = %{
      id: cell_id,
      type: String.to_atom(type),
      content: "",
      outputs: [],
      execution_count: nil,
      metadata: %{}
    }

    cells = List.insert_at(socket.assigns.cells, position_int, new_cell)

    {:noreply,
     socket
     |> assign(cells: cells)
     |> assign(show_add_cell_menu: false)
     |> assign(selected_cell: cell_id)}
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

    {:noreply, assign(socket, cells: cells)}
  end

  @impl true
  def handle_event("execute_cell", %{"cell_id" => cell_id}, socket) do
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

  @impl true
  def handle_event("delete_cell", %{"cell_id" => cell_id}, socket) do
    cells = Enum.reject(socket.assigns.cells, fn cell -> cell.id == cell_id end)
    {:noreply, assign(socket, cells: cells)}
  end

  @impl true
  def handle_event("move_cell_up", %{"cell_id" => cell_id}, socket) do
    cells = move_cell(socket.assigns.cells, cell_id, :up)
    {:noreply, assign(socket, cells: cells)}
  end

  @impl true
  def handle_event("move_cell_down", %{"cell_id" => cell_id}, socket) do
    cells = move_cell(socket.assigns.cells, cell_id, :down)
    {:noreply, assign(socket, cells: cells)}
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
  def handle_event("run_all_cells", _params, socket) do
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

  @impl true
  def handle_event("save_notebook", _params, socket) do
    send(self(), :save_notebook_async)
    {:noreply, put_flash(socket, :info, "Saving notebook...")}
  end

  @impl true
  def handle_info({:execute_cell_async, cell_id, code}, socket) do
    case :notebook_client.execute_code(socket.assigns.session_id, code, cell_id) do
      {:ok, result} ->
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

        {:noreply,
         socket
         |> assign(cells: cells)
         |> assign(executing_cells: executing_cells)
         |> assign(kernel_status: kernel_status)}

      {:error, reason} ->
        Logger.error("Failed to execute cell: #{inspect(reason)}")
        executing_cells = MapSet.delete(socket.assigns.executing_cells, cell_id)

        {:noreply,
         socket
         |> assign(executing_cells: executing_cells)
         |> put_flash(:error, "Execution failed")}
    end
  end

  @impl true
  def handle_info(:execute_all_cells, socket) do
    code_cells =
      socket.assigns.cells
      |> Enum.filter(fn cell -> cell.type == :code end)

    Enum.each(code_cells, fn cell ->
      send(self(), {:execute_cell_async, cell.id, cell.content})
    end)

    {:noreply, socket}
  end

  @impl true
  def handle_info(:save_notebook_async, socket) do
    cells_json = Jason.encode!(socket.assigns.cells)

    case NotebookClient.update_notebook_content(
           socket.assigns.notebook_id,
           to_string(socket.assigns.user.id),
           cells_json
         ) do
      :ok ->
        {:noreply, put_flash(socket, :info, "Notebook saved successfully")}

      {:error, reason} ->
        Logger.error("Failed to save notebook: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to save notebook")}
    end
  end

  @impl true
  def terminate(_reason, socket) do
    if socket.assigns[:session_id] do
      :notebook_client.close_session(socket.assigns.session_id)
    end

    :ok
  end

  defp load_cells(notebook) do
    case notebook.content_cid do
      nil ->
        [
          %{
            id: generate_cell_id(),
            type: :code,
            content: "",
            outputs: [],
            execution_count: nil,
            metadata: %{}
          }
        ]

      _cid ->
        case NotebookClient.get_notebook_content(notebook.id) do
          {:error, _} ->
            [
              %{
                id: generate_cell_id(),
                type: :code,
                content: "",
                outputs: [],
                execution_count: nil,
                metadata: %{}
              }
            ]

          content when is_binary(content) or is_list(content) ->
            content_str = if is_list(content), do: to_string(content), else: content

            case Jason.decode(content_str) do
              {:ok, cells} when is_list(cells) ->
                Enum.map(cells, fn cell ->
                  %{
                    id: cell["id"] || generate_cell_id(),
                    type: String.to_atom(cell["type"] || "code"),
                    content: cell["content"] || "",
                    outputs: cell["outputs"] || [],
                    execution_count: cell["execution_count"],
                    metadata: cell["metadata"] || %{}
                  }
                end)

              _ ->
                [
                  %{
                    id: generate_cell_id(),
                    type: :code,
                    content: "",
                    outputs: [],
                    execution_count: nil,
                    metadata: %{}
                  }
                ]
            end
        end
    end
  end

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
end
