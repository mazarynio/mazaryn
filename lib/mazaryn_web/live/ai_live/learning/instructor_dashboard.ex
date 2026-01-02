defmodule MazarynWeb.AiLive.Learning.InstructorDashboard do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)

      if is_verified_instructor?(user_id) do
        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(session_uuid: session_uuid)
          |> assign(active_tab: :overview)
          |> assign(my_paths: load_instructor_paths(user_id))
          |> assign(stats: load_instructor_stats(user_id))
          |> assign(recent_enrollments: [])
          |> assign(pending_content: [])

        {:ok, socket}
      else
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "You must be a verified instructor to access this page")
         |> redirect(to: "/#{locale}/ai/learning/become-instructor")}
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
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        if is_verified_instructor?(uid) do
          socket =
            socket
            |> assign(user: user)
            |> assign(user_id: uid)
            |> assign(active_tab: :overview)
            |> assign(my_paths: load_instructor_paths(uid))
            |> assign(stats: load_instructor_stats(uid))
            |> assign(recent_enrollments: [])
            |> assign(pending_content: [])

          {:ok, socket}
        else
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "You must be a verified instructor to access this page")
           |> redirect(to: "/#{locale}/ai/learning/become-instructor")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    tab_atom = String.to_existing_atom(tab)
    {:noreply, assign(socket, active_tab: tab_atom)}
  end

  @impl true
  def handle_event("delete_path", %{"id" => path_id}, socket) do
    charlist_id = String.to_charlist(path_id)

    case :learningdb.delete_learning_path(charlist_id, socket.assigns.user_id) do
      :ok ->
        {:noreply,
         socket
         |> assign(my_paths: load_instructor_paths(socket.assigns.user_id))
         |> put_flash(:info, "Learning path deleted successfully")}

      {:error, _reason} ->
        {:noreply, put_flash(socket, :error, "Failed to delete learning path")}
    end
  end

  defp is_verified_instructor?(user_id) do
    case :instructordb.is_verified_instructor(user_id) do
      true -> true
      _ -> false
    end
  rescue
    _ -> false
  end

  defp to_string_safe(value) when is_list(value), do: List.to_string(value)
  defp to_string_safe(value) when is_binary(value), do: value
  defp to_string_safe(nil), do: nil
  defp to_string_safe(:undefined), do: nil
  defp to_string_safe(value) when is_atom(value), do: Atom.to_string(value)
  defp to_string_safe(value), do: to_string(value)

  defp load_instructor_paths(user_id) do
    try do
      Logger.info("🔍 [load_instructor_paths] Starting for user_id: #{inspect(user_id)}")

      case :learningdb.get_creator_content(user_id, :learning_path) do
        paths when is_list(paths) ->
          Logger.info("✅ [load_instructor_paths] Got #{length(paths)} paths from DB")

          converted_paths =
            paths
            |> Enum.with_index()
            |> Enum.map(fn {path, idx} ->
              result = convert_path_tuple(path)

              if result do
                Logger.info("✅ Path ##{idx + 1}: #{result.title} (ID: #{result.id})")
              end

              result
            end)
            |> Enum.reject(&is_nil/1)

          Logger.info("✅ Successfully converted #{length(converted_paths)} paths")
          converted_paths

        other ->
          Logger.error("❌ Unexpected result: #{inspect(other)}")
          []
      end
    rescue
      error ->
        Logger.error("❌ Exception: #{inspect(error)}")
        []
    end
  end

  defp convert_path_tuple(path_tuple) when is_tuple(path_tuple) do
    try do
      raw_id = elem(path_tuple, 1)
      raw_title = elem(path_tuple, 2)
      raw_description = elem(path_tuple, 3)
      raw_creator_id = elem(path_tuple, 4)
      raw_creator_name = elem(path_tuple, 5)
      raw_creator_family = elem(path_tuple, 6)

      id = to_string_safe(raw_id)
      title = to_string_safe(raw_title)
      description_cid = to_string_safe(raw_description)
      creator_id = to_string_safe(raw_creator_id)
      creator_name = to_string_safe(raw_creator_name)
      creator_family = to_string_safe(raw_creator_family)

      # Fetch description from IPFS if it's a CID
      description = fetch_description_from_ipfs(description_cid)

      %{
        id: id,
        title: if(is_nil(title) or title == "", do: "Untitled Course", else: title),
        description: description,
        creator_id: creator_id,
        creator_name: creator_name || "Unknown",
        creator_family: creator_family || "",
        approval_status: elem(path_tuple, 7) || :pending,
        difficulty_level: elem(path_tuple, 13) || :beginner,
        estimated_duration: elem(path_tuple, 14) || 0,
        track: to_string_safe(elem(path_tuple, 22)),
        category: to_string_safe(elem(path_tuple, 23)) || "uncategorized",
        subcategory: to_string_safe(elem(path_tuple, 24)) || "",
        enrollment_count: elem(path_tuple, 28) || 0,
        completion_count: elem(path_tuple, 74) || 0,
        average_rating: (elem(path_tuple, 30) || 0.0) * 1.0,
        price: (elem(path_tuple, 37) || 0.0) * 1.0,
        currency: to_string_safe(elem(path_tuple, 38)) || "USD",
        language: to_string_safe(elem(path_tuple, 59)) || "en",
        self_paced: elem(path_tuple, 56) || true,
        lifetime_access: elem(path_tuple, 42) || true,
        tags: elem(path_tuple, 17) || [],
        learning_outcomes: elem(path_tuple, 32) || [],
        prerequisites: elem(path_tuple, 25) || []
      }
    rescue
      error ->
        Logger.error("❌ Conversion error: #{inspect(error)}")
        nil
    end
  end

  defp convert_path_tuple(_), do: nil

  defp fetch_description_from_ipfs(description) when is_binary(description) do
    cond do
      String.starts_with?(description, "Qm") or String.starts_with?(description, "baf") ->
        try do
          cid_charlist = String.to_charlist(description)
          content = :ipfs_content.get_text_content(cid_charlist)
          if is_binary(content), do: content, else: to_string(content)
        rescue
          _ -> description
        end

      true ->
        description
    end
  end

  defp fetch_description_from_ipfs(description), do: description || ""

  defp load_instructor_stats(user_id) do
    try do
      paths = load_instructor_paths(user_id)

      total_enrollments =
        Enum.reduce(paths, 0, fn path, acc ->
          acc + (path.enrollment_count || 0)
        end)

      total_completions =
        Enum.reduce(paths, 0, fn path, acc ->
          acc + (path.completion_count || 0)
        end)

      ratings =
        paths
        |> Enum.map(fn path -> path.average_rating end)
        |> Enum.reject(fn rating ->
          is_nil(rating) || rating == 0.0 || rating == 0
        end)

      avg_rating =
        if length(ratings) > 0 do
          Float.round(Enum.sum(ratings) / length(ratings), 1)
        else
          0.0
        end

      %{
        total_paths: length(paths),
        total_enrollments: total_enrollments,
        total_completions: total_completions,
        average_rating: avg_rating,
        published_paths:
          Enum.count(
            paths,
            &(&1.approval_status == :approved || &1.approval_status == "approved")
          ),
        pending_paths:
          Enum.count(paths, &(&1.approval_status == :pending || &1.approval_status == "pending"))
      }
    rescue
      error ->
        Logger.error("Error loading instructor stats: #{inspect(error)}")

        %{
          total_paths: 0,
          total_enrollments: 0,
          total_completions: 0,
          average_rating: 0.0,
          published_paths: 0,
          pending_paths: 0
        }
    end
  end
end
