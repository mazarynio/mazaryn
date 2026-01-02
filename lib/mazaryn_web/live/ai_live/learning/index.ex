defmodule MazarynWeb.AiLive.Learning.Index do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🔵 Learning Index mount - session_uuid: #{session_uuid}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      Logger.info("🔵 User loaded - user_id: #{user_id}, username: #{user.username}")

      is_admin = is_admin?(user)
      is_instructor = check_instructor_status(user_id)

      Logger.info("👤 User status - is_admin: #{is_admin}, is_instructor: #{is_instructor}")

      socket =
        socket
        |> assign(user: user)
        |> assign(session_uuid: session_uuid)
        |> assign(is_admin: is_admin)
        |> assign(is_instructor: is_instructor)
        |> assign(categories: get_categories())
        |> assign(search_query: "")
        |> assign(featured_paths: [])
        |> assign(trending_paths: [])
        |> assign(my_enrollments: [])
        |> assign(selected_category: nil)
        |> assign(selected_difficulty: nil)

      {:ok, socket}
    else
      {:error, reason} ->
        Logger.error("🔴 Failed to get user: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    Logger.info("🔵 Learning Index mount - user_id: #{user_id}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)
        Logger.info("🔵 User loaded - uid: #{uid}, username: #{user.username}")

        is_admin = is_admin?(user)
        is_instructor = check_instructor_status(uid)

        Logger.info("👤 User status - is_admin: #{is_admin}, is_instructor: #{is_instructor}")

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(is_admin: is_admin)
          |> assign(is_instructor: is_instructor)
          |> assign(categories: get_categories())
          |> assign(search_query: "")
          |> assign(featured_paths: [])
          |> assign(trending_paths: [])
          |> assign(my_enrollments: [])
          |> assign(selected_category: nil)
          |> assign(selected_difficulty: nil)

        {:ok, socket}

      {:error, reason} ->
        Logger.error("🔴 Failed to get user: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    Logger.info("🔍 Search event - query: #{query}")
    {:noreply, assign(socket, search_query: query)}
  end

  @impl true
  def handle_event("select_category", %{"category" => category}, socket) do
    Logger.info("📁 Select category - category: #{category}")
    {:noreply, assign(socket, selected_category: category)}
  end

  @impl true
  def handle_event("select_difficulty", %{"difficulty" => difficulty}, socket) do
    Logger.info("📊 Select difficulty - difficulty: #{difficulty}")
    {:noreply, assign(socket, selected_difficulty: difficulty)}
  end

  @impl true
  def handle_event("clear_filters", _params, socket) do
    Logger.info("🧹 Clear filters")
    {:noreply, assign(socket, selected_category: nil, selected_difficulty: nil, search_query: "")}
  end

  defp is_admin?(user) do
    user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
    admin_users = ["zaryn", "mazaryn", "arvand"]
    is_admin = user.username in admin_users || user_id in admin_users

    Logger.info(
      "🔍 Checking admin status - username: #{user.username}, user_id: #{user_id}, is_admin: #{is_admin}"
    )

    is_admin
  end

  defp check_instructor_status(user_id) when is_binary(user_id) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("🔍 CHECKING INSTRUCTOR STATUS")
    Logger.info("=" |> String.duplicate(80))
    Logger.info("   User ID: #{user_id}")
    Logger.info("   User ID type: binary")

    try do
      Logger.info("🔵 Calling :instructordb.is_verified_instructor(#{inspect(user_id)})")

      result = :instructordb.is_verified_instructor(user_id)

      Logger.info("📊 Result: #{inspect(result)}")
      Logger.info("   Result type: #{inspect(is_boolean(result))}")

      case result do
        true ->
          Logger.info("✅✅✅ USER IS VERIFIED INSTRUCTOR")
          Logger.info("=" |> String.duplicate(80))
          true

        false ->
          Logger.info("❌ User is NOT verified instructor")
          Logger.info("=" |> String.duplicate(80))
          false

        other ->
          Logger.warning("⚠️ Unexpected result: #{inspect(other)}")
          Logger.info("=" |> String.duplicate(80))
          false
      end
    rescue
      error ->
        Logger.error("🔴 Exception checking instructor status:")
        Logger.error("   Error: #{inspect(error)}")
        Logger.error("   Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
        Logger.info("=" |> String.duplicate(80))
        false
    end
  end

  defp check_instructor_status(user_id) do
    Logger.warning("⚠️ check_instructor_status called with non-binary user_id: #{inspect(user_id)}")
    Logger.warning("   Type: #{inspect(user_id.__struct__)}")

    user_id_str = to_string(user_id)
    Logger.info("   Converting to string: #{user_id_str}")

    check_instructor_status(user_id_str)
  end

  defp get_categories do
    [
      %{id: "programming", name: "Programming", icon: "💻"},
      %{id: "data_science", name: "Data Science", icon: "📊"},
      %{id: "web_dev", name: "Web Development", icon: "🌐"},
      %{id: "mobile", name: "Mobile Dev", icon: "📱"},
      %{id: "cloud", name: "Cloud & DevOps", icon: "☁️"},
      %{id: "security", name: "Security", icon: "🔒"},
      %{id: "blockchain", name: "Blockchain", icon: "⛓️"},
      %{id: "design", name: "Design", icon: "🎨"},
      %{id: "business", name: "Business", icon: "💼"}
    ]
  end
end
