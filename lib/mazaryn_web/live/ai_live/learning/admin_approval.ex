defmodule MazarynWeb.AiLive.Learning.AdminApproval do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🔵 AdminApproval mount - session_uuid: #{session_uuid}")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)
      Logger.info("🔵 User loaded - user_id: #{user_id}, username: #{user.username}")

      if is_admin?(user_id, user.username) do
        Logger.info("✅ User is admin - loading admin panel")

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: user_id)
          |> assign(session_uuid: session_uuid)
          |> assign(active_tab: :instructor_requests)
          |> assign(instructor_requests: load_instructor_requests())
          |> assign(pending_paths: load_pending_paths())
          |> assign(processing: false)

        {:ok, socket}
      else
        Logger.warning("❌ User #{user_id} is NOT an admin")
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "You must be an administrator to access this page")
         |> redirect(to: "/#{locale}/ai/learning")}
      end
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
    Logger.info("🔵 AdminApproval mount - user_id: #{user_id}")

    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)
        Logger.info("🔵 User loaded - uid: #{uid}, username: #{user.username}")

        if is_admin?(uid, user.username) do
          Logger.info("✅ User is admin - loading admin panel")

          socket =
            socket
            |> assign(user: user)
            |> assign(user_id: uid)
            |> assign(active_tab: :instructor_requests)
            |> assign(instructor_requests: load_instructor_requests())
            |> assign(pending_paths: load_pending_paths())
            |> assign(processing: false)

          {:ok, socket}
        else
          Logger.warning("❌ User #{uid} is NOT an admin")
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "You must be an administrator to access this page")
           |> redirect(to: "/#{locale}/ai/learning")}
        end

      {:error, reason} ->
        Logger.error("🔴 Failed to get user: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("switch_tab", %{"tab" => tab}, socket) do
    Logger.info("🔄 Switching tab to: #{tab}")
    tab_atom = String.to_existing_atom(tab)
    {:noreply, assign(socket, active_tab: tab_atom)}
  end

  @impl true
  def handle_event("approve_instructor", %{"user_id" => user_id}, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("✅ APPROVE INSTRUCTOR EVENT TRIGGERED")
    Logger.info("=" |> String.duplicate(80))
    Logger.info("📋 Request Details:")
    Logger.info("   User ID: #{user_id}")
    Logger.info("   Admin Username: #{socket.assigns.user.username}")
    Logger.info("")

    admin_username = socket.assigns.user.username

    try do
      Logger.info("🔵 Calling :instructordb.approve_request/2")
      Logger.info("   Params: (#{inspect(admin_username)}, #{inspect(user_id)})")

      result = :instructordb.approve_request(admin_username, user_id)

      Logger.info("")
      Logger.info("📊 Result from instructordb: #{inspect(result)}")
      Logger.info("")

      case result do
        :ok ->
          Logger.info("✅✅✅ SUCCESS - Instructor approved")
          Logger.info("🔄 Reloading instructor requests...")

          new_requests = load_instructor_requests()
          Logger.info("📊 Remaining requests: #{length(new_requests)}")

          {:noreply,
           socket
           |> assign(instructor_requests: new_requests)
           |> assign(processing: false)
           |> put_flash(:info, "Instructor approved successfully! 🎉")}

        {:error, reason} ->
          Logger.error("❌❌❌ FAILED to approve instructor")
          Logger.error("   Reason: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(processing: false)
           |> put_flash(:error, "Failed to approve instructor: #{inspect(reason)}")}

        other ->
          Logger.warning("⚠️ Unexpected result: #{inspect(other)}")
          Logger.info("🔄 Reloading requests anyway...")

          {:noreply,
           socket
           |> assign(instructor_requests: load_instructor_requests())
           |> assign(processing: false)
           |> put_flash(:info, "Instructor status updated")}
      end
    rescue
      error ->
        Logger.error("=" |> String.duplicate(80))
        Logger.error("🔴🔴🔴 EXCEPTION CAUGHT")
        Logger.error("=" |> String.duplicate(80))
        Logger.error("Exception: #{inspect(error)}")
        Logger.error("Stacktrace:")
        Logger.error(Exception.format_stacktrace(__STACKTRACE__))
        Logger.error("=" |> String.duplicate(80))

        {:noreply,
         socket
         |> assign(processing: false)
         |> put_flash(:error, "Error: #{inspect(error)}")}
    end
  end

  @impl true
  def handle_event("reject_instructor", %{"user_id" => user_id, "reason" => reason}, socket) do
    Logger.info("=" |> String.duplicate(80))
    Logger.info("❌ REJECT INSTRUCTOR EVENT TRIGGERED")
    Logger.info("=" |> String.duplicate(80))
    Logger.info("📋 Request Details:")
    Logger.info("   User ID: #{user_id}")
    Logger.info("   Admin Username: #{socket.assigns.user.username}")
    Logger.info("   Reason: #{reason}")
    Logger.info("")

    admin_username = socket.assigns.user.username

    try do
      Logger.info("🔵 Calling :instructordb.reject_request/3")

      result = :instructordb.reject_request(admin_username, user_id, reason)

      Logger.info("📊 Result: #{inspect(result)}")

      case result do
        :ok ->
          Logger.info("✅ Instructor rejected successfully")

          {:noreply,
           socket
           |> assign(instructor_requests: load_instructor_requests())
           |> assign(processing: false)
           |> put_flash(:info, "Instructor request rejected")}

        {:error, reason} ->
          Logger.error("❌ Failed to reject: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(processing: false)
           |> put_flash(:error, "Failed to reject: #{inspect(reason)}")}
      end
    rescue
      error ->
        Logger.error("🔴 Exception rejecting instructor: #{inspect(error)}")

        {:noreply,
         socket
         |> assign(processing: false)
         |> put_flash(:error, "Error: #{inspect(error)}")}
    end
  end

  @impl true
  def handle_event("approve_path", %{"path_id" => path_id}, socket) do
    Logger.info("✅ APPROVE PATH EVENT - path_id: #{path_id}")
    admin_username = socket.assigns.user.username

    try do
      result = :learningdb.approve_path(path_id, admin_username)
      Logger.info("Result: #{inspect(result)}")

      case result do
        :ok ->
          {:noreply,
           socket
           |> assign(pending_paths: load_pending_paths())
           |> assign(processing: false)
           |> put_flash(:info, "Learning path approved successfully! 🎉")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(processing: false)
           |> put_flash(:error, "Failed: #{inspect(reason)}")}

        other ->
          {:noreply,
           socket
           |> assign(pending_paths: load_pending_paths())
           |> assign(processing: false)
           |> put_flash(:info, "Path status updated")}
      end
    rescue
      error ->
        Logger.error("🔴 Exception approving path: #{inspect(error)}")

        {:noreply,
         socket
         |> assign(processing: false)
         |> put_flash(:error, "Error: #{inspect(error)}")}
    end
  end

  @impl true
  def handle_event("reject_path", %{"path_id" => path_id, "reason" => reason}, socket) do
    Logger.info("❌ REJECT PATH EVENT - path_id: #{path_id}, reason: #{reason}")
    admin_username = socket.assigns.user.username

    try do
      result = :learningdb.reject_path(path_id, admin_username, reason)

      case result do
        :ok ->
          {:noreply,
           socket
           |> assign(pending_paths: load_pending_paths())
           |> assign(processing: false)
           |> put_flash(:info, "Learning path rejected")}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(processing: false)
           |> put_flash(:error, "Failed: #{inspect(reason)}")}
      end
    rescue
      error ->
        Logger.error("🔴 Exception: #{inspect(error)}")

        {:noreply,
         socket
         |> assign(processing: false)
         |> put_flash(:error, "Error: #{inspect(error)}")}
    end
  end

  @impl true
  def handle_event(event_name, params, socket) do
    Logger.warning("⚠️ Unhandled event: #{event_name}")
    Logger.debug("Params: #{inspect(params)}")
    {:noreply, socket}
  end

  defp is_admin?(user_id, username) do
    admin_users = ["zaryn", "mazaryn", "arvand"]
    is_admin = username in admin_users || user_id in admin_users
    Logger.info("🔍 Admin check - username: #{username}, is_admin: #{is_admin}")
    is_admin
  end

  defp load_instructor_requests do
    Logger.info("=" |> String.duplicate(60))
    Logger.info("🔍 LOADING INSTRUCTOR REQUESTS")
    Logger.info("=" |> String.duplicate(60))

    try do
      Logger.info("🔵 Calling :instructordb.get_pending_requests()")

      requests = :instructordb.get_pending_requests()

      Logger.info("📊 Raw result type: #{inspect(is_list(requests))}")

      case requests do
        requests when is_list(requests) ->
          Logger.info("✅ Received #{length(requests)} requests")

          pending_requests =
            Enum.filter(requests, fn request ->
              status = Map.get(request, :status)
              status == :pending || status == "pending"
            end)

          Logger.info("📊 Filtered to #{length(pending_requests)} pending requests")

          Enum.each(pending_requests, fn request ->
            Logger.info("   📝 Request:")
            Logger.info("      User ID: #{inspect(Map.get(request, :user_id))}")

            Logger.info(
              "      Name: #{inspect(Map.get(request, :name))} #{inspect(Map.get(request, :family))}"
            )

            Logger.info("      Status: #{inspect(Map.get(request, :status))}")
          end)

          mapped_requests =
            Enum.map(pending_requests, fn request ->
              %{
                user_id: Map.get(request, :user_id),
                name: Map.get(request, :name),
                family: Map.get(request, :family),
                reason: Map.get(request, :reason),
                requested_at: Map.get(request, :requested_at),
                status: Map.get(request, :status)
              }
            end)

          Logger.info("✅ Mapped #{length(mapped_requests)} requests successfully")
          Logger.info("=" |> String.duplicate(60))
          mapped_requests

        other ->
          Logger.warning("⚠️ Unexpected return type: #{inspect(other)}")
          Logger.info("=" |> String.duplicate(60))
          []
      end
    rescue
      error ->
        Logger.error("🔴 Exception loading requests: #{inspect(error)}")
        Logger.error("Stacktrace: #{Exception.format_stacktrace(__STACKTRACE__)}")
        Logger.info("=" |> String.duplicate(60))
        []
    end
  end

  defp load_pending_paths do
    Logger.info("🔍 Loading pending learning paths...")

    try do
      all_paths = :learningdb.get_all_learning_paths()
      Logger.info("Got #{inspect(length(all_paths))} total paths")

      pending =
        Enum.filter(all_paths, fn path ->
          status = Map.get(path, :approval_status)
          status == :pending || status == "pending"
        end)

      Logger.info("Filtered to #{length(pending)} pending paths")

      mapped_paths =
        Enum.map(pending, fn path ->
          case Mazaryn.Schema.Learning.erl_changeset(path) do
            changeset when is_map(changeset) ->
              Ecto.Changeset.apply_changes(changeset)

            _ ->
              nil
          end
        end)
        |> Enum.reject(&is_nil/1)

      Logger.info("✅ Mapped #{length(mapped_paths)} paths")
      mapped_paths
    rescue
      error ->
        Logger.error("🔴 Exception loading pending paths: #{inspect(error)}")
        []
    end
  end

  defp format_datetime({{year, month, day}, {hour, minute, _second}}) do
    "#{year}-#{pad(month)}-#{pad(day)} #{pad(hour)}:#{pad(minute)}"
  end

  defp format_datetime(_), do: "N/A"

  defp format_duration(seconds) when is_integer(seconds) do
    hours = div(seconds, 3600)
    "#{hours} hours"
  end

  defp format_duration(_), do: "N/A"

  defp pad(num) when num < 10, do: "0#{num}"
  defp pad(num), do: "#{num}"
end
