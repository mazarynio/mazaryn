defmodule MazarynWeb.GroupLive.Invites do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => group_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      group = load_group(group_id)

      if group do
        user_id_str = extract_user_id(user)
        is_admin = is_admin?(group, user_id_str)

        if is_admin do
          invites = load_pending_invites(group_id)

          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(group: group)
            |> assign(is_admin: is_admin)
            |> assign(invites: invites)

          {:ok, socket}
        else
          {:ok,
           socket
           |> put_flash(:error, "Only admins can view invites")
           |> redirect(to: "/#{socket.assigns.locale}/groups/#{group_id}")}
        end
      else
        {:ok,
         socket
         |> put_flash(:error, "Group not found")
         |> redirect(to: "/#{socket.assigns.locale}/groups")}
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
  def mount(%{"id" => group_id}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        group = load_group(group_id)

        if group do
          user_id_str = extract_user_id(user)
          is_admin = is_admin?(group, user_id_str)

          if is_admin do
            invites = load_pending_invites(group_id)

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(group: group)
              |> assign(is_admin: is_admin)
              |> assign(invites: invites)

            {:ok, socket}
          else
            {:ok,
             socket
             |> put_flash(:error, "Only admins can view invites")
             |> redirect(to: "/#{socket.assigns.locale}/groups/#{group_id}")}
          end
        else
          {:ok,
           socket
           |> put_flash(:error, "Group not found")
           |> redirect(to: "/#{socket.assigns.locale}/groups")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("cancel_invite", %{"invite_id" => invite_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)

    try do
      :groupdb.cancel_invite(to_charlist(invite_id), to_charlist(user_id_str))

      invites = load_pending_invites(socket.assigns.group.id)

      {:noreply,
       socket
       |> assign(invites: invites)
       |> put_flash(:info, "Invitation cancelled")}
    rescue
      error ->
        Logger.error("Failed to cancel invite: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to cancel invitation")}
    end
  end

  defp extract_user_id(user) do
    if is_binary(user.id), do: user.id, else: to_string(user.id)
  end

  defp load_group(group_id) do
    try do
      group = :groupdb.get_group(to_charlist(group_id))
      convert_group_record(group)
    rescue
      _ -> nil
    end
  end

  defp load_pending_invites(group_id) do
    []
  end

  defp is_admin?(group, user_id) do
    member_result =
      Enum.any?(group.admins, fn admin_id ->
        to_string(admin_id) == to_string(user_id)
      end)

    owner_result = to_string(group.owner_id) == to_string(user_id)
    member_result || owner_result
  end

  defp convert_group_record(group) do
    admins_raw = elem(group, 8)
    members_raw = elem(group, 9)

    admins =
      case admins_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    members =
      case members_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    %{
      id: to_string(elem(group, 1)),
      unique_name: to_string(elem(group, 2)),
      name: to_string(elem(group, 3)),
      description: to_string(elem(group, 4)),
      type: elem(group, 5),
      privacy: elem(group, 6),
      owner_id: to_string(elem(group, 7)),
      admins: admins,
      members: members,
      settings: elem(group, 10),
      member_count: elem(group, 14),
      date_created: elem(group, 15)
    }
  end

  def get_user_info(user_id) do
    try do
      case Core.UserClient.get_user_by_id(to_charlist(user_id)) do
        {:ok, user} ->
          %{
            id: to_string(elem(user, 1)),
            username: to_string(elem(user, 2)),
            avatar_url: if(elem(user, 20) == :undefined, do: nil, else: to_string(elem(user, 20)))
          }

        _ ->
          %{id: user_id, username: "Unknown", avatar_url: nil}
      end
    rescue
      _ -> %{id: user_id, username: "Unknown", avatar_url: nil}
    end
  end

  def format_datetime(datetime) do
    case datetime do
      {{year, month, day}, {hour, minute, _second}} ->
        "#{day}/#{month}/#{year} #{String.pad_leading(to_string(hour), 2, "0")}:#{String.pad_leading(to_string(minute), 2, "0")}"

      _ ->
        "Unknown"
    end
  end
end
