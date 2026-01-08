defmodule MazarynWeb.GroupLive.Invites do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => group_id}, %{"session_uuid" => session_uuid}, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      mount_with_user(socket, user, group_id)
    else
      _ -> {:ok, socket |> put_flash(:error, "Session expired") |> redirect(to: "/en/login")}
    end
  end

  def mount(%{"id" => group_id}, %{"user_id" => user_id}, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} -> mount_with_user(socket, user, group_id)
      _ -> {:ok, socket |> put_flash(:error, "User not found") |> redirect(to: "/en/login")}
    end
  end

  defp mount_with_user(socket, user, group_id) do
    case load_group(group_id) do
      nil ->
        {:ok,
         socket
         |> put_flash(:error, "Group not found")
         |> redirect(to: "/#{socket.assigns.locale}/groups")}

      group ->
        user_id_str = to_string(user.id)
        is_admin = is_owner?(group, user_id_str) || Enum.member?(group.admins, user_id_str)

        if is_admin do
          {:ok,
           socket
           |> assign(:user, user)
           |> assign(:group, group)
           |> assign(:is_admin, is_admin)
           |> assign(:invites, load_pending_invites(group_id))
           |> assign(:show_invite_modal, false)
           |> assign(:search_query, "")
           |> assign(:found_users, [])}
        else
          {:ok,
           socket
           |> put_flash(:error, "Only admins can view invites")
           |> redirect(to: "/#{socket.assigns.locale}/groups/#{group_id}")}
        end
    end
  end

  @impl true
  def handle_event("stop_propagation", _, socket), do: {:noreply, socket}

  @impl true
  def handle_event("open_invite_modal", _, socket) do
    {:noreply, assign(socket, show_invite_modal: true, search_query: "", found_users: [])}
  end

  @impl true
  def handle_event("close_invite_modal", _, socket) do
    {:noreply, assign(socket, show_invite_modal: false, search_query: "", found_users: [])}
  end

  @impl true
  def handle_event("search_users", params, socket) do
    query = params["value"] || params["query"] || ""
    trimmed = String.trim(query)

    if trimmed == "" do
      {:noreply, assign(socket, search_query: query, found_users: [])}
    else
      found_users = search_and_filter(trimmed, socket.assigns.group)
      {:noreply, assign(socket, search_query: query, found_users: found_users)}
    end
  end

  @impl true
  def handle_event("send_invite", %{"user_id" => invitee_id}, socket) do
    try do
      Core.GroupClient.send_invite(
        to_charlist(socket.assigns.group.id),
        to_charlist(to_string(socket.assigns.user.id)),
        to_charlist(invitee_id),
        ~c"Join our group!"
      )

      {:noreply,
       socket
       |> assign(:invites, load_pending_invites(socket.assigns.group.id))
       |> assign(:show_invite_modal, false)
       |> assign(:search_query, "")
       |> assign(:found_users, [])
       |> put_flash(:info, "Invitation sent successfully!")}
    rescue
      error ->
        error_msg =
          case error do
            %RuntimeError{message: msg} ->
              cond do
                String.contains?(msg, "already_member") -> "User is already a member"
                String.contains?(msg, "user_banned") -> "User is banned from this group"
                String.contains?(msg, "invitee_not_found") -> "User not found"
                true -> "Failed to send invitation"
              end

            _ ->
              "Failed to send invitation"
          end

        {:noreply, put_flash(socket, :error, error_msg)}
    end
  end

  @impl true
  def handle_event("cancel_invite", %{"invite_id" => id}, socket) do
    try do
      Core.GroupClient.cancel_invite(
        to_charlist(id),
        to_charlist(to_string(socket.assigns.user.id))
      )

      {:noreply,
       socket
       |> assign(:invites, load_pending_invites(socket.assigns.group.id))
       |> put_flash(:info, "Invitation cancelled")}
    rescue
      _ -> {:noreply, put_flash(socket, :error, "Failed to cancel invitation")}
    end
  end

  defp search_and_filter(query, group) do
    try do
      Core.GroupClient.search_users_by_username(to_charlist(query))
      |> Enum.map(fn rec ->
        try do
          id = rec |> elem(1) |> to_string()
          username = rec |> elem(8) |> to_string()

          avatar =
            try do
              case elem(rec, 29) do
                :undefined -> "/images/default-avatar.png"
                nil -> "/images/default-avatar.png"
                url when is_binary(url) -> url
                url when is_list(url) -> to_string(url)
                _ -> "/images/default-avatar.png"
              end
            rescue
              _ -> "/images/default-avatar.png"
            end

          %{id: id, username: username, avatar_url: avatar}
        rescue
          _ -> nil
        end
      end)
      |> Enum.reject(&is_nil/1)
      |> Enum.reject(fn u ->
        Enum.member?(group.members, u.id) || Enum.member?(group.banned_users, u.id)
      end)
      |> Enum.take(10)
    rescue
      error ->
        Logger.error("Search error: #{inspect(error)}")
        []
    end
  end

  defp load_group(id) do
    try do
      Core.GroupClient.get_group(to_charlist(id))
      |> convert_group()
    rescue
      _ -> nil
    end
  end

  defp convert_group(g) do
    %{
      id: g |> elem(1) |> to_string(),
      unique_name: g |> elem(2) |> to_string(),
      name: g |> elem(3) |> to_string(),
      description: g |> elem(4) |> to_string(),
      type: elem(g, 5),
      privacy: elem(g, 6),
      owner_id: g |> elem(7) |> to_string(),
      admins: safe_list(elem(g, 10)),
      members: safe_list(elem(g, 11)),
      banned_users: safe_list(elem(g, 13)),
      settings: elem(g, 15),
      member_count: elem(g, 19),
      date_created: elem(g, 20)
    }
  end

  defp load_pending_invites(id) do
    try do
      Core.GroupClient.get_pending_invites(to_charlist(id))
      |> Enum.map(&convert_invite/1)
    rescue
      _ -> []
    end
  end

  defp convert_invite(i) do
    %{
      id: i |> elem(1) |> to_string(),
      group_id: i |> elem(2) |> to_string(),
      inviter_id: i |> elem(3) |> to_string(),
      invitee_id: i |> elem(4) |> to_string(),
      status: elem(i, 5),
      message: i |> elem(6) |> to_string(),
      date_created: elem(i, 7),
      date_responded: elem(i, 8)
    }
  end

  defp safe_list(:undefined), do: []
  defp safe_list(l) when is_list(l), do: Enum.map(l, &to_string/1)
  defp safe_list(_), do: []

  defp is_owner?(group, uid), do: group.owner_id == uid

  def get_user_info(uid) do
    try do
      Core.UserClient.get_user_by_id(to_charlist(uid))
      |> then(fn u ->
        avatar =
          try do
            case elem(u, 29) do
              :undefined -> "/images/default-avatar.png"
              nil -> "/images/default-avatar.png"
              url when is_binary(url) -> url
              url when is_list(url) -> to_string(url)
              _ -> "/images/default-avatar.png"
            end
          rescue
            _ -> "/images/default-avatar.png"
          end

        %{
          id: to_string(uid),
          username: u |> elem(8) |> to_string(),
          avatar_url: avatar
        }
      end)
    rescue
      _ -> %{id: to_string(uid), username: "Unknown", avatar_url: "/images/default-avatar.png"}
    end
  end

  def format_datetime({{y, m, d}, {h, min, _}}),
    do:
      "#{d}/#{m}/#{y} #{String.pad_leading("#{h}", 2, "0")}:#{String.pad_leading("#{min}", 2, "0")}"

  def format_datetime(_), do: "Unknown"
end
