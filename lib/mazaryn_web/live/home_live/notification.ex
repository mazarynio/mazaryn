defmodule MazarynWeb.HomeLive.Notification do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user_result =
      case session do
        %{"user_id" => user_email} -> Users.one_by_email(user_email)
        %{"session_uuid" => uuid} -> Users.get_by_session_uuid(uuid)
        _ -> {:error, :no_session}
      end

    case user_result do
      {:ok, user} ->
        Process.send_after(self(), :time_diff, 1000)

        notifs = get_all_user_notifs(user)
        group_invites = load_group_invites(user.id)
        channel_invites = load_channel_invites(user.id)

        send_update(MazarynWeb.HomeLive.NavComponent, id: "navigation", user: user)

        {:ok,
         socket
         |> assign(target_user: user)
         |> assign(user: user)
         |> assign(search: "")
         |> assign(notifs: notifs)
         |> assign(group_invites: group_invites)
         |> assign(channel_invites: channel_invites)}

      _ ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(_params, url, socket) do
    socket = assign(socket, current_path: URI.parse(url).path)
    MazarynWeb.HomeLive.NavComponent.handle_path(socket)
  end

  @impl true
  def handle_event("accept_group_invite", %{"invite_id" => invite_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    try do
      group_id_charlist = Core.GroupClient.accept_invite(invite_id, user_id)
      group_id = to_string(group_id_charlist)

      group_invites = load_group_invites(user_id)

      {:noreply,
       socket
       |> assign(group_invites: group_invites)
       |> put_flash(:info, "You've joined the group!")
       |> push_navigate(to: ~p"/#{socket.assigns.locale}/groups/#{group_id}")}
    rescue
      _ ->
        group_invites = load_group_invites(user_id)

        {:noreply,
         socket
         |> assign(group_invites: group_invites)
         |> put_flash(:error, "This invitation is no longer valid.")}
    end
  end

  @impl true
  def handle_event("reject_group_invite", %{"invite_id" => invite_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    try do
      Core.GroupClient.reject_invite(invite_id, user_id)
      group_invites = load_group_invites(user_id)

      {:noreply,
       socket
       |> assign(group_invites: group_invites)
       |> put_flash(:info, "Invitation declined")}
    rescue
      _ ->
        {:noreply, put_flash(socket, :error, "Failed to decline invitation")}
    end
  end

  @impl true
  def handle_event("accept_channel_invite", %{"invite_id" => invite_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    try do
      channel_id_charlist = Core.GroupClient.accept_channel_invite(invite_id, user_id)
      channel_id = to_string(channel_id_charlist)

      channel_invites = load_channel_invites(user_id)

      {:noreply,
       socket
       |> assign(channel_invites: channel_invites)
       |> put_flash(:info, "You've subscribed to the channel!")
       |> push_navigate(to: ~p"/#{socket.assigns.locale}/channels/#{channel_id}")}
    rescue
      _ ->
        channel_invites = load_channel_invites(user_id)

        {:noreply,
         socket
         |> assign(channel_invites: channel_invites)
         |> put_flash(:error, "This invitation is no longer valid.")}
    end
  end

  @impl true
  def handle_event("reject_channel_invite", %{"invite_id" => invite_id}, socket) do
    user_id = to_string(socket.assigns.user.id)

    try do
      Core.GroupClient.reject_channel_invite(invite_id, user_id)
      channel_invites = load_channel_invites(user_id)

      {:noreply,
       socket
       |> assign(channel_invites: channel_invites)
       |> put_flash(:info, "Invitation declined")}
    rescue
      _ ->
        {:noreply, put_flash(socket, :error, "Failed to decline invitation")}
    end
  end

  @impl true
  def handle_info(:time_diff, socket) do
    notifs =
      socket.assigns.notifs
      |> Enum.map(fn {user, message, _time_passed, time_stamp, type, data} ->
        time_passed = time_passed(time_stamp)
        {user, message, time_passed, time_stamp, type, data}
      end)

    Process.send_after(self(), :time_diff, 1000)

    {:noreply, assign(socket, :notifs, notifs)}
  end

  defp load_group_invites(user_id) do
    try do
      invites = Core.GroupClient.get_user_invites(user_id)
      Enum.map(invites, &convert_group_invite/1)
    rescue
      _ -> []
    end
  end

  defp load_channel_invites(user_id) do
    try do
      invites = Core.GroupClient.get_user_channel_invites(user_id)
      Enum.map(invites, &convert_channel_invite/1)
    rescue
      _ -> []
    end
  end

  defp convert_group_invite(invite) do
    group_info =
      try do
        group = Core.GroupClient.get_group(elem(invite, 2))

        %{
          id: group |> elem(1) |> to_string(),
          name: group |> elem(3) |> to_string()
        }
      rescue
        _ -> %{id: "unknown", name: "Unknown Group"}
      end

    inviter_info =
      try do
        inviter = Core.UserClient.get_user_by_id(elem(invite, 3))

        %{
          id: inviter |> elem(1) |> to_string(),
          username: inviter |> elem(8) |> to_string(),
          avatar_url: get_avatar(inviter)
        }
      rescue
        _ -> %{id: "unknown", username: "Unknown", avatar_url: "/images/default-avatar.png"}
      end

    %{
      id: invite |> elem(1) |> to_string(),
      group_id: invite |> elem(2) |> to_string(),
      inviter_id: invite |> elem(3) |> to_string(),
      invitee_id: invite |> elem(4) |> to_string(),
      status: elem(invite, 5),
      message: invite |> elem(6) |> to_string(),
      date_created: elem(invite, 7),
      group: group_info,
      inviter: inviter_info,
      type: :group
    }
  end

  defp convert_channel_invite(invite) do
    channel_info =
      try do
        channel = Core.GroupClient.get_channel(elem(invite, 2))

        %{
          id: channel |> elem(1) |> to_string(),
          name: channel |> elem(3) |> to_string()
        }
      rescue
        _ -> %{id: "unknown", name: "Unknown Channel"}
      end

    inviter_info =
      try do
        inviter = Core.UserClient.get_user_by_id(elem(invite, 3))

        %{
          id: inviter |> elem(1) |> to_string(),
          username: inviter |> elem(8) |> to_string(),
          avatar_url: get_avatar(inviter)
        }
      rescue
        _ -> %{id: "unknown", username: "Unknown", avatar_url: "/images/default-avatar.png"}
      end

    %{
      id: invite |> elem(1) |> to_string(),
      channel_id: invite |> elem(2) |> to_string(),
      inviter_id: invite |> elem(3) |> to_string(),
      invitee_id: invite |> elem(4) |> to_string(),
      status: elem(invite, 5),
      message: invite |> elem(6) |> to_string(),
      date_created: elem(invite, 7),
      channel: channel_info,
      inviter: inviter_info,
      type: :channel
    }
  end

  defp get_avatar(user_tuple) do
    try do
      case elem(user_tuple, 29) do
        :undefined -> "/images/default-avatar.png"
        nil -> "/images/default-avatar.png"
        url when is_binary(url) -> url
        url when is_list(url) -> to_string(url)
        _ -> "/images/default-avatar.png"
      end
    rescue
      _ -> "/images/default-avatar.png"
    end
  end

  defp get_all_user_notifs(user) do
    user.id
    |> Core.NotifEvent.get_all_notifs()
    |> Enum.map(fn notif ->
      case notif do
        {:notif, notif_id, actor_id, target_id, message, time_stamp, _read, _metadata} ->
          process_notif(notif_id, actor_id, target_id, message, time_stamp, :general, %{})

        {:notif, notif_id, actor_id, target_id, message, time_stamp, _read, _metadata, extra} ->
          process_notif(notif_id, actor_id, target_id, message, time_stamp, :general, extra)

        _ ->
          nil
      end
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_notif(notif_id, actor_id, target_id, message, time_stamp, type, data) do
    try do
      Core.NotifEvent.mark_notif_as_read(notif_id)

      case get_user(actor_id, target_id) do
        {:ok, user} ->
          time_passed = time_passed(time_stamp)
          {user, to_string(message), time_passed, time_stamp, type, data}

        {:error, _reason} ->
          nil
      end
    rescue
      _ -> nil
    end
  end

  defp time_passed(time_stamp) do
    date_time = Timex.to_datetime(time_stamp)
    time_difference([:years, :months, :weeks, :days, :hours, :minutes, :seconds], date_time)
  end

  defp time_difference([:seconds], date_time) do
    case Timex.diff(Timex.now(), date_time, :seconds) do
      0 -> "Now"
      diff -> "#{diff} seconds ago"
    end
  end

  defp time_difference([h | t] = _granularities, date_time) do
    case Timex.diff(Timex.now(), date_time, h) do
      0 -> time_difference(t, date_time)
      diff -> "#{diff} #{h} ago"
    end
  end

  defp get_user(actor_id, target_id) do
    id =
      case actor_id do
        :undefined -> target_id
        actor_id -> actor_id
      end

    Users.one_by_id(id)
  end

  def format_datetime({{y, m, d}, {h, min, _}}),
    do:
      "#{d}/#{m}/#{y} #{String.pad_leading("#{h}", 2, "0")}:#{String.pad_leading("#{min}", 2, "0")}"

  def format_datetime(_), do: "Unknown"
end
