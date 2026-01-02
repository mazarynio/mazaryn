defmodule MazarynWeb.GroupLive.Members do
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
        is_member = is_member?(group, user_id_str)
        is_owner = is_owner?(group, user_id_str)
        is_admin = is_admin?(group, user_id_str)

        if is_member do
          members = load_group_members(group_id)

          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(group: group)
            |> assign(is_member: is_member)
            |> assign(is_owner: is_owner)
            |> assign(is_admin: is_admin)
            |> assign(members: members)
            |> assign(search_query: "")
            |> assign(show_admin_modal: false)
            |> assign(selected_member: nil)

          {:ok, socket}
        else
          {:ok,
           socket
           |> put_flash(:error, "You must be a member to view members")
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
          is_member = is_member?(group, user_id_str)
          is_owner = is_owner?(group, user_id_str)
          is_admin = is_admin?(group, user_id_str)

          if is_member do
            members = load_group_members(group_id)

            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(group: group)
              |> assign(is_member: is_member)
              |> assign(is_owner: is_owner)
              |> assign(is_admin: is_admin)
              |> assign(members: members)
              |> assign(search_query: "")
              |> assign(show_admin_modal: false)
              |> assign(selected_member: nil)

            {:ok, socket}
          else
            {:ok,
             socket
             |> put_flash(:error, "You must be a member to view members")
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
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, assign(socket, search_query: query)}
  end

  @impl true
  def handle_event("remove_member", %{"member_id" => member_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    group_id = socket.assigns.group.id

    try do
      :groupdb.remove_member(
        to_charlist(group_id),
        to_charlist(member_id),
        to_charlist(user_id_str)
      )

      members = load_group_members(group_id)
      group = load_group(group_id)

      {:noreply,
       socket
       |> assign(members: members)
       |> assign(group: group)
       |> put_flash(:info, "Member removed successfully")}
    rescue
      error ->
        Logger.error("Failed to remove member: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to remove member")}
    end
  end

  @impl true
  def handle_event("open_admin_modal", %{"member_id" => member_id}, socket) do
    member = Enum.find(socket.assigns.members, fn m -> m.user_id == member_id end)
    {:noreply, assign(socket, show_admin_modal: true, selected_member: member)}
  end

  @impl true
  def handle_event("close_admin_modal", _params, socket) do
    {:noreply, assign(socket, show_admin_modal: false, selected_member: nil)}
  end

  @impl true
  def handle_event("make_admin", %{"member_id" => member_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    group_id = socket.assigns.group.id

    try do
      :groupdb.add_admin(
        to_charlist(group_id),
        to_charlist(member_id),
        to_charlist(user_id_str),
        [:all]
      )

      members = load_group_members(group_id)
      group = load_group(group_id)

      {:noreply,
       socket
       |> assign(members: members)
       |> assign(group: group)
       |> assign(show_admin_modal: false)
       |> assign(selected_member: nil)
       |> put_flash(:info, "Member promoted to admin")}
    rescue
      error ->
        Logger.error("Failed to make admin: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to promote member")}
    end
  end

  @impl true
  def handle_event("remove_admin", %{"member_id" => member_id}, socket) do
    group_id = socket.assigns.group.id

    try do
      :groupdb.remove_admin(to_charlist(group_id), to_charlist(member_id))

      members = load_group_members(group_id)
      group = load_group(group_id)

      {:noreply,
       socket
       |> assign(members: members)
       |> assign(group: group)
       |> assign(show_admin_modal: false)
       |> assign(selected_member: nil)
       |> put_flash(:info, "Admin privileges removed")}
    rescue
      error ->
        Logger.error("Failed to remove admin: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to remove admin privileges")}
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

  defp load_group_members(group_id) do
    try do
      case :groupdb.get_group_members(to_charlist(group_id)) do
        members when is_list(members) ->
          Enum.map(members, &convert_member_record/1)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp is_member?(group, user_id) do
    Enum.any?(group.members, fn member_id ->
      to_string(member_id) == to_string(user_id)
    end)
  end

  defp is_owner?(group, user_id) do
    to_string(group.owner_id) == to_string(user_id)
  end

  defp is_admin?(group, user_id) do
    member_result =
      Enum.any?(group.admins, fn admin_id ->
        to_string(admin_id) == to_string(user_id)
      end)

    owner_result = is_owner?(group, user_id)
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

  defp convert_member_record(member) do
    %{
      id: to_string(elem(member, 1)),
      group_id: to_string(elem(member, 2)),
      user_id: to_string(elem(member, 3)),
      role: elem(member, 4),
      permissions: elem(member, 5),
      join_date: elem(member, 6),
      invited_by: if(elem(member, 7) == :undefined, do: nil, else: to_string(elem(member, 7))),
      muted: elem(member, 8),
      notifications: elem(member, 9)
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

  def format_date(datetime) do
    case datetime do
      {{year, month, day}, _time} ->
        "#{day}/#{month}/#{year}"

      _ ->
        "Unknown"
    end
  end

  def filtered_members(members, search_query) do
    if String.trim(search_query) == "" do
      members
    else
      query = String.downcase(search_query)

      Enum.filter(members, fn member ->
        user_info = get_user_info(member.user_id)
        String.contains?(String.downcase(user_info.username), query)
      end)
    end
  end
end
