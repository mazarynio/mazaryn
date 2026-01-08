defmodule MazarynWeb.GroupLive.Members do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => group_id_from_url}, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🔍 Members mount - URL param: '#{group_id_from_url}'")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      Logger.info("✅ User authenticated: #{user.username}")

      group = load_group(group_id_from_url) || load_group(group_id_from_url <> "_")

      if group do
        Logger.info("✅ Group loaded: #{group.name}")

        user_id_str = extract_user_id(user)
        is_member = is_member?(group, user_id_str)
        is_owner = is_owner?(group, user_id_str)
        is_admin = is_admin?(group, user_id_str)

        Logger.info(
          "👤 User: #{user_id_str}, Member: #{is_member}, Owner: #{is_owner}, Admin: #{is_admin}"
        )

        if is_member do
          members = load_group_members(group.id)
          Logger.info("✅ Loaded #{length(members)} members")

          {:ok,
           socket
           |> assign(user: user)
           |> assign(session_uuid: session_uuid)
           |> assign(group: group)
           |> assign(group_id: group.id)
           |> assign(is_member: is_member)
           |> assign(is_owner: is_owner)
           |> assign(is_admin: is_admin)
           |> assign(members: members)
           |> assign(search_query: "")
           |> assign(show_admin_modal: false)
           |> assign(selected_member: nil)}
        else
          Logger.error("❌ User not a member")

          {:ok,
           socket
           |> put_flash(:error, "You must be a member to view members")
           |> redirect(to: ~p"/#{socket.assigns.locale}/groups/#{group.id}")}
        end
      else
        Logger.error("❌ Group not found")

        {:ok,
         socket
         |> put_flash(:error, "Group not found")
         |> redirect(to: ~p"/#{socket.assigns.locale}/groups")}
      end
    else
      {:error, reason} ->
        Logger.error("❌ Auth error: #{inspect(reason)}")

        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(%{"id" => group_id_from_url}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        group = load_group(group_id_from_url) || load_group(group_id_from_url <> "_")

        if group do
          user_id_str = extract_user_id(user)
          is_member = is_member?(group, user_id_str)
          is_owner = is_owner?(group, user_id_str)
          is_admin = is_admin?(group, user_id_str)

          if is_member do
            members = load_group_members(group.id)

            {:ok,
             socket
             |> assign(user: user)
             |> assign(user_id: user_id)
             |> assign(group: group)
             |> assign(group_id: group.id)
             |> assign(is_member: is_member)
             |> assign(is_owner: is_owner)
             |> assign(is_admin: is_admin)
             |> assign(members: members)
             |> assign(search_query: "")
             |> assign(show_admin_modal: false)
             |> assign(selected_member: nil)}
          else
            {:ok,
             socket
             |> put_flash(:error, "You must be a member to view members")
             |> redirect(to: ~p"/#{socket.assigns.locale}/groups/#{group.id}")}
          end
        else
          {:ok,
           socket
           |> put_flash(:error, "Group not found")
           |> redirect(to: ~p"/#{socket.assigns.locale}/groups")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("search", %{"value" => query}, socket) do
    {:noreply, assign(socket, search_query: query)}
  end

  @impl true
  def handle_event("remove_member", %{"member_id" => member_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    group_id = socket.assigns.group_id

    try do
      Core.GroupClient.remove_member(group_id, member_id, user_id_str)

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
  def handle_event("make_admin", %{"member_id" => member_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    group_id = socket.assigns.group_id

    try do
      Core.GroupClient.add_admin(group_id, member_id, user_id_str, [:all])

      members = load_group_members(group_id)
      group = load_group(group_id)

      {:noreply,
       socket
       |> assign(members: members)
       |> assign(group: group)
       |> put_flash(:info, "Member promoted to admin")}
    rescue
      error ->
        Logger.error("Failed to make admin: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to promote member")}
    end
  end

  @impl true
  def handle_event("remove_admin", %{"member_id" => member_id}, socket) do
    group_id = socket.assigns.group_id

    try do
      Core.GroupClient.remove_admin(group_id, member_id)

      members = load_group_members(group_id)
      group = load_group(group_id)

      {:noreply,
       socket
       |> assign(members: members)
       |> assign(group: group)
       |> put_flash(:info, "Admin privileges removed")}
    rescue
      error ->
        Logger.error("Failed to remove admin: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to remove admin")}
    end
  end

  defp extract_user_id(user) do
    cond do
      is_binary(user.id) -> user.id
      is_list(user.id) -> to_string(user.id)
      true -> to_string(user.id)
    end
  end

  defp load_group(group_id) do
    try do
      group_tuple = Core.GroupClient.get_group(group_id)
      convert_group_record(group_tuple)
    rescue
      _ -> nil
    end
  end

  defp load_group_members(group_id) do
    try do
      members = Core.GroupClient.get_group_members(group_id)
      Enum.map(members, &convert_member_record/1)
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
    %{
      id: group |> elem(1) |> to_string(),
      unique_name: group |> elem(2) |> to_string(),
      name: group |> elem(3) |> to_string(),
      description: group |> elem(4) |> to_string(),
      type: elem(group, 5),
      privacy: elem(group, 6),
      owner_id: group |> elem(7) |> to_string(),
      admins: safe_list(elem(group, 10)),
      members: safe_list(elem(group, 11)),
      settings: elem(group, 16),
      banned_users: safe_list(elem(group, 12)),
      member_count: elem(group, 19),
      date_created: elem(group, 21)
    }
  end

  defp convert_member_record(member) do
    %{
      id: member |> elem(1) |> to_string(),
      group_id: member |> elem(2) |> to_string(),
      user_id: member |> elem(3) |> to_string(),
      role: elem(member, 4),
      permissions: elem(member, 5),
      join_date: elem(member, 6),
      invited_by: if(elem(member, 7) == :undefined, do: nil, else: to_string(elem(member, 7))),
      muted: elem(member, 8),
      notifications: elem(member, 9)
    }
  end

  defp safe_list(:undefined), do: []
  defp safe_list(l) when is_list(l), do: Enum.map(l, &to_string/1)
  defp safe_list(_), do: []

  def get_user_info(user_id) do
    try do
      user_tuple = Core.UserClient.get_user_by_id(user_id)

      case user_tuple do
        tuple when is_tuple(tuple) and tuple_size(tuple) > 8 ->
          username = tuple |> elem(8) |> to_string()

          avatar_url =
            try do
              case elem(tuple, 29) do
                :undefined -> "/images/default-avatar.png"
                nil -> "/images/default-avatar.png"
                url when is_list(url) -> to_string(url)
                url when is_binary(url) -> url
                _ -> "/images/default-avatar.png"
              end
            rescue
              _ -> "/images/default-avatar.png"
            end

          %{
            id: to_string(user_id),
            username: username,
            avatar_url: avatar_url
          }

        _ ->
          %{
            id: to_string(user_id),
            username: "Unknown",
            avatar_url: "/images/default-avatar.png"
          }
      end
    rescue
      error ->
        Logger.error("Failed to get user info for #{user_id}: #{inspect(error)}")

        %{
          id: to_string(user_id),
          username: "Unknown",
          avatar_url: "/images/default-avatar.png"
        }
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
    query = String.trim(search_query) |> String.downcase()

    if query == "" do
      members
    else
      Enum.filter(members, fn member ->
        user_info = get_user_info(member.user_id)
        String.contains?(String.downcase(user_info.username), query)
      end)
    end
  end
end
