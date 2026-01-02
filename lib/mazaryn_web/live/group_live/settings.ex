defmodule MazarynWeb.GroupLive.Settings do
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
        is_owner = is_owner?(group, user_id_str)

        if is_owner do
          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(group: group)
            |> assign(is_owner: is_owner)
            |> assign(
              form_data: %{
                "name" => group.name,
                "unique_name" => group.unique_name,
                "description" => group.description,
                "privacy" => Atom.to_string(group.privacy),
                "type" => Atom.to_string(group.type)
              }
            )

          {:ok, socket}
        else
          {:ok,
           socket
           |> put_flash(:error, "Only group owners can access settings")
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
          is_owner = is_owner?(group, user_id_str)

          if is_owner do
            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(group: group)
              |> assign(is_owner: is_owner)
              |> assign(
                form_data: %{
                  "name" => group.name,
                  "unique_name" => group.unique_name,
                  "description" => group.description,
                  "privacy" => Atom.to_string(group.privacy),
                  "type" => Atom.to_string(group.type)
                }
              )

            {:ok, socket}
          else
            {:ok,
             socket
             |> put_flash(:error, "Only group owners can access settings")
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
  def handle_event("update_form", params, socket) do
    form_data = Map.merge(socket.assigns.form_data, params)
    {:noreply, assign(socket, form_data: form_data)}
  end

  @impl true
  def handle_event("update_group", params, socket) do
    group_id = socket.assigns.group.id

    update_map = %{
      name: params["name"],
      unique_name: params["unique_name"],
      description: params["description"],
      privacy: String.to_atom(params["privacy"]),
      type: String.to_atom(params["type"])
    }

    try do
      :groupdb.update_group(to_charlist(group_id), update_map)

      group = load_group(group_id)

      {:noreply,
       socket
       |> assign(group: group)
       |> put_flash(:info, "Group settings updated successfully!")}
    rescue
      error ->
        Logger.error("Failed to update group: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to update group settings")}
    end
  end

  @impl true
  def handle_event("delete_group", _params, socket) do
    group_id = socket.assigns.group.id

    try do
      :groupdb.delete_group(to_charlist(group_id))

      {:noreply,
       socket
       |> put_flash(:info, "Group deleted successfully")
       |> redirect(to: "/#{socket.assigns.locale}/groups")}
    rescue
      error ->
        Logger.error("Failed to delete group: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to delete group")}
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

  defp is_owner?(group, user_id) do
    to_string(group.owner_id) == to_string(user_id)
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
end
