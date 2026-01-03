defmodule MazarynWeb.ChannelLive.Settings do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => channel_id}, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      channel = load_channel(channel_id)

      if channel do
        user_id_str = extract_user_id(user)
        is_owner = is_owner?(channel, user_id_str)

        if is_owner do
          socket =
            socket
            |> assign(user: user)
            |> assign(session_uuid: session_uuid)
            |> assign(channel: channel)
            |> assign(is_owner: is_owner)
            |> assign(
              form_data: %{
                "name" => channel.name,
                "unique_name" => channel.unique_name,
                "description" => channel.description,
                "privacy" => Atom.to_string(channel.privacy),
                "category" => channel.category
              }
            )

          {:ok, socket}
        else
          {:ok,
           socket
           |> put_flash(:error, "Only channel owners can access settings")
           |> redirect(to: "/#{socket.assigns.locale}/channels/#{channel_id}")}
        end
      else
        {:ok,
         socket
         |> put_flash(:error, "Channel not found")
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
  def mount(%{"id" => channel_id}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        channel = load_channel(channel_id)

        if channel do
          user_id_str = extract_user_id(user)
          is_owner = is_owner?(channel, user_id_str)

          if is_owner do
            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(channel: channel)
              |> assign(is_owner: is_owner)
              |> assign(
                form_data: %{
                  "name" => channel.name,
                  "unique_name" => channel.unique_name,
                  "description" => channel.description,
                  "privacy" => Atom.to_string(channel.privacy),
                  "category" => channel.category
                }
              )

            {:ok, socket}
          else
            {:ok,
             socket
             |> put_flash(:error, "Only channel owners can access settings")
             |> redirect(to: "/#{socket.assigns.locale}/channels/#{channel_id}")}
          end
        else
          {:ok,
           socket
           |> put_flash(:error, "Channel not found")
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
  def handle_event("update_channel", params, socket) do
    channel_id = socket.assigns.channel.id

    update_map = %{
      name: params["name"],
      unique_name: params["unique_name"],
      description: params["description"],
      privacy: String.to_atom(params["privacy"]),
      category: params["category"]
    }

    try do
      :groupdb.update_channel(to_charlist(channel_id), update_map)

      channel = load_channel(channel_id)

      {:noreply,
       socket
       |> assign(channel: channel)
       |> put_flash(:info, "Channel settings updated successfully!")}
    rescue
      error ->
        Logger.error("Failed to update channel: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to update channel settings")}
    end
  end

  @impl true
  def handle_event("delete_channel", _params, socket) do
    channel_id = socket.assigns.channel.id

    try do
      :groupdb.delete_channel(to_charlist(channel_id))

      {:noreply,
       socket
       |> put_flash(:info, "Channel deleted successfully")
       |> redirect(to: "/#{socket.assigns.locale}/groups")}
    rescue
      error ->
        Logger.error("Failed to delete channel: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to delete channel")}
    end
  end

  defp extract_user_id(user) do
    if is_binary(user.id), do: user.id, else: to_string(user.id)
  end

  defp load_channel(channel_id) do
    try do
      channel = :groupdb.get_channel(to_charlist(channel_id))
      convert_channel_record(channel)
    rescue
      _ -> nil
    catch
      :throw, {:transaction_failed, {:throw, :channel_not_found}} -> nil
      :throw, _ -> nil
    end
  end

  defp is_owner?(channel, user_id) do
    to_string(channel.owner_id) == to_string(user_id)
  end

  defp convert_channel_record(channel) do
    admins_raw = elem(channel, 7)
    subscribers_raw = elem(channel, 8)
    category_raw = elem(channel, 9)

    admins =
      case admins_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    subscribers =
      case subscribers_raw do
        :undefined -> []
        list when is_list(list) -> Enum.map(list, &to_string/1)
        _ -> []
      end

    category =
      case category_raw do
        :undefined -> ""
        [] -> ""
        cat when is_list(cat) ->
          cat_string = to_string(cat)
          if valid_category?(cat_string), do: cat_string, else: ""
        cat when is_binary(cat) ->
          if valid_category?(cat), do: cat, else: ""
        _ -> ""
      end

    %{
      id: to_string(elem(channel, 1)),
      unique_name: to_string(elem(channel, 2)),
      name: to_string(elem(channel, 3)),
      description: to_string(elem(channel, 4) || ""),
      privacy: elem(channel, 5),
      owner_id: to_string(elem(channel, 6)),
      admins: admins,
      subscribers: subscribers,
      category: category,
      subscriber_count: elem(channel, 11) || 0,
      date_created: elem(channel, 12)
    }
  end

  defp valid_category?(category) when is_binary(category) do
    if String.trim(category) == "" do
      false
    else
      len = String.length(category)
      if len > 50 do
        false
      else
        has_space = String.contains?(category, " ")
        has_comma = String.contains?(category, ",")
        is_short = len <= 20

        has_space or has_comma or is_short
      end
    end
  end

  defp valid_category?(_), do: false
end
