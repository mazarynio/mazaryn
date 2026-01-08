defmodule MazarynWeb.ChannelLive.Subscribers do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(%{"id" => channel_id_from_url}, %{"session_uuid" => session_uuid} = _session, socket) do
    Logger.info("🔍 Subscribers mount - URL param: '#{channel_id_from_url}'")

    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      Logger.info("✅ User authenticated: #{user.username}")

      channel = load_channel(channel_id_from_url) || load_channel(channel_id_from_url <> "_")

      if channel do
        Logger.info("✅ Channel loaded: #{channel.name}")

        user_id_str = extract_user_id(user)
        is_subscriber = is_subscriber?(channel, user_id_str)
        is_owner = is_owner?(channel, user_id_str)
        is_admin = is_admin?(channel, user_id_str)

        Logger.info(
          "👤 User: #{user_id_str}, Subscriber: #{is_subscriber}, Owner: #{is_owner}, Admin: #{is_admin}"
        )

        if is_admin || is_subscriber do
          subscribers = load_channel_subscribers(channel.id)
          Logger.info("✅ Loaded #{length(subscribers)} subscribers")

          {:ok,
           socket
           |> assign(user: user)
           |> assign(session_uuid: session_uuid)
           |> assign(channel: channel)
           |> assign(channel_id: channel.id)
           |> assign(is_subscriber: is_subscriber)
           |> assign(is_owner: is_owner)
           |> assign(is_admin: is_admin)
           |> assign(subscribers: subscribers)
           |> assign(search_query: "")}
        else
          Logger.error("❌ User not a subscriber or admin")

          {:ok,
           socket
           |> put_flash(:error, "You must be a subscriber to view subscribers")
           |> redirect(to: ~p"/#{socket.assigns.locale}/channels/#{channel.id}")}
        end
      else
        Logger.error("❌ Channel not found")

        {:ok,
         socket
         |> put_flash(:error, "Channel not found")
         |> redirect(to: ~p"/#{socket.assigns.locale}/channels")}
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
  def mount(%{"id" => channel_id_from_url}, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        channel = load_channel(channel_id_from_url) || load_channel(channel_id_from_url <> "_")

        if channel do
          user_id_str = extract_user_id(user)
          is_subscriber = is_subscriber?(channel, user_id_str)
          is_owner = is_owner?(channel, user_id_str)
          is_admin = is_admin?(channel, user_id_str)

          if is_admin || is_subscriber do
            subscribers = load_channel_subscribers(channel.id)

            {:ok,
             socket
             |> assign(user: user)
             |> assign(user_id: user_id)
             |> assign(channel: channel)
             |> assign(channel_id: channel.id)
             |> assign(is_subscriber: is_subscriber)
             |> assign(is_owner: is_owner)
             |> assign(is_admin: is_admin)
             |> assign(subscribers: subscribers)
             |> assign(search_query: "")}
          else
            {:ok,
             socket
             |> put_flash(:error, "You must be a subscriber to view subscribers")
             |> redirect(to: ~p"/#{socket.assigns.locale}/channels/#{channel.id}")}
          end
        else
          {:ok,
           socket
           |> put_flash(:error, "Channel not found")
           |> redirect(to: ~p"/#{socket.assigns.locale}/channels")}
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
  def handle_event("remove_subscriber", %{"subscriber_id" => subscriber_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    channel_id = socket.assigns.channel_id

    try do
      Core.GroupClient.unsubscribe_channel(subscriber_id, channel_id)

      subscribers = load_channel_subscribers(channel_id)
      channel = load_channel(channel_id)

      {:noreply,
       socket
       |> assign(subscribers: subscribers)
       |> assign(channel: channel)
       |> put_flash(:info, "Subscriber removed successfully")}
    rescue
      error ->
        Logger.error("Failed to remove subscriber: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to remove subscriber")}
    end
  end

  @impl true
  def handle_event("make_admin", %{"subscriber_id" => subscriber_id}, socket) do
    user_id_str = extract_user_id(socket.assigns.user)
    channel_id = socket.assigns.channel_id

    try do
      Core.GroupClient.add_channel_admin(channel_id, subscriber_id, user_id_str)

      subscribers = load_channel_subscribers(channel_id)
      channel = load_channel(channel_id)

      {:noreply,
       socket
       |> assign(subscribers: subscribers)
       |> assign(channel: channel)
       |> put_flash(:info, "Subscriber promoted to admin")}
    rescue
      error ->
        Logger.error("Failed to make admin: #{inspect(error)}")
        {:noreply, put_flash(socket, :error, "Failed to promote subscriber")}
    end
  end

  @impl true
  def handle_event("remove_admin", %{"subscriber_id" => subscriber_id}, socket) do
    channel_id = socket.assigns.channel_id

    try do
      Core.GroupClient.remove_channel_admin(channel_id, subscriber_id)

      subscribers = load_channel_subscribers(channel_id)
      channel = load_channel(channel_id)

      {:noreply,
       socket
       |> assign(subscribers: subscribers)
       |> assign(channel: channel)
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

  defp load_channel(channel_id) do
    try do
      channel_tuple = Core.GroupClient.get_channel(channel_id)
      convert_channel_record(channel_tuple)
    rescue
      _ -> nil
    end
  end

  defp load_channel_subscribers(channel_id) do
    try do
      subscribers = Core.GroupClient.get_channel_subscribers(channel_id)
      Enum.map(subscribers, &convert_subscriber_record/1)
    rescue
      _ -> []
    end
  end

  defp is_subscriber?(channel, user_id) do
    Enum.any?(channel.subscribers, fn subscriber_id ->
      to_string(subscriber_id) == to_string(user_id)
    end)
  end

  defp is_owner?(channel, user_id) do
    to_string(channel.owner_id) == to_string(user_id)
  end

  defp is_admin?(channel, user_id) do
    admin_result =
      Enum.any?(channel.admins, fn admin_id ->
        to_string(admin_id) == to_string(user_id)
      end)

    owner_result = is_owner?(channel, user_id)
    admin_result || owner_result
  end

  defp convert_channel_record(channel) do
    admins_raw = elem(channel, 7)
    subscribers_raw = elem(channel, 8)

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

    %{
      id: to_string(elem(channel, 1)),
      unique_name: to_string(elem(channel, 2)),
      name: to_string(elem(channel, 3)),
      description: to_string(elem(channel, 4) || ""),
      privacy: elem(channel, 5),
      owner_id: to_string(elem(channel, 6)),
      admins: admins,
      subscribers: subscribers,
      category: to_string(elem(channel, 9) || ""),
      subscriber_count: elem(channel, 15),
      date_created: elem(channel, 12)
    }
  end

  defp convert_subscriber_record(subscriber) do
    %{
      id: subscriber |> elem(1) |> to_string(),
      channel_id: subscriber |> elem(2) |> to_string(),
      user_id: subscriber |> elem(3) |> to_string(),
      subscribe_date: elem(subscriber, 4)
    }
  end

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

  def filtered_subscribers(subscribers, channel, search_query) do
    query = String.trim(search_query) |> String.downcase()

    filtered =
      if query == "" do
        subscribers
      else
        Enum.filter(subscribers, fn subscriber ->
          user_info = get_user_info(subscriber.user_id)
          String.contains?(String.downcase(user_info.username), query)
        end)
      end

    Enum.map(filtered, fn subscriber ->
      is_owner = subscriber.user_id == channel.owner_id
      is_admin = Enum.member?(channel.admins, subscriber.user_id)

      role =
        cond do
          is_owner -> :owner
          is_admin -> :admin
          true -> :subscriber
        end

      Map.put(subscriber, :role, role)
    end)
  end
end
