defmodule MazarynWeb.ChannelLive.Index do
  use MazarynWeb, :live_view
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_channels = load_user_channels(user)
      all_public_channels = load_all_public_channels()

      socket =
        socket
        |> assign(:user, user)
        |> assign(:session_uuid, session_uuid)
        |> assign(:channels, user_channels)
        |> assign(:all_channels, all_public_channels)
        |> assign(:search_query, "")

      {:ok, socket}
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        user_channels = load_user_channels(user)
        all_public_channels = load_all_public_channels()

        socket =
          socket
          |> assign(:user, user)
          |> assign(:user_id, user_id)
          |> assign(:channels, user_channels)
          |> assign(:all_channels, all_public_channels)
          |> assign(:search_query, "")

        {:ok, socket}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, assign(socket, :search_query, String.trim(query))}
  end

  defp extract_user_id(user) do
    if is_binary(user.id), do: user.id, else: to_string(user.id)
  end

  defp load_user_channels(user) do
    user_id_str = extract_user_id(user)

    try do
      case :groupdb.get_user_channels(to_charlist(user_id_str)) do
        channels when is_list(channels) ->
          Enum.map(channels, &convert_channel_record/1)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp load_all_public_channels do
    try do
      case :groupdb.search_channels(to_charlist(""), :public) do
        channels when is_list(channels) ->
          Enum.map(channels, &convert_channel_record/1)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp convert_channel_record(channel) do
    owner_id = to_string(elem(channel, 6))

    # DEBUG: Log all elements to find subscriber_count
    Logger.info("🔍 DEBUG CHANNEL RECORD:")
    Logger.info("   Tuple size: #{tuple_size(channel)}")

    for i <- 0..(tuple_size(channel) - 1) do
      value = elem(channel, i)
      Logger.info("   elem(#{i}): #{inspect(value)}")
    end

    %{
      id: to_string(elem(channel, 1)),
      unique_name: to_string(elem(channel, 2)),
      name: to_string(elem(channel, 3)),
      description: to_string(elem(channel, 4) || ""),
      privacy: elem(channel, 5),
      owner_id: owner_id,
      owner_username: get_username_by_id(owner_id),
      category: to_string(elem(channel, 9) || ""),
      subscriber_count: elem(channel, 15) || 0,
      date_created: elem(channel, 12)
    }
  end

  defp get_username_by_id(user_id) do
    try do
      charlist_id = if is_binary(user_id), do: to_charlist(user_id), else: user_id

      case Core.UserClient.get_user_by_id(charlist_id) do
        {:error, _reason} ->
          "Unknown"

        :user_not_exist ->
          "Unknown"

        user_tuple when is_tuple(user_tuple) ->
          elem(user_tuple, 8) |> to_string()

        _ ->
          "Unknown"
      end
    rescue
      _ -> "Unknown"
    end
  end

  def get_user_avatar(username) when is_binary(username) do
    try do
      case Account.Users.one_by_username(username) do
        {:ok, user} ->
          case user.avatar_url do
            nil -> "/images/default-avatar.png"
            "" -> "/images/default-avatar.png"
            url -> url
          end

        {:error, _} ->
          "/images/default-avatar.png"
      end
    rescue
      _ -> "/images/default-avatar.png"
    end
  end

  def get_user_avatar(_), do: "/images/default-avatar.png"

  def filtered_channels(channels, search_query) do
    if search_query == "" do
      channels
    else
      query_lower = String.downcase(search_query)

      Enum.filter(channels, fn channel ->
        String.contains?(String.downcase(channel.name), query_lower) ||
          String.contains?(String.downcase(channel.unique_name), query_lower) ||
          String.contains?(String.downcase(channel.description || ""), query_lower) ||
          String.contains?(String.downcase(channel.owner_username || ""), query_lower)
      end)
    end
  end
end
