defmodule MazarynWeb.GroupLive.Index do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_groups = load_user_groups(user)
      user_channels = load_user_channels(user)
      all_public_groups = load_all_public_groups()
      all_public_channels = load_all_public_channels()

      socket =
        socket
        |> assign(:user, user)
        |> assign(:session_uuid, session_uuid)
        |> assign(:groups, user_groups)
        |> assign(:channels, user_channels)
        |> assign(:all_groups, all_public_groups)
        |> assign(:all_channels, all_public_channels)
        |> assign(:search_query, "")
        |> assign(:filter, "my_groups")
        |> assign(:show_create_modal, false)
        |> assign(:create_type, "group")
        |> assign(:form_data, reset_form_data())

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
        user_groups = load_user_groups(user)
        user_channels = load_user_channels(user)
        all_public_groups = load_all_public_groups()
        all_public_channels = load_all_public_channels()

        socket =
          socket
          |> assign(:user, user)
          |> assign(:user_id, user_id)
          |> assign(:groups, user_groups)
          |> assign(:channels, user_channels)
          |> assign(:all_groups, all_public_groups)
          |> assign(:all_channels, all_public_channels)
          |> assign(:search_query, "")
          |> assign(:filter, "my_groups")
          |> assign(:show_create_modal, false)
          |> assign(:create_type, "group")
          |> assign(:form_data, reset_form_data())

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

  @impl true
  def handle_event("clear_search", _, socket) do
    {:noreply, assign(socket, :search_query, "")}
  end

  @impl true
  def handle_event("filter_change", %{"filter" => filter}, socket) do
    {:noreply, assign(socket, :filter, filter)}
  end

  @impl true
  def handle_event("open_create_modal", %{"type" => type}, socket) do
    {:noreply,
     socket
     |> assign(:show_create_modal, true)
     |> assign(:create_type, type)
     |> assign(:form_data, reset_form_data())}
  end

  @impl true
  def handle_event("close_create_modal", _, socket) do
    {:noreply,
     socket
     |> assign(:show_create_modal, false)
     |> assign(:form_data, reset_form_data())}
  end

  @impl true
  def handle_event("update_form", params, socket) do
    form_data = Map.merge(socket.assigns.form_data, params)
    {:noreply, assign(socket, :form_data, form_data)}
  end

  @impl true
  def handle_event("create_group", params, socket) do
    user_id_str = extract_user_id(socket.assigns.user)

    name = String.trim(params["name"] || "")
    unique_name = String.trim(params["unique_name"] || "")
    description = String.trim(params["description"] || "")
    type = String.to_existing_atom(params["type"] || "general")
    privacy = String.to_existing_atom(params["privacy"] || "public")
    settings = %{}

    cond do
      name == "" or unique_name == "" ->
        {:noreply, put_flash(socket, :error, "Name and unique name are required")}

      true ->
        try do
          :groupdb.create_group(
            to_charlist(user_id_str),
            to_charlist(unique_name),
            to_charlist(name),
            to_charlist(description),
            type,
            privacy,
            settings
          )

          user_groups = load_user_groups(socket.assigns.user)

          {:noreply,
           socket
           |> assign(:groups, user_groups)
           |> assign(:show_create_modal, false)
           |> assign(:form_data, reset_form_data())
           |> put_flash(:info, "Group created successfully!")}
        rescue
          e in RuntimeError ->
            msg =
              case e.message do
                "unique_name_already_taken" -> "This @handle is already taken"
                _ -> "Failed to create group"
              end

            {:noreply, put_flash(socket, :error, msg)}

          _ ->
            {:noreply, put_flash(socket, :error, "Failed to create group")}
        end
    end
  end

  @impl true
  def handle_event("create_channel", params, socket) do
    user_id_str = extract_user_id(socket.assigns.user)

    name = String.trim(params["name"] || "")
    unique_name = String.trim(params["unique_name"] || "")
    description = String.trim(params["description"] || "")
    privacy = String.to_existing_atom(params["privacy"] || "public")
    category = String.trim(params["category"] || "")
    settings = %{}

    cond do
      name == "" or unique_name == "" ->
        {:noreply, put_flash(socket, :error, "Name and unique name are required")}

      true ->
        try do
          :groupdb.create_channel(
            to_charlist(user_id_str),
            to_charlist(unique_name),
            to_charlist(name),
            to_charlist(description),
            privacy,
            to_charlist(category),
            settings
          )

          user_channels = load_user_channels(socket.assigns.user)

          {:noreply,
           socket
           |> assign(:channels, user_channels)
           |> assign(:show_create_modal, false)
           |> assign(:form_data, reset_form_data())
           |> put_flash(:info, "Channel created successfully!")}
        rescue
          e in RuntimeError ->
            msg =
              case e.message do
                "unique_name_already_taken" -> "This @handle is already taken"
                _ -> "Failed to create channel"
              end

            {:noreply, put_flash(socket, :error, msg)}

          _ ->
            {:noreply, put_flash(socket, :error, "Failed to create channel")}
        end
    end
  end

  defp extract_user_id(user) do
    if is_binary(user.id), do: user.id, else: to_string(user.id)
  end

  defp load_user_groups(user) do
    user_id_str = extract_user_id(user)

    try do
      case :groupdb.get_member_groups(to_charlist(user_id_str)) do
        groups when is_list(groups) ->
          Enum.map(groups, &convert_group_record/1)

        _ ->
          []
      end
    rescue
      _ -> []
    end
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

  defp load_all_public_groups do
    try do
      case :groupdb.search_groups(to_charlist(""), :public) do
        groups when is_list(groups) ->
          Enum.map(groups, &convert_group_record/1)

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

  defp convert_group_record(group) do
    %{
      id: to_string(elem(group, 1)),
      unique_name: to_string(elem(group, 2)),
      name: to_string(elem(group, 3)),
      description: to_string(elem(group, 4) || ""),
      type: elem(group, 5),
      privacy: elem(group, 6),
      owner_id: to_string(elem(group, 7)),
      member_count: elem(group, 19) || 0,
      date_created: elem(group, 20)
    }
  end

  defp convert_channel_record(channel) do
    %{
      id: to_string(elem(channel, 1)),
      unique_name: to_string(elem(channel, 2)),
      name: to_string(elem(channel, 3)),
      description: to_string(elem(channel, 4) || ""),
      privacy: elem(channel, 5),
      owner_id: to_string(elem(channel, 6)),
      category: to_string(elem(channel, 9) || ""),
      subscriber_count: elem(channel, 11) || 0,
      date_created: elem(channel, 12)
    }
  end

  defp reset_form_data do
    %{
      "name" => "",
      "unique_name" => "",
      "description" => "",
      "type" => "general",
      "privacy" => "public",
      "category" => ""
    }
  end

  defp filtered_items(groups, channels, all_groups, all_channels, filter, search_query) do
    base_items =
      case filter do
        "my_groups" -> groups
        "my_channels" -> channels
        "discover_groups" -> all_groups
        "discover_channels" -> all_channels
        _ -> groups
      end

    if search_query == "" do
      base_items
    else
      query_lower = String.downcase(search_query)

      Enum.filter(base_items, fn item ->
        String.contains?(String.downcase(item.name), query_lower) ||
          String.contains?(String.downcase(item.unique_name), query_lower) ||
          String.contains?(String.downcase(item.description || ""), query_lower)
      end)
    end
  end
end
