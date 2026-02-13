defmodule MazarynWeb.AdminLive.ArtistRequests do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)
    username = get_username(user)
    is_admin = is_admin_user(username)

    if not is_admin do
      {:ok,
       socket
       |> put_flash(:error, "Unauthorized access")
       |> push_navigate(to: ~p"/#{socket.assigns[:locale] || "en"}/home")}
    else
      {:ok,
       socket
       |> assign(:page_title, "Artist Requests")
       |> assign(:user, user)
       |> assign(:username, username)
       |> assign(:locale, socket.assigns[:locale] || "en")
       |> assign(:requests, [])
       |> assign(:selected_request, nil)
       |> assign(:rejection_reason, "")
       |> assign(:filter, "pending")
       |> assign(:loading, true)
       |> load_requests()}
    end
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("select_request", %{"id" => request_id}, socket) do
    request =
      Enum.find(socket.assigns.requests, fn r ->
        to_string(r.id) == to_string(request_id)
      end)

    {:noreply,
     socket
     |> assign(:selected_request, request)
     |> assign(:rejection_reason, "")}
  end

  @impl true
  def handle_event("close_detail", _params, socket) do
    {:noreply, socket |> assign(:selected_request, nil) |> assign(:rejection_reason, "")}
  end

  @impl true
  def handle_event("change_filter", %{"filter" => filter}, socket) do
    {:noreply,
     socket
     |> assign(:filter, filter)
     |> assign(:selected_request, nil)
     |> assign(:loading, true)
     |> load_requests()}
  end

  @impl true
  def handle_event("approve_request", %{"id" => request_id}, socket) do
    username = socket.assigns.username
    erlang_request_id = to_erlang_chlist(request_id)
    erlang_username = to_erlang_chlist(username)

    case :musicdb.approve_artist_request(erlang_request_id, erlang_username) do
      {:ok, _artist_id} ->
        {:noreply,
         socket
         |> load_requests()
         |> assign(:selected_request, nil)
         |> put_flash(:info, "Artist request approved! Artist profile created.")}

      {:error, :unauthorized} ->
        {:noreply, put_flash(socket, :error, "Unauthorized: admin check failed")}

      {:error, :not_found} ->
        {:noreply, put_flash(socket, :error, "Request not found in database")}

      {:error, reason} ->
        {:noreply, put_flash(socket, :error, "Failed to approve: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("validate_rejection", %{"rejection" => %{"reason" => reason}}, socket) do
    {:noreply, assign(socket, :rejection_reason, reason)}
  end

  @impl true
  def handle_event("reject_request", %{"rejection" => %{"reason" => reason}}, socket) do
    request_id = socket.assigns.selected_request.id
    username = socket.assigns.username

    if reason == "" or String.trim(reason) == "" do
      {:noreply, put_flash(socket, :error, "Please provide a rejection reason")}
    else
      erlang_request_id = to_erlang_chlist(request_id)
      erlang_username = to_erlang_chlist(username)
      erlang_reason = to_erlang_chlist(reason)

      case :musicdb.reject_artist_request(erlang_request_id, erlang_username, erlang_reason) do
        {:ok, :rejected} ->
          {:noreply,
           socket
           |> load_requests()
           |> assign(:selected_request, nil)
           |> assign(:rejection_reason, "")
           |> put_flash(:info, "Artist request rejected")}

        {:error, :unauthorized} ->
          {:noreply, put_flash(socket, :error, "Unauthorized: admin check failed")}

        {:error, :not_found} ->
          {:noreply, put_flash(socket, :error, "Request not found in database")}

        {:error, err} ->
          {:noreply, put_flash(socket, :error, "Failed to reject: #{inspect(err)}")}
      end
    end
  end

  defp get_user_from_session(%{"session_uuid" => session_uuid}) when session_uuid != nil do
    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        user

      _ ->
        case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
          [{_, token}] ->
            signing_salt = MazarynWeb.Endpoint.config(:live_view)[:signing_salt]

            case Phoenix.Token.verify(MazarynWeb.Endpoint, signing_salt, token, max_age: 806_400) do
              {:ok, user_id} ->
                cl = if is_binary(user_id), do: String.to_charlist(user_id), else: user_id

                case Core.UserClient.get_user_by_id(cl) do
                  t when is_tuple(t) -> t
                  _ -> nil
                end

              _ ->
                nil
            end

          [] ->
            nil
        end
    end
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    cl = if is_list(user_id), do: user_id, else: String.to_charlist(user_id)

    case Core.UserClient.get_user_by_id(cl) do
      t when is_tuple(t) -> t
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp get_username(nil), do: ""

  defp get_username(%Account.User{} = user) do
    case user.username do
      u when is_binary(u) and u != "" -> u
      _ -> ""
    end
  end

  defp get_username(user) when is_tuple(user) do
    case elem(user, 8) do
      u when is_list(u) ->
        s = List.to_string(u)
        if String.starts_with?(s, ["/ip4", "/ip6"]), do: "", else: s

      u when is_binary(u) ->
        if String.starts_with?(u, ["/ip4", "/ip6"]), do: "", else: u

      _ ->
        ""
    end
  end

  defp get_username(_), do: ""

  defp is_admin_user(username) when is_binary(username) and username != "" do
    admin_usernames = ["arvand", "mazaryn", "zaryn"]
    Enum.member?(admin_usernames, username |> String.trim() |> String.downcase())
  end

  defp is_admin_user(_), do: false

  defp load_requests(socket) do
    requests =
      case socket.assigns.filter do
        "pending" ->
          case :musicdb.get_pending_artist_requests() do
            {:ok, erlang_requests} ->
              Enum.map(erlang_requests, &format_request/1)

            _error ->
              []
          end

        _ ->
          []
      end

    socket |> assign(:requests, requests) |> assign(:loading, false)
  end

  defp format_request(request) when is_tuple(request) do
    id = elem(request, 1)
    user_id = elem(request, 2)
    username = elem(request, 3)
    status = elem(request, 4)
    requested_at = elem(request, 5)
    reviewed_at = elem(request, 6)
    reviewed_by = elem(request, 7)
    request_data = elem(request, 8)
    rejection_reason = if tuple_size(request) > 8, do: elem(request, 9), else: nil

    {artist_name, bio, experience, proof_links, genres} = extract_request_data_safe(request_data)

    %{
      id: normalize_str(id),
      user_id: normalize_str(user_id),
      username: normalize_str(username),
      artist_name: artist_name,
      bio: bio,
      experience: experience,
      proof_links: proof_links,
      genres: genres,
      status: status,
      requested_at: format_datetime(requested_at),
      reviewed_at: format_datetime(reviewed_at),
      reviewed_by: normalize_str(reviewed_by),
      rejection_reason: normalize_str(rejection_reason)
    }
  end

  defp format_request(_other) do
    %{
      id: "",
      user_id: "",
      username: "",
      artist_name: "",
      bio: "",
      experience: "",
      proof_links: "",
      genres: "",
      status: :unknown,
      requested_at: nil,
      reviewed_at: nil,
      reviewed_by: "",
      rejection_reason: ""
    }
  end

  defp extract_request_data_safe(data) do
    elixir_map =
      cond do
        is_map(data) ->
          try do
            for {key, value} <- data, into: %{} do
              string_key =
                cond do
                  is_atom(key) -> Atom.to_string(key)
                  is_binary(key) -> key
                  is_list(key) -> List.to_string(key)
                  true -> inspect(key)
                end
              {string_key, value}
            end
          rescue
            _ -> data
          end
        true -> %{}
      end

    artist_name = get_field_value(elixir_map, ["artist_name", "artistName", :artist_name])
    bio = get_field_value(elixir_map, ["bio", :bio])
    experience = get_field_value(elixir_map, ["experience", :experience])
    proof_links = get_field_value(elixir_map, ["proof_links", "proofLinks", :proof_links])
    genres = get_field_value(elixir_map, ["genres", :genres])

    {artist_name, bio, experience, proof_links, genres}
  end

  defp get_field_value(map, possible_keys) when is_map(map) do
    Enum.find_value(possible_keys, "", fn key ->
      case Map.get(map, key) do
        nil -> nil
        value -> normalize_str(value)
      end
    end)
  end

  defp get_field_value(_, _), do: ""

  defp normalize_str(nil), do: ""
  defp normalize_str(:undefined), do: ""
  defp normalize_str(v) when is_list(v), do: List.to_string(v)
  defp normalize_str(v) when is_binary(v), do: v
  defp normalize_str(v) when is_atom(v), do: Atom.to_string(v)
  defp normalize_str(v), do: inspect(v)

  defp to_erlang_chlist(v) when is_list(v), do: v
  defp to_erlang_chlist(v) when is_binary(v), do: String.to_charlist(v)
  defp to_erlang_chlist(v), do: String.to_charlist("#{v}")

  defp format_datetime(:undefined), do: nil
  defp format_datetime(nil), do: nil

  defp format_datetime({{y, mo, d}, {h, mi, _}}) do
    "#{y}-#{pad(mo)}-#{pad(d)} #{pad(h)}:#{pad(mi)}"
  end

  defp format_datetime(_), do: nil

  defp pad(n), do: String.pad_leading(Integer.to_string(n), 2, "0")
end
