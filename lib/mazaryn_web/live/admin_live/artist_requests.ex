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
    Logger.info("🎨 [select_request] id=#{inspect(request_id)}")

    request =
      Enum.find(socket.assigns.requests, fn r ->
        to_string(r.id) == to_string(request_id)
      end)

    Logger.info("🎨 [select_request] resolved=#{inspect(request)}")

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
    Logger.info("🎨 [approve_request] raw id=#{inspect(request_id)}")
    username = socket.assigns.username

    erlang_request_id = to_erlang_chlist(request_id)
    erlang_username = to_erlang_chlist(username)

    Logger.info("🎨 [approve_request] erlang_request_id=#{inspect(erlang_request_id)}")
    Logger.info("🎨 [approve_request] erlang_username=#{inspect(erlang_username)}")

    case :musicdb.approve_artist_request(erlang_request_id, erlang_username) do
      {:ok, artist_id} ->
        Logger.info("🎨 [approve_request] ✅ success artist_id=#{inspect(artist_id)}")

        {:noreply,
         socket
         |> load_requests()
         |> assign(:selected_request, nil)
         |> put_flash(:info, "Artist request approved! Artist profile created.")}

      {:error, :unauthorized} ->
        Logger.error("🎨 [approve_request] ❌ unauthorized")
        {:noreply, put_flash(socket, :error, "Unauthorized: admin check failed")}

      {:error, :not_found} ->
        Logger.error("🎨 [approve_request] ❌ not_found")
        {:noreply, put_flash(socket, :error, "Request not found in database")}

      {:error, reason} ->
        Logger.error("🎨 [approve_request] ❌ error: #{inspect(reason)}")
        {:noreply, put_flash(socket, :error, "Failed to approve: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("update_rejection_reason", %{"rejection_reason" => reason}, socket) do
    Logger.info("🎨 [update_rejection_reason] reason=#{inspect(reason)}")
    {:noreply, assign(socket, :rejection_reason, reason)}
  end

  @impl true
  def handle_event("update_rejection_reason", params, socket) do
    reason =
      case params do
        %{"value" => value} -> value
        %{"_target" => [_, value]} -> value
        _ -> ""
      end

    Logger.info("🎨 [update_rejection_reason] fallback reason=#{inspect(reason)}")
    {:noreply, assign(socket, :rejection_reason, reason)}
  end

  @impl true
  def handle_event("reject_request", %{"id" => request_id}, socket) do
    Logger.info("🎨 [reject_request] raw id=#{inspect(request_id)}")

    username = socket.assigns.username
    reason = socket.assigns.rejection_reason

    Logger.info("🎨 [reject_request] reason=#{inspect(reason)}")

    if reason == "" do
      {:noreply, put_flash(socket, :error, "Please provide a rejection reason")}
    else
      erlang_request_id = to_erlang_chlist(request_id)
      erlang_username = to_erlang_chlist(username)
      erlang_reason = to_erlang_chlist(reason)

      Logger.info("🎨 [reject_request] erlang_request_id=#{inspect(erlang_request_id)}")
      Logger.info("🎨 [reject_request] erlang_username=#{inspect(erlang_username)}")

      case :musicdb.reject_artist_request(erlang_request_id, erlang_username, erlang_reason) do
        {:ok, :rejected} ->
          Logger.info("🎨 [reject_request] ✅ success")

          {:noreply,
           socket
           |> load_requests()
           |> assign(:selected_request, nil)
           |> assign(:rejection_reason, "")
           |> put_flash(:info, "Artist request rejected")}

        {:error, :unauthorized} ->
          Logger.error("🎨 [reject_request] ❌ unauthorized")
          {:noreply, put_flash(socket, :error, "Unauthorized: admin check failed")}

        {:error, :not_found} ->
          Logger.error("🎨 [reject_request] ❌ not_found")
          {:noreply, put_flash(socket, :error, "Request not found in database")}

        {:error, err} ->
          Logger.error("🎨 [reject_request] ❌ error: #{inspect(err)}")
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
              Logger.info("🎨 [load_requests] raw count=#{length(erlang_requests)}")

              Enum.map(erlang_requests, fn r ->
                Logger.info("🎨 [load_requests] raw record=#{inspect(r)}")
                formatted = format_request(r)
                Logger.info("🎨 [load_requests] formatted=#{inspect(formatted)}")
                formatted
              end)

            error ->
              Logger.error("🎨 [load_requests] fetch error=#{inspect(error)}")
              []
          end

        _ ->
          []
      end

    socket |> assign(:requests, requests) |> assign(:loading, false)
  end

  defp format_request(request) when is_tuple(request) do
    Logger.info("🎨 [format_request] tuple_size=#{tuple_size(request)}")
    Logger.info("🎨 [format_request] elem0=#{inspect(elem(request, 0))}")

    id = elem(request, 1)
    user_id = elem(request, 2)
    username = elem(request, 3)
    status = elem(request, 4)
    requested_at = elem(request, 5)
    reviewed_at = elem(request, 6)
    reviewed_by = elem(request, 7)
    request_data = elem(request, 8)
    rejection_reason = if tuple_size(request) > 9, do: elem(request, 9), else: nil

    Logger.info("🎨 [format_request] id=#{inspect(id)}")
    Logger.info("🎨 [format_request] username=#{inspect(username)}")
    Logger.info("🎨 [format_request] request_data=#{inspect(request_data)}")
    Logger.info("🎨 [format_request] request_data type=#{inspect(is_map(request_data))}")
    Logger.info("🎨 [format_request] rejection_reason=#{inspect(rejection_reason)}")

    {artist_name, bio, experience, proof_links, genres} = extract_request_data(request_data)

    Logger.info("🎨 [format_request] extracted artist_name=#{inspect(artist_name)}")
    Logger.info("🎨 [format_request] extracted bio=#{inspect(bio)}")

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

  defp format_request(other) do
    Logger.error("🎨 [format_request] unexpected input: #{inspect(other)}")

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

  defp extract_request_data(data) when is_map(data) do
    Logger.info("🎨 [extract_request_data] keys=#{inspect(Map.keys(data))}")
    Logger.info("🎨 [extract_request_data] full map=#{inspect(data)}")

    artist_name = extract_field(data, ["artist_name", :artist_name])
    bio = extract_field(data, ["bio", :bio])
    experience = extract_field(data, ["experience", :experience])
    proof_links = extract_field(data, ["proof_links", :proof_links])
    genres = extract_field(data, ["genres", :genres])

    Logger.info("🎨 [extract_request_data] FINAL artist_name=#{inspect(artist_name)}")
    Logger.info("🎨 [extract_request_data] FINAL bio=#{inspect(bio)}")
    Logger.info("🎨 [extract_request_data] FINAL experience=#{inspect(experience)}")
    Logger.info("🎨 [extract_request_data] FINAL proof_links=#{inspect(proof_links)}")
    Logger.info("🎨 [extract_request_data] FINAL genres=#{inspect(genres)}")

    {artist_name, bio, experience, proof_links, genres}
  end

  defp extract_request_data(other) do
    Logger.warning("🎨 [extract_request_data] not a map: #{inspect(other)}")
    {"", "", "", "", ""}
  end

  defp extract_field(map, keys) do
    Enum.reduce_while(keys, "", fn key, _acc ->
      cond do
        is_binary(key) and Map.has_key?(map, key) ->
          val = Map.get(map, key)
          Logger.info("🎨 [extract_field] found with string key '#{key}': #{inspect(val)}")
          {:halt, normalize_str(val)}

        is_atom(key) and Map.has_key?(map, key) ->
          val = Map.get(map, key)
          Logger.info("🎨 [extract_field] found with atom key #{key}: #{inspect(val)}")
          {:halt, normalize_str(val)}

        true ->
          {:cont, ""}
      end
    end) || ""
  end

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
