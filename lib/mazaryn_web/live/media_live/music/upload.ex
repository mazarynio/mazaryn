defmodule MazarynWeb.MediaLive.Music.Upload do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    {user_id, username, user_tuple} = get_user_from_session(session)

    charlist_user_id =
      cond do
        is_list(user_id) -> user_id
        is_binary(user_id) -> String.to_charlist(user_id)
        true -> nil
      end

    is_artist = check_artist_status(charlist_user_id)

    {:ok,
     socket
     |> assign(:page_title, "Upload Music")
     |> assign(:user_id, user_id)
     |> assign(:username, username)
     |> assign(:charlist_user_id, charlist_user_id)
     |> assign(:user_tuple, user_tuple)
     |> assign(:current_user_id, user_id)
     |> assign(:current_username, username)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:create_playlist, false)
     |> assign(:privacy, "public")
     |> assign(:is_artist, is_artist)
     |> assign(:artist_status, get_artist_request_status(charlist_user_id))
     |> assign(:show_request_form, false)
     |> assign(:artist_name, "")
     |> assign(:bio, "")
     |> assign(:experience, "")
     |> assign(:proof_links, "")
     |> assign(:genres, "")
     |> allow_upload(:music_files,
       accept: ~w(audio/flac audio/wav audio/x-m4a audio/ogg audio/mpeg audio/x-aiff),
       max_entries: 10,
       max_file_size: 500_000_000
     )}
  end

  @impl true
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("search", %{"query" => query}, socket),
    do: {:noreply, assign(socket, :search_query, query)}

  @impl true
  def handle_event("validate", _params, socket), do: {:noreply, socket}

  @impl true
  def handle_event("cancel_upload", %{"ref" => ref}, socket),
    do: {:noreply, cancel_upload(socket, :music_files, ref)}

  @impl true
  def handle_event("set_privacy", %{"value" => privacy}, socket),
    do: {:noreply, assign(socket, :privacy, privacy)}

  @impl true
  def handle_event("show_request_form", _params, socket),
    do: {:noreply, assign(socket, :show_request_form, true)}

  @impl true
  def handle_event("hide_request_form", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_request_form, false)
     |> assign(:artist_name, "")
     |> assign(:bio, "")
     |> assign(:experience, "")
     |> assign(:proof_links, "")
     |> assign(:genres, "")}
  end

  @impl true
  def handle_event("update_field", params, socket) do
    artist_name = Map.get(params, "artist_name", socket.assigns.artist_name)
    bio = Map.get(params, "bio", socket.assigns.bio)
    experience = Map.get(params, "experience", socket.assigns.experience)
    proof_links = Map.get(params, "proof_links", socket.assigns.proof_links)
    genres = Map.get(params, "genres", socket.assigns.genres)

    {:noreply,
     socket
     |> assign(:artist_name, artist_name)
     |> assign(:bio, bio)
     |> assign(:experience, experience)
     |> assign(:proof_links, proof_links)
     |> assign(:genres, genres)}
  end

  @impl true
  def handle_event("validate_artist_request", %{"artist" => params}, socket) do
    socket =
      socket
      |> assign(:artist_name, Map.get(params, "name", ""))
      |> assign(:bio, Map.get(params, "bio", ""))
      |> assign(:experience, Map.get(params, "experience", ""))
      |> assign(:proof_links, Map.get(params, "proof_links", ""))
      |> assign(:genres, Map.get(params, "genres", ""))

    {:noreply, socket}
  end

  @impl true
  def handle_event("submit_artist_request", params, socket) do
    artist_name = Map.get(params, "artist_name", "")
    bio = Map.get(params, "bio", "")
    experience = Map.get(params, "experience", "")
    proof_links = Map.get(params, "proof_links", "")
    genres = Map.get(params, "genres", "")

    charlist_user_id = socket.assigns.charlist_user_id
    username = socket.assigns.username

    if charlist_user_id && username do
      case Core.UserClient.get_user_by_id(charlist_user_id) do
        user_tuple when is_tuple(user_tuple) ->
          request_data = %{
            artist_name: artist_name,
            bio: bio,
            experience: experience,
            proof_links: proof_links,
            genres: genres
          }

          case :musicdb.create_artist_request(charlist_user_id, username, request_data) do
            {:ok, _request_id} ->
              {:noreply, redirect(socket, to: "/#{socket.assigns.locale}/music/artist-request-success")}

            {:error, :already_exists} ->
              {:noreply, put_flash(socket, :error, "You already have a pending request.")}

            {:error, reason} ->
              {:noreply, put_flash(socket, :error, "Failed to submit request: #{inspect(reason)}")}
          end

        _ ->
          {:noreply, put_flash(socket, :error, "User verification failed")}
      end
    else
      {:noreply, redirect(socket, to: "/#{socket.assigns.locale}/login")}
    end
  end

  @impl true
  def handle_event("upload_files", _params, socket) do
    if not socket.assigns.is_artist do
      {:noreply, put_flash(socket, :error, "Only approved artists can upload music.")}
    else
      charlist_user_id = socket.assigns.charlist_user_id
      username = socket.assigns.username

      uploaded_files =
        consume_uploaded_entries(socket, :music_files, fn %{path: path}, entry ->
          file_name = entry.client_name

          case File.read(path) do
            {:ok, _binary} ->
              privacy = String.to_atom(socket.assigns.privacy)

              case :musicdb.create_music_with_rust(
                     charlist_user_id,
                     file_name,
                     username,
                     "Unknown Album",
                     path,
                     0,
                     [],
                     privacy,
                     true,
                     false,
                     false
                   ) do
                music_id when is_binary(music_id) or is_list(music_id) ->
                  {:ok, %{id: music_id, name: file_name, size: entry.client_size}}

                {:error, reason} ->
                  Logger.error("Upload failed for #{file_name}: #{inspect(reason)}")
                  {:postpone, :error}
              end

            {:error, reason} ->
              Logger.error("Failed to read #{file_name}: #{inspect(reason)}")
              {:postpone, :error}
          end
        end)

      ok_count = Enum.count(uploaded_files, &match?({:ok, _}, &1))
      fail_count = Enum.count(uploaded_files, &(&1 == :error))

      msg =
        if fail_count > 0,
          do: "Uploaded #{ok_count} file(s), #{fail_count} failed.",
          else: "Successfully uploaded #{ok_count} file(s)!"

      {:noreply, put_flash(socket, :info, msg)}
    end
  end

  defp get_user_from_session(%{"session_uuid" => session_uuid}) when session_uuid != nil do
    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, %{id: id, username: username}} ->
        {normalize_id(id), normalize_username(username), nil}

      {:ok, t} when is_tuple(t) ->
        {normalize_id(elem(t, 1)), normalize_username(elem(t, 8)), t}

      _ ->
        get_user_from_ets(session_uuid)
    end
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    id_str = normalize_id(user_id)

    case Core.UserClient.get_user_by_id(id_str) do
      t when is_tuple(t) -> {id_str, normalize_username(elem(t, 8)), t}
      _ -> {nil, nil, nil}
    end
  end

  defp get_user_from_session(_), do: {nil, nil, nil}

  defp get_user_from_ets(session_uuid) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        salt = MazarynWeb.Endpoint.config(:live_view)[:signing_salt]

        case Phoenix.Token.verify(MazarynWeb.Endpoint, salt, token, max_age: 806_400) do
          {:ok, user_id} ->
            id_str = normalize_id(user_id)

            case Core.UserClient.get_user_by_id(id_str) do
              t when is_tuple(t) -> {id_str, normalize_username(elem(t, 8)), t}
              _ -> {nil, nil, nil}
            end

          _ ->
            {nil, nil, nil}
        end

      [] ->
        {nil, nil, nil}
    end
  end

  defp check_artist_status(nil), do: false

  defp check_artist_status(user_id) when is_list(user_id) do
    match?({:ok, _}, :musicdb.get_artist_by_user_id(user_id))
  end

  defp check_artist_status(_), do: false

  defp get_artist_request_status(nil), do: nil

  defp get_artist_request_status(user_id) when is_list(user_id) do
    case :musicdb.get_artist_request_status(user_id) do
      {:ok, status} -> Atom.to_string(status)
      _ -> nil
    end
  end

  defp get_artist_request_status(_), do: nil

  defp normalize_id(v) when is_list(v), do: List.to_string(v)
  defp normalize_id(v) when is_binary(v), do: v
  defp normalize_id(v) when is_integer(v), do: Integer.to_string(v)
  defp normalize_id(v) when is_atom(v), do: Atom.to_string(v)
  defp normalize_id(_), do: nil

  defp normalize_username(v) when is_list(v), do: List.to_string(v)
  defp normalize_username(v) when is_binary(v), do: v
  defp normalize_username(v) when is_atom(v), do: Atom.to_string(v)
  defp normalize_username(_), do: "Unknown"

  def format_file_size(b) when b < 1024, do: "#{b} B"
  def format_file_size(b) when b < 1_048_576, do: "#{Float.round(b / 1024, 1)} KB"
  def format_file_size(b) when b < 1_073_741_824, do: "#{Float.round(b / 1_048_576, 1)} MB"
  def format_file_size(b), do: "#{Float.round(b / 1_073_741_824, 1)} GB"

  def error_to_string(:too_large), do: "File is too large (max 500MB)"
  def error_to_string(:not_accepted), do: "Invalid file type"
  def error_to_string(:too_many_files), do: "Too many files (max 10)"
  def error_to_string(_), do: "Unknown error"
end
