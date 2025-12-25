defmodule MazarynWeb.MediaLive.Livestream.VodWatch do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(%{"id" => stream_id}, session, socket) do
    user = get_user_from_session(session)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(Mazaryn.PubSub, "livestreams:global")
    end

    case :livestreamdb.get_livestream(stream_id) do
      {:ok, stream_tuple} when is_tuple(stream_tuple) ->
        formatted = format_stream(stream_tuple)
        vod_url =
          if formatted.vod_cid && formatted.vod_cid != "" do
            "http://localhost:8080/ipfs/#{formatted.vod_cid}"
          else
            nil
          end

        {:ok,
         socket
         |> assign(:page_title, formatted.title || "Stream Recording")
         |> assign(:locale, socket.assigns[:locale] || "en")
         |> assign(:user, user)
         |> assign(:stream, formatted)
         |> assign(:vod_url, vod_url)}

      _ ->
        {:ok,
         socket
         |> put_flash(:error, "Stream not found")
         |> push_navigate(to: ~p"/#{socket.assigns[:locale] || "en"}/livestreams/my-streams")}
    end
  end

  defp get_user_from_session(%{"session_uuid" => session_uuid}) when session_uuid != nil do
    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, user} -> user
      _ -> nil
    end
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    charlist_id = if is_list(user_id), do: user_id, else: String.to_charlist(user_id)

    case Core.UserClient.get_user_by_id(charlist_id) do
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp format_stream(stream_tuple) when is_tuple(stream_tuple) do
    stream_id = safe_elem(stream_tuple, 1) |> to_string()

    vod_cid =
      case :livestreamdb.get_recording_cid(stream_id) do
        {:ok, cid} when is_list(cid) and length(cid) > 0 -> List.to_string(cid)
        {:ok, cid} when is_binary(cid) and byte_size(cid) > 10 -> String.trim(cid)
        _ -> nil
      end

    title = safe_elem(stream_tuple, 3) |> to_string()
    description = safe_elem(stream_tuple, 4) |> to_string()
    thumbnail_cid = safe_elem(stream_tuple, 5)
    category = safe_elem(stream_tuple, 10) |> format_category()
    total_views = safe_elem(stream_tuple, 23) || 0
    started_at = safe_elem(stream_tuple, 47)

    %{
      id: stream_id,
      title: if(title == "" or title == " ", do: "Untitled Stream", else: title),
      description: description,
      thumbnail_url: get_thumbnail_url(thumbnail_cid),
      category: category,
      total_views: total_views,
      started_at: started_at,
      vod_cid: vod_cid
    }
  end

  defp get_thumbnail_url(cid) when is_binary(cid) and cid != "", do: "https://ipfs.io/ipfs/#{cid}"
  defp get_thumbnail_url(cid) when is_list(cid) and length(cid) > 0, do: "https://ipfs.io/ipfs/#{List.to_string(cid)}"
  defp get_thumbnail_url(_), do: "/images/default-stream-thumbnail.svg"

  defp format_category(cat) when is_atom(cat), do: Atom.to_string(cat)
  defp format_category(cat) when is_binary(cat), do: cat
  defp format_category(cat) when is_list(cat), do: List.to_string(cat)
  defp format_category(_), do: "other"

  defp format_date(dt) when is_tuple(dt) do
    try do
      {{y, m, d}, {h, min, _}} = dt
      {:ok, naive} = NaiveDateTime.new(y, m, d, h, min, 0)
      Calendar.strftime(naive, "%B %d, %Y at %I:%M %p")
    rescue
      _ -> "Unknown date"
    end
  end

  defp format_date(_), do: "Unknown date"

  defp format_viewers(num) when is_integer(num) and num >= 1_000_000, do: "#{Float.round(num / 1_000_000, 1)}M"
  defp format_viewers(num) when is_integer(num) and num >= 1_000, do: "#{Float.round(num / 1_000, 1)}K"
  defp format_viewers(num) when is_integer(num), do: "#{num}"
  defp format_viewers(_), do: "0"

  defp safe_elem(tuple, index) when is_tuple(tuple) and index < tuple_size(tuple), do: elem(tuple, index)
  defp safe_elem(_, _), do: nil
end
