defmodule MazarynWeb.VideoController do
  use MazarynWeb, :controller
  require Logger

  def stream(conn, %{"id" => video_id}) do
    Logger.info("===== VIDEO STREAM: Request for video #{video_id} =====")

    case check_local_cache(video_id) do
      {:ok, local_path} ->
        Logger.info("===== VIDEO STREAM: Serving from local: #{local_path} =====")
        serve_local_file(conn, local_path)

      :not_found ->
        Logger.info("===== VIDEO STREAM: Not in cache, checking IPFS =====")
        charlist_id = String.to_charlist(video_id)

        case :videodb.get_video_by_id(charlist_id) do
          video when is_tuple(video) ->
            Logger.info("===== VIDEO STREAM: Video found in DB =====")

            case elem(video, 5) do
              cid when is_list(cid) or is_binary(cid) ->
                cid_str = if is_list(cid), do: List.to_string(cid), else: cid
                Logger.info("===== VIDEO STREAM: Redirecting to IPFS: #{cid_str} =====")
                redirect(conn, external: "https://ipfs.io/ipfs/#{cid_str}")

              _ ->
                Logger.warning("===== VIDEO STREAM: Video still processing =====")
                conn |> put_status(404) |> json(%{error: "Video processing"})
            end

          _ ->
            Logger.error("===== VIDEO STREAM: Video not found in DB =====")
            conn |> put_status(404) |> json(%{error: "Video not found"})
        end
    end
  rescue
    error ->
      Logger.error("===== VIDEO STREAM: Exception: #{inspect(error)} =====")
      Logger.error("===== VIDEO STREAM: Stacktrace: #{inspect(__STACKTRACE__)} =====")
      conn |> put_status(500) |> json(%{error: "Internal server error"})
  end

  defp check_local_cache(video_id) do
    Logger.info("===== CHECK_LOCAL_CACHE: Checking for video #{video_id} =====")

    try do
      case :ets.whereis(:video_path_cache) do
        :undefined ->
          Logger.warning("===== CHECK_LOCAL_CACHE: ETS table does not exist =====")
          :not_found

        _ ->
          case :ets.lookup(:video_path_cache, video_id) do
            [{^video_id, local_path, timestamp}] ->
              Logger.info("===== CHECK_LOCAL_CACHE: Found in cache: #{local_path} =====")
              Logger.info("===== CHECK_LOCAL_CACHE: Timestamp: #{timestamp} =====")

              if File.exists?(local_path) do
                Logger.info("===== CHECK_LOCAL_CACHE: File exists =====")
                {:ok, local_path}
              else
                Logger.warning(
                  "===== CHECK_LOCAL_CACHE: File no longer exists, removing from cache ====="
                )

                :ets.delete(:video_path_cache, video_id)
                :not_found
              end

            [] ->
              Logger.info("===== CHECK_LOCAL_CACHE: Not found in cache =====")
              :not_found
          end
      end
    rescue
      ArgumentError ->
        Logger.warning("===== CHECK_LOCAL_CACHE: ArgumentError =====")
        :not_found
    end
  end

  defp serve_local_file(conn, file_path) do
    Logger.info("===== SERVE_LOCAL_FILE: Path: #{file_path} =====")

    content_type = get_content_type(file_path)
    Logger.info("===== SERVE_LOCAL_FILE: Content-Type: #{content_type} =====")

    conn
    |> put_resp_content_type(content_type)
    |> put_resp_header("accept-ranges", "bytes")
    |> put_resp_header("cache-control", "public, max-age=300")
    |> send_file(200, file_path)
  rescue
    error ->
      Logger.error("===== SERVE_LOCAL_FILE: Exception: #{inspect(error)} =====")
      conn |> put_status(500) |> json(%{error: "Failed to serve file"})
  end

  defp get_content_type(path) do
    case Path.extname(path) do
      ".mp4" -> "video/mp4"
      ".webm" -> "video/webm"
      ".mov" -> "video/quicktime"
      ".avi" -> "video/x-msvideo"
      ".mkv" -> "video/x-matroska"
      _ -> "video/mp4"
    end
  end
end
