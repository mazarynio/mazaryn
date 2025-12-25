defmodule MazarynWeb.LivestreamController do
  use MazarynWeb, :controller
  require Logger

  def save_recording(conn, %{"id" => stream_id} = params) do
    Logger.info("📥 [CONTROLLER] save_recording called")
    Logger.info("📥 Stream ID: #{inspect(stream_id)}")
    Logger.info("📥 Full params: #{inspect(params, limit: :infinity)}")
    Logger.info("📥 Params keys: #{inspect(Map.keys(params))}")

    user_id = get_user_id_from_session(conn)
    Logger.info("👤 User ID from session: #{inspect(user_id)}")

    if is_nil(user_id) do
      Logger.error("❌ Unauthorized - no user ID in session")

      conn
      |> put_status(:unauthorized)
      |> json(%{error: "Unauthorized"})
    else
      case params do
        %{"recording_file" => upload} ->
          Logger.info("✅ Recording file found in params")
          Logger.info("📁 Upload details: #{inspect(upload, limit: :infinity)}")
          Logger.info("📁 Upload path: #{inspect(upload.path)}")
          Logger.info("📁 Upload filename: #{inspect(upload.filename)}")
          Logger.info("📁 Upload content_type: #{inspect(upload.content_type)}")
          Logger.info("📁 Upload size: #{inspect(File.stat!(upload.path).size)} bytes")
          handle_file_upload(conn, stream_id, user_id, upload)

        _ ->
          Logger.error("❌ No recording file in params")
          Logger.error("❌ Available params keys: #{inspect(Map.keys(params))}")
          Logger.error("❌ Full params: #{inspect(params)}")

          conn
          |> put_status(:bad_request)
          |> json(%{error: "No recording file provided"})
      end
    end
  end

  defp handle_file_upload(conn, stream_id, user_id, upload) do
    Logger.info("📤 [UPLOAD] Starting file upload handler")
    Logger.info("📤 Stream ID: #{stream_id}")
    Logger.info("📤 User ID: #{user_id}")

    temp_dir = System.tmp_dir!()
    filename = "livestream_#{stream_id}_#{:os.system_time(:second)}.webm"
    temp_path = Path.join(temp_dir, filename)

    Logger.info("📂 Temp directory: #{temp_dir}")
    Logger.info("📂 Target path: #{temp_path}")

    case File.cp(upload.path, temp_path) do
      :ok ->
        Logger.info("✅ File copied successfully to: #{temp_path}")
        file_stat = File.stat!(temp_path)
        Logger.info("📊 File size after copy: #{file_stat.size} bytes")

        charlist_stream_id = String.to_charlist(stream_id)
        charlist_user_id = String.to_charlist(user_id)
        charlist_path = String.to_charlist(temp_path)

        Logger.info("🔄 Calling :livestreamdb.save_recording_from_file")
        Logger.info("🔄 Args: stream_id=#{stream_id}, user_id=#{user_id}, path=#{temp_path}")

        case :livestreamdb.save_recording_from_file(
               charlist_stream_id,
               charlist_user_id,
               charlist_path
             ) do
          {:ok, :processing} ->
            Logger.info("✅ Recording upload initiated successfully for stream #{stream_id}")

            conn
            |> put_status(:ok)
            |> json(%{
              success: true,
              message: "Recording is being uploaded to IPFS",
              stream_id: stream_id,
              file_size: file_stat.size
            })

          {:error, reason} ->
            Logger.error("❌ Failed to save recording: #{inspect(reason)}")

            conn
            |> put_status(:internal_server_error)
            |> json(%{error: "Failed to save recording: #{inspect(reason)}"})

          other ->
            Logger.error("❌ Unexpected response from Erlang: #{inspect(other)}")

            conn
            |> put_status(:internal_server_error)
            |> json(%{error: "Unexpected response: #{inspect(other)}"})
        end

      {:error, reason} ->
        Logger.error("❌ Failed to copy file: #{inspect(reason)}")
        Logger.error("❌ Source: #{upload.path}")
        Logger.error("❌ Target: #{temp_path}")

        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Failed to save recording file"})
    end
  end

  defp get_user_id_from_session(conn) do
    session_uuid = get_session(conn, :session_uuid)
    Logger.info("🔍 Session UUID: #{inspect(session_uuid)}")

    if session_uuid do
      case Account.Users.get_by_session_uuid(session_uuid) do
        {:ok, %{id: id}} ->
          user_id = to_string(id)
          Logger.info("✅ Found user ID: #{user_id}")
          user_id

        error ->
          Logger.error("❌ Failed to get user by session: #{inspect(error)}")
          nil
      end
    else
      Logger.warn("⚠️ No session UUID in conn")
      nil
    end
  end

  def hls(conn, %{"id" => _stream_id}) do
    conn
    |> put_status(:not_implemented)
    |> json(%{error: "HLS endpoint not yet implemented"})
  end

  def rtmp_info(conn, %{"id" => _stream_id}) do
    conn
    |> put_status(:not_implemented)
    |> json(%{error: "RTMP info endpoint not yet implemented"})
  end
end
