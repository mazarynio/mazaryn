defmodule MazarynWeb.DownloadController do
  use MazarynWeb, :controller
  require Logger

  def download_file(conn, %{"id" => download_id}) do
    Logger.info("=== DOWNLOAD FILE REQUEST ===")
    Logger.info("Download ID: #{download_id}")
    Logger.info("User agent: #{get_req_header(conn, "user-agent")}")

    rust_api_url = "http://localhost:2020/downloads/#{download_id}/file"
    Logger.info("Fetching from Rust API: #{rust_api_url}")

    case HTTPoison.get(rust_api_url, [], recv_timeout: 120_000, timeout: 120_000, stream_to: self(), async: :once) do
      {:ok, %HTTPoison.AsyncResponse{id: ref}} ->
        Logger.info("Started async download from Rust API")
        stream_file_response(conn, ref, download_id)

      {:error, %HTTPoison.Error{reason: reason}} ->
        Logger.error("HTTP request failed: #{inspect(reason)}")

        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Failed to connect to download service: #{inspect(reason)}"})
    end
  end

  defp stream_file_response(conn, ref, download_id) do
    receive do
      %HTTPoison.AsyncStatus{id: ^ref, code: 200} ->
        Logger.info("Received 200 status, waiting for headers...")
        HTTPoison.stream_next(%HTTPoison.AsyncResponse{id: ref})
        stream_file_response(conn, ref, download_id)

      %HTTPoison.AsyncStatus{id: ^ref, code: 404} ->
        Logger.error("Download not found (404)")

        conn
        |> put_status(:not_found)
        |> json(%{error: "Download not found"})

      %HTTPoison.AsyncStatus{id: ^ref, code: 400} ->
        Logger.error("Download not ready (400)")

        conn
        |> put_status(:bad_request)
        |> json(%{error: "Download not completed yet"})

      %HTTPoison.AsyncStatus{id: ^ref, code: status_code} ->
        Logger.error("Unexpected status: #{status_code}")

        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Unexpected status: #{status_code}"})

      %HTTPoison.AsyncHeaders{id: ^ref, headers: headers} ->
        Logger.info("Received headers: #{inspect(headers)}")

        content_disposition = Enum.find_value(headers, fn
          {"Content-Disposition", value} -> value
          {"content-disposition", value} -> value
          _ -> nil
        end)

        filename = extract_filename_from_disposition(content_disposition) || "dataset_#{download_id}.zip"
        Logger.info("Filename: #{filename}")

        content_type = Enum.find_value(headers, fn
          {"Content-Type", value} -> value
          {"content-type", value} -> value
          _ -> nil
        end) || "application/zip"

        content_length = Enum.find_value(headers, fn
          {"Content-Length", value} -> value
          {"content-length", value} -> value
          _ -> nil
        end)

        Logger.info("Content-Type: #{content_type}, Content-Length: #{content_length}")

        conn = conn
        |> put_resp_content_type(content_type)
        |> put_resp_header("content-disposition", "attachment; filename=\"#{filename}\"")
        |> put_resp_header("cache-control", "no-cache, no-store, must-revalidate")
        |> put_resp_header("pragma", "no-cache")
        |> put_resp_header("expires", "0")

        conn = if content_length do
          put_resp_header(conn, "content-length", content_length)
        else
          conn
        end

        conn = send_chunked(conn, 200)
        HTTPoison.stream_next(%HTTPoison.AsyncResponse{id: ref})
        stream_file_chunks(conn, ref, 0)

      %HTTPoison.AsyncEnd{id: ^ref} ->
        Logger.warn("Received AsyncEnd before headers")

        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Download stream ended prematurely"})
    after
      30_000 ->
        Logger.error("Timeout waiting for response")

        conn
        |> put_status(:gateway_timeout)
        |> json(%{error: "Download request timed out"})
    end
  end

  defp stream_file_chunks(conn, ref, bytes_sent) do
    receive do
      %HTTPoison.AsyncChunk{id: ^ref, chunk: chunk} ->
        case chunk(conn, chunk) do
          {:ok, conn} ->
            new_bytes = bytes_sent + byte_size(chunk)
            if rem(new_bytes, 1_048_576) == 0 do
              Logger.info("Streamed #{div(new_bytes, 1_048_576)} MB...")
            end
            HTTPoison.stream_next(%HTTPoison.AsyncResponse{id: ref})
            stream_file_chunks(conn, ref, new_bytes)

          {:error, reason} ->
            Logger.error("Failed to send chunk: #{inspect(reason)}")
            conn
        end

      %HTTPoison.AsyncEnd{id: ^ref} ->
        Logger.info("Download complete! Total bytes: #{bytes_sent}")
        conn
    after
      30_000 ->
        Logger.error("Timeout while streaming chunks")
        conn
    end
  end

  defp extract_filename_from_disposition(nil), do: nil
  defp extract_filename_from_disposition(disposition) do
    case Regex.run(~r/filename[*]?=["']?([^"';]+)["']?/i, disposition) do
      [_, filename] ->
        filename
        |> String.trim()
        |> URI.decode()
      _ -> nil
    end
  end
end
