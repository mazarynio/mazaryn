defmodule MazarynWeb.DatasetDownloadController do
  use MazarynWeb, :controller
  require Logger

  def download(conn, %{"dataset_id" => dataset_id} = params) do
    user_id = Map.get(params, "user_id", "")

    Logger.info("Dataset download request: dataset_id=#{dataset_id}, user_id=#{user_id}")

    dataset_id_charlist = String.to_charlist(dataset_id)
    user_id_charlist = String.to_charlist(user_id)

    case :datasetdb.get_dataset_zip_by_id(dataset_id_charlist, user_id_charlist) do
      {:ok, binary} when is_binary(binary) ->
        Logger.info("Successfully retrieved dataset binary, size: #{byte_size(binary)} bytes")

        filename = "dataset_#{dataset_id}.zip"

        conn
        |> put_resp_content_type("application/zip")
        |> put_resp_header("content-disposition", "attachment; filename=\"#{filename}\"")
        |> send_resp(200, binary)

      {:error, :access_denied} ->
        Logger.warn("Access denied for user #{user_id} to dataset #{dataset_id}")

        conn
        |> put_status(:forbidden)
        |> json(%{error: "Access denied"})

      {:error, :dataset_not_found} ->
        Logger.warn("Dataset not found: #{dataset_id}")

        conn
        |> put_status(:not_found)
        |> json(%{error: "Dataset not found"})

      {:error, :not_a_zip_file} ->
        Logger.error("Dataset #{dataset_id} is not a ZIP file")

        conn
        |> put_status(:unprocessable_entity)
        |> json(%{error: "Dataset is not a ZIP file"})

      {:error, reason} ->
        Logger.error("Failed to retrieve dataset: #{inspect(reason)}")

        conn
        |> put_status(:internal_server_error)
        |> json(%{error: "Failed to retrieve dataset"})
    end
  end
end
