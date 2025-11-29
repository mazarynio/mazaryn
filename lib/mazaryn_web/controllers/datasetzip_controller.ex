defmodule MazarynWeb.DatasetZipController do
  use MazarynWeb, :controller

  require Logger

  alias Account.Users

  def serve(conn, %{"dataset_id" => dataset_id}) do
    dataset_id_cl = String.to_charlist(dataset_id)

    with {:ok, _user} <- get_current_user(conn),
         {:ok, dataset} <- fetch_dataset(dataset_id_cl),
         {:ok, zip_binary} <- get_zip_content(dataset_id_cl) do

      filename = derive_zip_filename(dataset)
      conn
      |> put_resp_content_type("application/zip")
      |> put_resp_header("content-disposition", "attachment; filename=\"#{filename}\"")
      |> send_resp(200, zip_binary)
    else
      {:error, :unauthorized} ->
        conn
        |> put_status(401)
        |> json(%{error: "Authentication required"})

      {:error, :dataset_not_found} ->
        conn
        |> put_status(404)
        |> json(%{error: "Dataset not found"})

      {:error, reason} ->
        Logger.error("ZIP fetch error for #{dataset_id}: #{inspect(reason)}")
        conn
        |> put_status(404)
        |> json(%{error: "Dataset ZIP not available"})

      other ->
        Logger.error("Unexpected error in DatasetZipController: #{inspect(other)}")
        conn
        |> put_status(500)
        |> json(%{error: "Internal server error"})
    end
  end

  defp get_current_user(conn) do
    case get_session(conn, :session_uuid) do
      nil -> {:error, :unauthorized}
      session_uuid -> Users.get_by_session_uuid(session_uuid)
    end
  end

  defp fetch_dataset(dataset_id_cl) when is_list(dataset_id_cl) do
    case :datasetdb.get_dataset_by_id(dataset_id_cl) do
      {:error, _} -> {:error, :dataset_not_found}
      dataset when is_tuple(dataset) and elem(dataset, 0) == :dataset -> {:ok, dataset}
      _ -> {:error, :dataset_not_found}
    end
  rescue
    _ -> {:error, :dataset_not_found}
  end

  defp get_zip_content(dataset_id_cl) do
    case :datasetdb.get_dataset_content(dataset_id_cl) do
      content when is_binary(content) -> {:ok, content}
      content when is_list(content) -> {:ok, :erlang.list_to_binary(content)}
      {:error, _} = err -> err
      other -> {:error, {:unexpected_content, other}}
    end
  end

  defp derive_zip_filename(dataset) when is_tuple(dataset) and elem(dataset, 0) == :dataset do
    metadata = elem(dataset, 20)

    filename = case metadata do
      %{filename: fname} when is_list(fname) -> to_string(fname)
      %{filename: fname} when is_binary(fname) -> to_string(fname)
      %{"filename" => fname} when is_list(fname) -> to_string(fname)
      %{"filename" => fname} when is_binary(fname) -> to_string(fname)
      _ -> nil
    end

    if filename && filename != "" do
      # Remove trailing .zip if already present
      clean_name = if String.ends_with?(String.downcase(filename), ".zip") do
        String.slice(filename, 0, String.length(filename) - 4)
      else
        filename
      end
      sanitize_filename(clean_name) <> ".zip"
    else
      title = elem(dataset, 2)
      title_str = case title do
        t when is_list(t) -> to_string(t)
        t when is_binary(t) -> to_string(t)
        _ -> "dataset"
      end
      sanitize_filename(title_str) <> ".zip"
    end
  end

  defp sanitize_filename(filename) do
    filename
    |> String.replace(~r/[^a-zA-Z0-9._\-]/, "_")
    |> String.slice(0, 100)
    |> String.trim()
    |> case do
      "" -> "dataset"
      name -> name
    end
  end
end
