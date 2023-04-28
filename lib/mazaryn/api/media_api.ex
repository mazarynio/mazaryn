defmodule Mazaryn.MediaAPI do
  @moduledoc """
  Public Media API
  """

  require Logger
  alias Core.MediaClient
  alias Mazaryn.Schema.Media

  @spec(upload_file(%Ecto.Changeset{}) :: %Media{}, {:error, :string})
  def upload_file(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def upload_file(%Ecto.Changeset{changes: %{user_id: user_id, file: file}} = changeset) do
    media = Ecto.Changeset.get_field(changeset, :media, [])

    case create(user_id, file) do
      {:ok, media_id} ->
        one_by_id(media_id)

      {:error, some_error} ->
        {:error, some_error}
    end
  end

  @doc """
  Currently returns the post_id
  """
  def create(user_id, file, _other \\ []) do
    {:ok, MediaClient.insert_media(user_id, file)}
  end

  def one_by_id(media_id) do
    case Core.MediaClient.get_media(media_id) do
      erl_media ->
        erl_media
        |> Media.erl_changeset()
        |> Media.build()
    end
  end
end
