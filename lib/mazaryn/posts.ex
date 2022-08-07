defmodule Mazaryn.Posts do
  @moduledoc """
  Public Mazaryn.Posts API
  """

  require Logger

  alias Mazaryn.Schema.Post

  def create(author, content, media) do
    case Core.PostClient.create(author, content, media) do
      post_id when is_binary(post_id) ->
        {:ok, post_id}

      something_else ->
        Logger.warn(something_else)
        {:error, something_else}
    end
  end

  def one_by_id(id) do
    case Core.PostClient.get_by_id(id) do
      erl_post ->
        erl_post
        |> Post.erl_changeset()
        |> Post.build()
    end
  end
end
