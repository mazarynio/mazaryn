defmodule Mazaryn.Comments do
  @moduledoc """
  Public Mazaryn.Comments API
  """

  require Logger

  alias Mazaryn.Schema.Comment

  def create(author, post_id, content) do
    case Core.PostClient.add_comment(author, post_id, content) do
      comment_id when is_binary(comment_id) ->
        {:ok, comment_id}

      something_else ->
        Logger.warning(something_else)
        {:error, something_else}
    end
  end

  def one_by_id(id) do
    case Core.PostClient.get_single_comment(id) do
      erl_post ->
        erl_post
        |> Comment.erl_changeset()
        |> Comment.build()
    end
  end
end
