defmodule Post do
  @moduledoc """
  Post Struct
  """

  defstruct id: nil,
    content: nil,
    comments: [],
    author: nil,
    date_created: nil

  def new({:post, id, content, comments, author, date_created}) do
    struct(Post, %{id: id, content: content, comments: comments, author: author, date_created: date_created})
  end
end
