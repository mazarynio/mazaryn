defmodule Post do
  @moduledoc """
  Post Struct
  """

  defstruct id: nil,
            content: nil,
            media: nil,
            comments: [],
            author: nil,
            date_created: nil,
            photo_url: nil

  def new({:post, id, content, comments, author, photo_url, date_created}) do
    struct(Post, %{
      id: id,
      content: content,
      comments: comments,
      author: author,
      date_created: date_created,
      photo_url: photo_url
    })
  end
end
