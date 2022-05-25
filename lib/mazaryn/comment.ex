defmodule Comment do
  @moduledoc """
  Comment Struct
  """

  defstruct username: nil,
    content: nil,
    date_created: nil

  def new({:comment, username, content, date_created}) do
    struct(Comment, %{username: username, content: content, date_created: date_created})
  end
end
