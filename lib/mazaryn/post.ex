defmodule Post do
  @moduledoc """
  Post Struct
  """

  defstruct id: nil,
    content: nil,
    comments: [],
    author: nil,
    date_created: nil
end
