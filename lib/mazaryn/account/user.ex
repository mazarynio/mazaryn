defmodule Account.User do
  @moduledoc """
  User Struct
  """

  @derive {Inspect, only: [:name]}
  defstruct username: nil,
    email: nil,
    following: [],
    follower: [],
    saved_posts: [],
    other_info: [],
    date_created: nil,
    date_updated: nil
end
