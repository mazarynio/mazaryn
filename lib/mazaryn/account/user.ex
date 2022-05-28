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

  def new({:user, username, password, email, following, follower, saved_posts, other_info, date_created, date_updated}) do
    struct(Account.User, %{username: username, password: password, email: email, follower: follower, following: following, saved_posts: saved_posts, other_info: other_info, date_created: date_created, date_updated: date_updated})
  end
end
