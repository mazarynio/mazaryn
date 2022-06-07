defmodule Account.User do
  @moduledoc """
  User Struct
  """

  defstruct username: nil,
    email: nil,
    following: [],
    follower: [],
    blocking: [],
    saved_posts: [],
    other_info: [],
    private: nil,
    date_created: nil,
    date_updated: nil,
    password: nil

  def new({:user, username, password, email, following, follower, blocking, saved_posts, other_info, private, date_created, date_updated}) do
    struct(Account.User, %{username: username, password: password, email: email, follower: follower, blocking: blocking, following: following, saved_posts: saved_posts, other_info: other_info, date_created: date_created, date_updated: date_updated, private: private})
  end
end
