defmodule Mazaryn.Search do
  alias Core.UserClient

  def user_search(username) do
    UserClient.search_user(username)
  end
end
