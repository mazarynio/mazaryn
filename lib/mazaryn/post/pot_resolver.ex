defmodule Post.PostResolver do
  alias Core.PostClient, as: PostClient

  def all(_args, _info) do
    {:ok, PostClient.get_posts()}
  end
end
