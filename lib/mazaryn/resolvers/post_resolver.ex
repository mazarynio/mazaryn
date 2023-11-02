defmodule Resolvers.PostResolver do
  alias Mazaryn.Posts

  def all(_args, _info) do
    {:ok, Posts.get_posts()}
  end

  def create(author, content, media \\ [], hashtag, mention, link_url) do
    {:ok, Posts.create_a_post(author, content, media, hashtag, mention, link_url)}
  end

  def find(%{id: id}) do
    case Posts.one_by_id(id) do
      post -> {:ok, post}
      _ -> {:error, "Post id #{id} not found"}
    end
  end

  def find(%{author: author}) do
    case Posts.get_posts_by_author(author) do
      post -> {:ok, post}
      _ -> {:error, "Post author #{author} not found"}
    end
  end

  def find(%{hashtag: hashtag}) do
    case Posts.get_posts_by_hashtag(hashtag) do
      post -> {:ok, post}
      _ -> {:error, "Post hashtag #{hashtag} not found"}
    end
  end
end
