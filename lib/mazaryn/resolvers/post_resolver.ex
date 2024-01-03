defmodule Resolvers.PostResolver do
  alias Mazaryn.Posts

  def all(_args, _info) do
    {:ok, Posts.get_posts()}
  end

  def create(args, _info) do
    author = args[:author]
    content = args[:content]
    media = args[:media]
    hashtag = args[:hashtag]
    mention = args[:mention]
    link_url = args[:link_url]
    Posts.create_a_post(author, content, media, hashtag, mention, link_url)
  end

  def find_post_by_id(%{id: id}, _info) do
    Posts.one_by_id(id)
  end

  def find_post_by_author(%{author: author}, _info) do
    Posts.get_posts_by_author(author)
  end

  def find_post_by_hashtag(%{hashtag: hashtag}, _info) do
    Posts.get_posts_by_hashtag(hashtag)
  end
end
