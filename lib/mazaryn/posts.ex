defmodule Mazaryn.Posts do
  @moduledoc """
  Public Mazaryn.Posts API
  """

  require Logger

  alias Core.PostClient
  alias Mazaryn.Schema.Post

  @spec(create_post(%Ecto.Changeset{}) :: %Post{}, {:error, :string})
  def create_post(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_post(%Ecto.Changeset{changes: %{author: author, content: content}} = changeset) do
    media = Ecto.Changeset.get_field(changeset, :media, [])
    hashtag = Ecto.Changeset.get_field(changeset, :hashtag)

    case create(author, content, media, hashtag) do
      {:ok, post_id} ->
        one_by_id(post_id)

      {:error, some_error} ->
        {:error, some_error}
    end
  end

  @spec create(String.t(), String.t(), list(String.t()), list(String.t())) :: any
  def create(author, content, media, hashtag, _other \\ []) do
    case PostClient.create(author, content, media, hashtag) do
      post_id when is_binary(post_id) ->
        {:ok, post_id}

      something_else ->
        Logger.warn(something_else)
        {:error, something_else}
    end
  end

  def one_by_id(id) do
    case Core.PostClient.get_by_id(id) do
      erl_post ->
        erl_post
        |> Post.erl_changeset()
        |> Post.build()
    end
  end

  def get_posts_by_author(author) do
    case PostClient.get_posts_by_author(author) do
      posts when is_list(posts) ->
        for post <- posts do
          {:ok, post} =
            post
            |> Post.erl_changeset()
            |> Post.build()

          post
        end
        |> Enum.sort_by(& &1.date_created, &>=/2)

      _ ->
        Logger.error("handle here")
    end
  end

  def get_posts_by_hashtag(hashtag) do
    case PostClient.get_posts_by_hashtag(hashtag) do
      posts when is_list(posts) ->
        for post <- posts do
          {:ok, post} =
            post
            |> Post.erl_changeset()
            |> Post.build()

          post
        end
        |> Enum.sort_by(& &1.date_created, &>=/2)

      _ ->
        Logger.error("handle here")
    end
  end

  def get_home_posts do
    case PostClient.get_posts() do
      posts when is_list(posts) ->
        for post_id <- posts do
          {:ok, post} = one_by_id(post_id)
          post
        end
        |> Enum.sort_by(& &1.date_created, &>=/2)

      _something_else ->
        Logger.error("handle here")
    end
  end

  def posts_from_user_following(_email) do
    []
    #   {:ok, user} = Users.one_by_email(email)

    #   following = Users.get_following(user.id)

    #   Logger.info("[Posts] Getting posts from following:")

    #   result = Enum.map(following, fn user -> posts_from_user(user.username) end)

    #   Logger.info(result)

    #   Enum.shuffle(result)
    # end
  end
end
