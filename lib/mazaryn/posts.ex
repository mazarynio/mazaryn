defmodule Mazaryn.Posts do
  @moduledoc """
  Public Mazaryn.Posts API
  """

  require Logger

  alias Core.PostClient
  alias Mazaryn.Schema.Post
  alias Mazaryn.Schema.Comment

  def update_comment(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def update_comment(%{changes: %{id: comment_id, content: content}}) do
    comment_id = comment_id |> to_charlist

    case PostClient.update_comment(comment_id, content) do
      :ok ->
        PostClient.get_single_comment(comment_id)
        |> Comment.erl_changeset()

      {:error, some_error} ->
        {:error, some_error}
    end
  end

  def create_comment(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_comment(
        %Ecto.Changeset{changes: %{author: author, content: content, post_id: post_id}} =
          _changeset
      ) do

    author = author |> to_charlist
    post_id = post_id |> to_charlist

    case Core.PostClient.add_comment(author, post_id, content) do
      {:error, some_error} ->
        {:error, some_error}

      comment_id ->
        Core.PostClient.get_single_comment(comment_id)
        |> Comment.erl_changeset()
    end
  end

  @spec(create_post(%Ecto.Changeset{}) :: %Post{}, {:error, :string})
  def create_post(%Ecto.Changeset{valid?: false} = changeset), do: changeset

  def create_post(%Ecto.Changeset{changes: %{author: author, content: content}} = changeset) do
    media = Ecto.Changeset.get_field(changeset, :media, [])
    hashtag = Ecto.Changeset.get_field(changeset, :hashtag)
    mention = Ecto.Changeset.get_field(changeset, :mention)
    link_url = Ecto.Changeset.get_field(changeset, :link_url)

    case create(author, content, media, hashtag, mention, link_url) do
      {:ok, post_id} ->
        one_by_id(post_id)

      {:error, some_error} ->
        {:error, some_error}
    end
  end

  @doc """
  Currently returns the post_id
  """

  def create(author, content, media, hashtag, mention, link_url) do
    {:ok, PostClient.create(author, content, media, hashtag, mention, link_url)}
  end

  def one_by_id(id) do
    case Core.PostClient.get_by_id(id) do
      erl_post ->
        erl_post
        |> Post.erl_changeset()
        |> Post.build()
    end
  end

  # comments
  def build_comments_structure(comment_id) do
    case Core.PostClient.get_single_comment(comment_id) do
      {:ok, comment_tuple} ->
        case comment_tuple
             |> Mazaryn.Schema.Comment.erl_changeset()
             |> Mazaryn.Schema.Comment.build() do
          {:ok, comment_struct} -> comment_struct
          {:error, _reason} -> %{}
        end

      {:error, _reason} ->
        %{}
    end
  end

  # fetch comments by post id
  def get_comment_by_post_id(post_id) when is_binary(post_id) do
    post_id
    |> String.to_charlist()
    |> :postdb.get_all_comment_ids()
    |> Enum.map(&build_comments_structure/1)
    |> Enum.reject(&(&1 == %{}))
  end

  def get_comment_by_post_id(post_id) when is_list(post_id) do
    post_id
    |> :postdb.get_all_comment_ids()
    |> Enum.map(&build_comments_structure/1)
    |> Enum.reject(&(&1 == %{}))
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

  def get_posts_by_user_id(userID) do
    case PostClient.get_posts_by_user_id(userID) do
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

  def get_posts() do
    case PostClient.get_posts() do
      posts when is_list(posts) ->
        for post_id <- posts do
          {:ok, post} = one_by_id(post_id)
          post
        end
        |> Enum.sort_by(& &1.date_created, &>=/2)

      _ ->
        Logger.error("error getting posts")
    end
  end

  def create_a_post(author, content, media \\ [], hashtag, mention, link_url) do
    PostClient.create(author, content, media, hashtag, mention, link_url)
    |> one_by_id()
  end

  @spec get_likes_by_post_id(integer()) :: list(map())
  def get_likes_by_post_id(post_id) do
    Core.PostClient.get_likes(post_id)
  end
end
