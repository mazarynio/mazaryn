defmodule Mazaryn.Posts do
  @moduledoc """
  Public Mazaryn.Posts API
  """

  require Logger

  alias Core.PostClient
  alias Mazaryn.Schema.Post

  @spec(create_post(%Ecto.Changeset{}, :map) :: %Post{}, {:error, :string})
  def create_post(%Ecto.Changeset{valid?: false} = changeset, _after_save \\ %{}), do: changeset

  def create_post(
        %Ecto.Changeset{changes: %{author: author, content: content}} = changeset,
        after_save
      ) do
    media = Ecto.Changeset.get_field(changeset, :media, [])

    case create(author, content, media) do
      {:ok, post_id} ->
        one_by_id(post_id)
        |> after_save(after_save)

      {:error, some_error} ->
        {:error, some_error}
    end
  end

  # REVER
  defp after_save(%Post{} = post, func) do
    {:ok, _post} = func.(post) |> IO.inspect(label: "333")
  end

  defp after_save(error, _func), do: error

  @spec create(String.t(), String.t(), list(String.t()), list(String.t())) :: any
  def create(author, content, media, _other \\ []) do
    case PostClient.create(author, content, media) do
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

  def get_home_posts do
    case PostClient.get_posts() do
      posts when is_list(posts) ->
        for post_id <- posts do
          {:ok, post} = one_by_id(post_id)
          post
        end

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
