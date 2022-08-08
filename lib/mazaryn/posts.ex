defmodule Mazaryn.Posts do
  @moduledoc """
  Public Mazaryn.Posts API
  """

  require Logger

  alias Mazaryn.Schema.Post

  @spec(create_post(%Ecto.Changeset{}, :map) :: %Post{}, {:error, :string})
  def create_post(%Ecto.Changeset{valid?: false} = changeset, _after_save \\ %{}), do: changeset

  def create_post(
        %Ecto.Changeset{changes: %{author: author, content: content}} = changeset,
        _after_save
      ) do
    media = Ecto.Changeset.get_field(changeset, :media, [])

    case create(author, content, media) do
      {:ok, post_id} ->
        one_by_id(post_id)

      {:error, some_error} ->
        {:error, some_error}
    end
  end

  @spec create(String.t(), String.t(), list(String.t()), list(String.t())) :: any
  def create(author, content, media, _other \\ []) do
    case Core.PostClient.create(author, content, media) do
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
