defmodule Mazaryn.Schema.BlogPost do
  use Ecto.Schema
  alias Timex
  import Ecto.Changeset
  alias Account.Users
  alias Core.BlogClient
  alias Mazaryn.Schema.Comment

  @optional_fields ~w(
    id
    comments
    media
    author
    date_created
    date_updated
  )

  @required_fields ~w(
    content
  )

  embedded_schema do
    field(:content, :string)
    field(:comments, {:array, :string}, default: [])
    field(:media, {:array, :string}, default: [])
    field(:author, :string)
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
  end

  def erl_changeset(
        {:blog_post, id, content, comments, media, author, date_created,
         date_updated}
      ) do

    preload_comments = preload_comments(comments)

    %__MODULE__{}
    |> change(%{
      id: id,
      content: content,
      comments: preload_comments,
      media: media,
      author: author,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_created)
    })
  end

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  def changeset(post, attrs \\ %{}) do
    post
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  defp preload_comments([]), do: []

  defp preload_comments(comments) do
    Enum.map(comments, fn comment ->
      case comment do
        comment when is_tuple(comment) ->
          comment
          |> Comment.erl_changeset()
          |> Comment.build()
          |> case do
            {:ok, comment} -> build_comment_struct(comment)
            _ -> %{}
          end

        comment ->
          comment
          |> BlogClient.get_single_comment()
          |> Comment.erl_changeset()
          |> Comment.build()
          |> case do
            {:ok, comment} -> build_comment_struct(comment)
            _ -> %{}
          end
      end
    end)
    |> Enum.filter(&(&1 != %{}))
    |> Enum.sort_by(& &1.date_created, :desc)
  end

  defp build_comment_struct(comment) do
    author =
      comment.author
      |> Users.one_by_id()
      |> elem(1)

    %{
      id: comment.id,
      author: author,
      date_created: comment.date_created,
      content: comment.content,
      post_id: comment.post_id
    }
  end
end
