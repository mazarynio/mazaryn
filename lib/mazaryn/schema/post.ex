defmodule Mazaryn.Schema.Post do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Post
  """

  use Ecto.Schema
  alias Timex
  import Ecto.Changeset
  alias Mazaryn.Schema.Comment
  alias Account.Users
  alias Core.PostClient

  @optional_fields ~w(
    id
    ai_post_id
    media
    hashtag
    mention
    emoji
    link_url
    photo_url
    author
    other
    comments
    likes
    profile_tags
    date_created
    date_updated
    report
    device_info
    data
  )a

  @required_fields ~w(
    content
  )a

  # date_created
  embedded_schema do
    field(:content, :string)
    field(:ai_post_id, :string)
    field(:media, {:array, :string}, default: [])
    field(:hashtag, :string)
    field(:mention, :string)
    field(:emoji, :string)
    field(:link_url, :string)
    field(:author, :string)
    field(:other, {:array, :string}, default: [])
    # TODO: Add to mnesia
    field(:comments, {:array, :string}, default: [])
    field(:profile_tags, {:array, :string}, default: [])
    field(:likes, {:array, :integer}, default: [])

    field(:photo_url, :string)
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:report, {:array, :string}, default: [])
    field(:device_info, :string)
    field(:data, :map)
  end

  def erl_changeset(
        {:post, id, ai_post_id, content, comments, likes, media, hashtag, mention, emoji,
         link_url, author, other, date_created, date_updated, report, device_info, data}
      ) do
    new_likes =
      case likes do
        list when is_list(list) -> list
        _ -> []
      end

    preload_comments = preload_comments(comments)

    %__MODULE__{}
    |> change(%{
      id: id,
      ai_post_id: ai_post_id,
      content: content,
      comments: preload_comments,
      likes: new_likes,
      media: media,
      hashtag: hashtag,
      mention: mention,
      emoji: emoji,
      link_url: link_url,
      author: author,
      other: other,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: report,
      device_info: device_info,
      data: data
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
          |> PostClient.get_single_comment()
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
