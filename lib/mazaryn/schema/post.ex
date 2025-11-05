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
    user_id
    business_id
    media
    hashtag
    mention
    ipns
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
    pin_info
    is_repost
    original_post_id
    repost_type
    repost_comment
    repost_count
    reposted_by
    data
  )a

  @required_fields ~w(
    content
  )a

  embedded_schema do
    field(:content, :string)
    field(:ai_post_id, :string)
    field(:user_id, :string)
    field(:business_id, :string)
    field(:media, {:array, :string}, default: [])
    field(:hashtag, :string)
    field(:mention, :string)
    field(:ipns, :string)
    field(:emoji, :string)
    field(:link_url, :string)
    field(:author, :string)
    field(:other, {:array, :string}, default: [])
    field(:comments, {:array, :string}, default: [])
    field(:profile_tags, {:array, :string}, default: [])
    field(:likes, {:array, :integer}, default: [])

    field(:photo_url, :string)
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:report, {:array, :string}, default: [])
    field(:device_info, :string)
    field(:pin_info, :map)

    field(:is_repost, :boolean, default: false)
    field(:original_post_id, :string)
    field(:repost_type, :string)
    field(:repost_comment, :string)
    field(:repost_count, :integer, default: 0)
    field(:reposted_by, {:array, :map}, default: [])

    field(:data, :map)
  end

  def erl_changeset(
        {:post, id, ai_post_id, user_id, business_id, content, ipns, emoji, comments, likes,
         media, hashtag, mention, link_url, author, other, date_created, date_updated, report,
         device_info, pin_info, is_repost, original_post_id, repost_type, repost_comment,
         repost_count, reposted_by, data}
      ) do
    new_likes =
      case likes do
        list when is_list(list) -> list
        _ -> []
      end

    new_comments =
      case comments do
        list when is_list(list) -> list
        nil -> []
        :undefined -> []
        _ -> []
      end

    preload_comments = preload_comments(new_comments)

    is_repost_value =
      case is_repost do
        true -> true
        false -> false
        :undefined -> false
        _ -> false
      end

    original_post_id_value =
      case original_post_id do
        :undefined -> nil
        nil -> nil
        value when is_binary(value) -> value
        value when is_list(value) -> to_string(value)
        _ -> nil
      end

    repost_type_value =
      case repost_type do
        :simple -> "simple"
        :with_comment -> "with_comment"
        "simple" -> "simple"
        "with_comment" -> "with_comment"
        :undefined -> nil
        _ -> nil
      end

    repost_comment_value =
      case repost_comment do
        :undefined ->
          nil

        nil ->
          nil

        {:repost_comment, _id} = tuple ->
          tuple
          |> :erlang.term_to_binary()
          |> Base.encode64()

        value when is_binary(value) ->
          value

        value when is_list(value) ->
          to_string(value)

        _ ->
          nil
      end

    repost_count_value =
      case repost_count do
        count when is_integer(count) -> count
        :undefined -> 0
        _ -> 0
      end

    reposted_by_value =
      case reposted_by do
        list when is_list(list) ->
          Enum.map(list, fn
            {user_id, repost_id, type, date} ->
              %{
                user_id: to_string(user_id),
                repost_id: to_string(repost_id),
                type: atom_to_string(type),
                date: handle_datetime(date)
              }

            _ ->
              nil
          end)
          |> Enum.reject(&is_nil/1)

        _ ->
          []
      end

    %__MODULE__{}
    |> change(%{
      id: id,
      ai_post_id: ai_post_id,
      user_id: user_id,
      business_id: business_id,
      content: content,
      comments: preload_comments,
      likes: new_likes,
      media: media,
      hashtag: hashtag,
      mention: mention,
      ipns: ipns,
      emoji: emoji,
      link_url: link_url,
      author: author,
      other: other,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: report,
      device_info: device_info,
      pin_info: pin_info,
      is_repost: is_repost_value,
      original_post_id: original_post_id_value,
      repost_type: repost_type_value,
      repost_comment: repost_comment_value,
      repost_count: repost_count_value,
      reposted_by: reposted_by_value,
      data: data
    })
  end

  def erl_changeset(
        {:post, id, ai_post_id, user_id, business_id, content, comments, likes, media, hashtag,
         mention, ipns, emoji, link_url, author, other, date_created, date_updated, report,
         device_info, pin_info, data}
      ) do
    new_likes =
      case likes do
        list when is_list(list) -> list
        _ -> []
      end

    new_comments =
      case comments do
        list when is_list(list) -> list
        nil -> []
        :undefined -> []
        _ -> []
      end

    preload_comments = preload_comments(new_comments)

    %__MODULE__{}
    |> change(%{
      id: id,
      ai_post_id: ai_post_id,
      user_id: user_id,
      business_id: business_id,
      content: content,
      comments: preload_comments,
      likes: new_likes,
      media: media,
      hashtag: hashtag,
      mention: mention,
      ipns: ipns,
      emoji: emoji,
      link_url: link_url,
      author: author,
      other: other,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: report,
      device_info: device_info,
      pin_info: pin_info,
      is_repost: false,
      original_post_id: nil,
      repost_type: nil,
      repost_comment: nil,
      repost_count: 0,
      reposted_by: [],
      data: data
    })
  end

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(nil), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  defp atom_to_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp atom_to_string(string) when is_binary(string), do: string
  defp atom_to_string(_), do: nil

  def changeset(post, attrs \\ %{}) do
    post
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  defp preload_comments(nil), do: []
  defp preload_comments([]), do: []

  defp preload_comments(comments) when is_list(comments) do
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

  def simple_repost?(%__MODULE__{is_repost: true, repost_type: "simple"}), do: true
  def simple_repost?(_), do: false

  def repost_with_comment?(%__MODULE__{is_repost: true, repost_type: "with_comment"}), do: true
  def repost_with_comment?(_), do: false

  def get_original_author(%__MODULE__{is_repost: true, original_post_id: original_id})
      when not is_nil(original_id) do
    case PostClient.get_by_id(to_charlist(original_id)) do
      post when is_tuple(post) ->
        post
        |> erl_changeset()
        |> build()
        |> case do
          {:ok, original_post} -> original_post.author
          _ -> nil
        end

      _ ->
        nil
    end
  end

  def get_original_author(_), do: nil
end
