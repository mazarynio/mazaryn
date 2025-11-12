defmodule Mazaryn.Schema.Comment do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Comment
  """
  use Ecto.Schema
  import Ecto.Changeset

  @optional_fields ~w(
    id
    user_id
    content_status
    date_created
    likes
    replies
    ipns
    data
    like_comment_event
    reactions
    reaction_counts
  )a

  @required_fields ~w(
    content
    author
    post_id
  )a

  embedded_schema do
    field(:user_id, :string)
    field(:content, :string)
    field(:content_status, :string)
    field(:date_created, :utc_datetime)
    field(:likes, {:array, :string}, default: [])
    field(:replies, {:array, :string}, default: [])
    field(:ipns, :string)
    field(:author, :string)
    field(:post_id, :string)
    field(:data, :map)
    field(:like_comment_event, :string)

    field(:reactions, :map,
      default: %{
        "like" => [],
        "celebrate" => [],
        "support" => [],
        "love" => [],
        "insightful" => [],
        "funny" => []
      }
    )

    field(:reaction_counts, :map,
      default: %{
        "like" => 0,
        "celebrate" => 0,
        "support" => 0,
        "love" => 0,
        "insightful" => 0,
        "funny" => 0
      }
    )
  end

  def erl_changeset(
        {:comment, id, user_id, post, author, content, content_status, date_created, ipns, likes,
         reactions, reaction_counts, replies, data}
      ) do
    processed_reactions = process_reactions(reactions)
    processed_reaction_counts = process_reaction_counts(reaction_counts)
    processed_likes = process_likes(likes)
    processed_replies = process_replies(replies)

    %__MODULE__{}
    |> change(%{
      id: id,
      user_id: user_id,
      post_id: post,
      author: author,
      content: content,
      content_status: content_status,
      date_created: handle_datetime(date_created),
      ipns: ipns,
      likes: processed_likes,
      reactions: processed_reactions,
      reaction_counts: processed_reaction_counts,
      replies: processed_replies,
      data: data,
      like_comment_event: nil
    })
  end

  def erl_changeset(
        {:comment, id, user_id, post, author, content, content_status, date_created, likes,
         replies, ipns, data}
      ) do
    processed_likes = process_likes(likes)
    processed_replies = process_replies(replies)

    %__MODULE__{}
    |> change(%{
      id: id,
      user_id: user_id,
      post_id: post,
      author: author,
      content: content,
      content_status: content_status,
      date_created: handle_datetime(date_created),
      likes: processed_likes,
      replies: processed_replies,
      ipns: ipns,
      data: data,
      like_comment_event: nil,
      reactions: %{
        "like" => [],
        "celebrate" => [],
        "support" => [],
        "love" => [],
        "insightful" => [],
        "funny" => []
      },
      reaction_counts: %{
        "like" => 0,
        "celebrate" => 0,
        "support" => 0,
        "love" => 0,
        "insightful" => 0,
        "funny" => 0
      }
    })
  end

  def erl_changeset(_), do: %{}

  defp process_reactions(reactions) when is_map(reactions) do
    Enum.reduce(reactions, %{}, fn {key, value}, acc ->
      string_key = atom_to_string(key)

      processed_value =
        case value do
          list when is_list(list) -> list
          _ -> []
        end

      Map.put(acc, string_key, processed_value)
    end)
  end

  defp process_reactions(_),
    do: %{
      "like" => [],
      "celebrate" => [],
      "support" => [],
      "love" => [],
      "insightful" => [],
      "funny" => []
    }

  defp process_reaction_counts(reaction_counts) when is_map(reaction_counts) do
    Enum.reduce(reaction_counts, %{}, fn {key, value}, acc ->
      string_key = atom_to_string(key)

      count =
        case value do
          num when is_integer(num) -> num
          _ -> 0
        end

      Map.put(acc, string_key, count)
    end)
  end

  defp process_reaction_counts(_),
    do: %{
      "like" => 0,
      "celebrate" => 0,
      "support" => 0,
      "love" => 0,
      "insightful" => 0,
      "funny" => 0
    }

  defp process_likes(likes) when is_list(likes), do: likes
  defp process_likes(_), do: []

  defp process_replies(replies) when is_list(replies), do: replies
  defp process_replies(_), do: []

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(nil), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  defp atom_to_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp atom_to_string(string) when is_binary(string), do: string
  defp atom_to_string(_), do: "like"

  def changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, @optional_fields ++ @required_fields)
    |> validate_required(@required_fields)
  end

  def update_changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, [:id, :content, :like_comment_event, :reactions, :reaction_counts])
    |> validate_required([:id, :content])
  end

  def build(map) when map == %{}, do: %{}

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def total_reactions(%__MODULE__{reaction_counts: counts}) when is_map(counts) do
    counts
    |> Map.values()
    |> Enum.sum()
  end

  def total_reactions(_), do: 0

  def user_reacted?(%__MODULE__{reactions: reactions}, user_id) when is_map(reactions) do
    reactions
    |> Map.values()
    |> List.flatten()
    |> Enum.any?(fn reaction_id -> reaction_id == user_id end)
  end

  def user_reacted?(_, _), do: false

  def user_reaction_type(%__MODULE__{reactions: reactions}, user_id) when is_map(reactions) do
    Enum.find_value(reactions, fn {type, reaction_list} ->
      if Enum.member?(reaction_list, user_id), do: type
    end)
  end

  def user_reaction_type(_, _), do: nil
end
