defmodule Mazaryn.HashtagIndexer do
  use GenServer
  require Logger

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def init(_state) do
    {:ok, %{index: %{}, last_updated: nil}}
  end

  def get_posts_by_hashtag(hashtag) do
    normalized = normalize_hashtag(hashtag)

    current_posts = Mazaryn.Posts.get_posts()

    Enum.filter(current_posts, fn post ->
      hashtags = extract_all_hashtags_from_post(post)
      normalized in hashtags
    end)
  end

  def rebuild_index do
    GenServer.cast(__MODULE__, :rebuild)
  end

  def handle_cast(:rebuild, _state) do
    Logger.info("Rebuilding hashtag index...")

    new_index = build_hashtag_index()

    Logger.info("Hashtag index rebuilt: #{map_size(new_index)} unique hashtags")

    {:noreply, %{index: new_index, last_updated: DateTime.utc_now()}}
  end

  defp build_hashtag_index do
    all_posts = Mazaryn.Posts.get_posts()

    Logger.info("Indexing #{length(all_posts)} posts...")

    all_posts
    |> Enum.reduce(%{}, fn post, acc ->
      hashtags = extract_all_hashtags_from_post(post)

      Enum.reduce(hashtags, acc, fn hashtag, inner_acc ->
        Map.update(inner_acc, hashtag, [post], fn existing ->
          [post | existing]
        end)
      end)
    end)
  end

  defp extract_all_hashtags_from_post(post) do
    actual_content = get_real_post_content(post)

    content_hashtags = extract_from_text(actual_content)
    profile_hashtags = extract_from_profile_tags(post.profile_tags)
    mention_hashtags = extract_from_mention_field(post)

    (content_hashtags ++ profile_hashtags ++ mention_hashtags)
    |> Enum.map(&normalize_hashtag/1)
    |> Enum.reject(&(&1 == ""))
    |> Enum.uniq()
  end

  defp get_real_post_content(post) do
    try do
      post_id = post.id |> to_charlist()

      case Core.PostClient.get_post_content_by_id(post_id) do
        content when is_binary(content) ->
          content

        content when is_list(content) ->
          List.to_string(content)

        _ ->
          ""
      end
    rescue
      _ -> ""
    end
  end

  defp extract_from_text(text) do
    str = safe_string(text)

    Regex.scan(~r/#([a-zA-Z0-9_]+)/u, str)
    |> Enum.map(fn [_, tag] -> tag end)
  end

  defp extract_from_profile_tags(nil), do: []
  defp extract_from_profile_tags([]), do: []

  defp extract_from_profile_tags(tags) when is_list(tags) do
    Enum.flat_map(tags, fn tag ->
      str = safe_string(tag)

      cond do
        String.contains?(str, "#") ->
          Regex.scan(~r/#([a-zA-Z0-9_]+)/u, str)
          |> Enum.map(fn [_, tag] -> tag end)

        String.trim(str) != "" ->
          [str]

        true ->
          []
      end
    end)
  end

  defp extract_from_profile_tags(_), do: []

  defp extract_from_mention_field(post) do
    try do
      cond do
        Map.has_key?(post, :hashtag) and post.hashtag != nil ->
          str = safe_string(post.hashtag)
          [str]

        true ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp normalize_hashtag(tag) when is_binary(tag) do
    tag
    |> String.trim()
    |> String.downcase()
    |> String.replace(~r/^#+/, "")
  end

  defp normalize_hashtag(tag) when is_list(tag) do
    try do
      tag |> List.to_string() |> normalize_hashtag()
    rescue
      _ -> ""
    end
  end

  defp normalize_hashtag(_), do: ""

  defp safe_string(nil), do: ""
  defp safe_string(val) when is_binary(val), do: val
  defp safe_string(val) when is_atom(val), do: Atom.to_string(val)

  defp safe_string(val) when is_list(val) do
    if is_charlist?(val) do
      try do
        List.to_string(val)
      rescue
        _ -> ""
      end
    else
      val |> Enum.map(&safe_string/1) |> Enum.join(" ")
    end
  end

  defp safe_string(_), do: ""

  defp is_charlist?(list) when is_list(list) do
    Enum.all?(list, fn x -> is_integer(x) and x >= 0 and x <= 1_114_111 end)
  end

  defp is_charlist?(_), do: false
end
