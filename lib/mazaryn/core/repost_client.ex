defmodule Core.RepostClient do
  def simple_repost(user_id, original_post_id) do
    :repostdb.simple_repost(user_id, original_post_id)
  end

  def repost_with_comment(user_id, original_post_id, comment) do
    IO.puts("📞 RepostClient.repost_with_comment called")
    IO.puts("📞 User ID: #{inspect(user_id)}")
    IO.puts("📞 Original Post ID: #{inspect(original_post_id)}")
    IO.puts("📞 Comment: #{inspect(comment)}")

    result = :repostdb.repost_with_comment(user_id, original_post_id, comment)
    IO.puts("📞 Erlang result: #{inspect(result)}")
    result
  end

  def unrepost(user_id, repost_id) do
    :repostdb.unrepost(user_id, repost_id)
  end

  def get_repost(repost_id) do
    :repostdb.get_repost(repost_id)
  end

  def get_original_post(repost_id) do
    :repostdb.get_original_post(repost_id)
  end

  def get_user_reposts(user_id) do
    :repostdb.get_user_reposts(user_id)
  end

  def get_post_reposters(post_id) do
    :repostdb.get_post_reposters(post_id)
  end

  def is_reposted_by_user(user_id, post_id) do
    :repostdb.is_reposted_by_user(user_id, post_id)
  end

  def get_repost_count(post_id) do
    :repostdb.get_repost_count(post_id)
  end

  def get_repost_type(repost_id) do
    :repostdb.get_repost_type(repost_id)
  end

  def get_target_post_for_interaction(post_id) do
    :repostdb.get_target_post_for_interaction(post_id)
  end

  def validate_repost(user_id, original_post_id) do
    :repostdb.validate_repost(user_id, original_post_id)
  end

  def get_repost_comment_content(repost_id) do
    repost_id_charlist = to_charlist_safe(repost_id)

    case :repostdb.get_repost_comment_content(repost_id_charlist) do
      {:error, reason} ->
        {:error, reason}

      nil ->
        nil

      {:ok, undefined} ->
        nil

      {:ok, ""} ->
        ""

      {:ok, content} when is_list(content) ->
        List.to_string(content)

      {:ok, content} when is_binary(content) ->
        content

      content when is_list(content) ->
        List.to_string(content)

      content when is_binary(content) ->
        content

      other ->
        IO.warn("Unexpected repost comment content format: #{inspect(other)}")
        safe_to_string(other)
    end
  end

  def has_repost_comment?(repost_id) do
    case get_repost_type(repost_id) do
      {:ok, :with_comment} -> true
      {:ok, "with_comment"} -> true
      _ -> false
    end
  end

  def get_repost_with_comment(repost_id) do
    case get_repost(repost_id) do
      {:ok, repost} ->
        comment_content = get_repost_comment_content(repost_id)
        {:ok, Map.put(repost, :comment_content, comment_content)}

      error ->
        error
    end
  end

  defp to_charlist_safe(value) when is_binary(value), do: String.to_charlist(value)
  defp to_charlist_safe(value) when is_list(value), do: value
  defp to_charlist_safe(value), do: to_charlist(value)

  defp safe_to_string(nil), do: nil
  defp safe_to_string(bin) when is_binary(bin), do: bin

  defp safe_to_string(list) when is_list(list) do
    try do
      List.to_string(list)
    rescue
      _ -> nil
    end
  end

  defp safe_to_string(_), do: nil
end
