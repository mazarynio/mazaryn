defmodule MazarynWeb.HomeLive.PostHelpers do
  @moduledoc """
  Helper functions for detecting and handling different post types
  """

  def is_video_share_post?(post) do
    content = get_actual_post_content(post)
    result = String.starts_with?(content, "VIDEO_SHARE:")
    result
  end

  defp get_actual_post_content(post) do
    try do
      post_id_charlist =
        case post.id do
          id when is_binary(id) -> String.to_charlist(id)
          id when is_list(id) -> id
          _ -> String.to_charlist(to_string(post.id))
        end

      case Core.PostClient.get_post_content_by_id(post_id_charlist) do
        content when is_list(content) ->
          List.to_string(content)

        content when is_binary(content) ->
          content

        _ ->
          to_string(post.content || "")
      end
    rescue
      e ->
        to_string(post.content || "")
    end
  end

  def extract_video_id(post) do
    content = get_actual_post_content(post)

    case String.split(content, "\n", parts: 2) do
      [header | _] ->
        case String.split(header, "|") do
          [video_share_part | _] ->
            video_share_part
            |> String.replace_prefix("VIDEO_SHARE:", "")
            |> String.trim()

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  def parse_video_share_content(post) do
    content = get_actual_post_content(post)

    case String.split(content, "\n", parts: 2) do
      [header, description] ->
        parse_header_with_description(header, String.trim(description), post)

      [header] ->
        parse_header_with_description(header, nil, post)

      _ ->
        %{
          video_id: nil,
          video_url: nil,
          title: "Shared Video",
          description: nil,
          video_owner: to_string(post.author),
          sharer: to_string(post.author)
        }
    end
  end

  defp parse_header_with_description(header, description, post) do
    parts = String.split(header, "|")

    case parts do
      [video_share_part, url, title, video_owner, sharer] ->
        video_id = String.replace_prefix(video_share_part, "VIDEO_SHARE:", "") |> String.trim()

        %{
          video_id: video_id,
          video_url: String.trim(url),
          title: String.trim(title),
          description: description,
          video_owner: String.trim(video_owner),
          sharer: String.trim(sharer)
        }

      [video_share_part, url, title] ->
        video_id = String.replace_prefix(video_share_part, "VIDEO_SHARE:", "") |> String.trim()

        %{
          video_id: video_id,
          video_url: String.trim(url),
          title: String.trim(title),
          description: description,
          video_owner: to_string(post.author),
          sharer: to_string(post.author)
        }

      [video_share_part, url] ->
        video_id = String.replace_prefix(video_share_part, "VIDEO_SHARE:", "") |> String.trim()

        %{
          video_id: video_id,
          video_url: String.trim(url),
          title: "Shared Video",
          description: description,
          video_owner: to_string(post.author),
          sharer: to_string(post.author)
        }

      [video_share_part] ->
        video_id = String.replace_prefix(video_share_part, "VIDEO_SHARE:", "") |> String.trim()

        %{
          video_id: video_id,
          video_url: nil,
          title: "Shared Video",
          description: description,
          video_owner: to_string(post.author),
          sharer: to_string(post.author)
        }

      _ ->
        %{
          video_id: nil,
          video_url: nil,
          title: "Shared Video",
          description: description,
          video_owner: to_string(post.author),
          sharer: to_string(post.author)
        }
    end
  end

  def get_video_thumbnail(post) do
    case post.media do
      media when is_list(media) and length(media) > 0 ->
        List.first(media)

      media when is_binary(media) and media != "" ->
        media

      _ ->
        "/images/default-video-thumbnail.svg"
    end
  end

  def get_display_author(post) do
    if is_video_share_post?(post) do
      video_data = parse_video_share_content(post)
      video_data.video_owner
    else
      to_string(post.author)
    end
  end

  def is_current_user_sharer?(post, current_user_username) do
    if is_video_share_post?(post) do
      video_data = parse_video_share_content(post)
      video_data.sharer == to_string(current_user_username)
    else
      to_string(post.author) == to_string(current_user_username)
    end
  end

  def get_sharer(post) do
    if is_video_share_post?(post) do
      video_data = parse_video_share_content(post)
      video_data.sharer
    else
      to_string(post.author)
    end
  end
end
