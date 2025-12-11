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
        case String.split(header, "|", parts: 3) do
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
        parse_header_with_description(header, String.trim(description))

      [header] ->
        parse_header_with_description(header, nil)

      _ ->
        %{video_id: nil, video_url: nil, title: "Shared Video", description: nil}
    end
  end

  defp parse_header_with_description(header, description) do
    case String.split(header, "|", parts: 3) do
      [video_share_part, url, title] ->
        video_id = String.replace_prefix(video_share_part, "VIDEO_SHARE:", "") |> String.trim()

        %{
          video_id: video_id,
          video_url: String.trim(url),
          title: String.trim(title),
          description: description
        }

      [video_share_part, url] ->
        video_id = String.replace_prefix(video_share_part, "VIDEO_SHARE:", "") |> String.trim()

        %{
          video_id: video_id,
          video_url: String.trim(url),
          title: "Shared Video",
          description: description
        }

      [video_share_part] ->
        video_id = String.replace_prefix(video_share_part, "VIDEO_SHARE:", "") |> String.trim()

        %{
          video_id: video_id,
          video_url: nil,
          title: "Shared Video",
          description: description
        }

      _ ->
        %{video_id: nil, video_url: nil, title: "Shared Video", description: description}
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
end
