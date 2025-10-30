defmodule MazarynWeb.HomeLive.PostComponent.ContentHelper do
  alias MazarynWeb.Router.Helpers, as: Routes

  def activate_content_characters(post, socket) do
    locale = socket.assigns[:locale] || "en"

    content_text = convert_to_string(post.content)

    content_text
    |> parse_and_linkify_hashtags(socket, locale)
    |> parse_and_linkify_mentions(socket, locale)
    |> parse_and_linkify_urls()
  end

  defp parse_and_linkify_hashtags(text, socket, locale) do
    Regex.replace(~r/#([^\s#]+)/u, text, fn full_match, hashtag ->
      url = Routes.live_path(socket, MazarynWeb.HashtagLive.Index, locale, hashtag)

      ~s(<a href="#{url}" class="text-blue-600 dark:text-blue-400 hover:underline font-semibold">#{full_match}</a>)
    end)
  end

  defp parse_and_linkify_mentions(text, socket, locale) do
    Regex.replace(~r/@([^\s@]+)/u, text, fn full_match, username ->
      url = Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username)

      ~s(<a href="#{url}" class="text-blue-600 dark:text-blue-400 hover:underline font-semibold">#{full_match}</a>)
    end)
  end

  defp parse_and_linkify_urls(text) do
    Regex.replace(~r/(https?:\/\/[^\s]+)/u, text, fn url ->
      ~s(<a href="#{url}" target="_blank" rel="noopener noreferrer" class="text-blue-600 dark:text-blue-400 hover:underline">#{url}</a>)
    end)
  end

  defp convert_to_string(val) when is_binary(val), do: val

  defp convert_to_string(val) when is_list(val) do
    cond do
      Enum.all?(val, &is_integer/1) ->
        try do
          List.to_string(val)
        rescue
          _ -> inspect(val)
        end

      true ->
        val |> Enum.map(&convert_to_string/1) |> Enum.join(" ")
    end
  end

  defp convert_to_string(val), do: Kernel.to_string(val)
end
