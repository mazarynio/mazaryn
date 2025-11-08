defmodule MazarynWeb.HomeLive.PostComponent.Helper do
  use MazarynWeb, :verified_routes
  alias MazarynWeb.Router.Helpers, as: Routes

  alias Account.Users
  alias Core.{UserClient, PostClient}
  alias Mazaryn.Schema.{Comment, Post}
  alias MazarynWeb.HomeLive.{CommentHandler, IpnsManager}
  alias Home.Like

  @content_cache :post_content_cache
  @batch_size 5
  @cache_ttl %{content: 600, likes: 300, ipns: 300}
  @timeouts %{content: 800, likes: 300, ipns: 200, comment: 2000, post_update: 1500}

  def fetch_likers(post_id_charlist) when is_list(post_id_charlist) do
    likes = PostClient.get_likes(post_id_charlist)

    IO.puts(
      "👍 fetch_likers: #{length(likes)} like tuples for #{List.to_string(post_id_charlist)}"
    )

    likes
    |> Enum.map(&extract_user_from_like/1)
    |> Enum.reject(&is_nil/1)
    |> Enum.map(&get_user_details/1)
  end

  def extract_user_from_like({:like, _like_id, _post_id, :undefined, user_id, _ts, _meta}) do
    cond do
      is_list(user_id) -> List.to_string(user_id)
      is_binary(user_id) -> user_id
      true -> nil
    end
  end

  def extract_user_from_like(other) do
    IO.puts("⚠️ Unexpected like tuple: #{inspect(other)}")
    nil
  end

  def get_user_details(user_id) when is_list(user_id),
    do: get_user_details(List.to_string(user_id))

  def get_user_details(user_id) when is_binary(user_id) do
    username =
      try do
        case Core.UserClient.get_user_by_id(String.to_charlist(user_id)) do
          tuple when is_tuple(tuple) and tuple_size(tuple) >= 9 ->
            elem(tuple, 8) |> to_string()

          other ->
            IO.puts("⚠️ get_user_by_id unexpected shape for #{user_id}: #{inspect(other)}")
            user_id
        end
      rescue
        e ->
          IO.puts("❌ get_user_by_id failed for #{user_id}: #{inspect(e)}")
          user_id
      end

    case Users.one_by_username(username) do
      {:ok, user} ->
        user

      miss ->
        IO.puts("ℹ️ No DB record for #{username} (#{inspect(miss)}), using lightweight map")
        %{username: username, verified: false, level: nil, country: nil, bio: nil}
    end
  end

  def get_user_details(_), do: nil

  # ============================ TEMPLATE HELPERS ===============================
  def verified?(author) do
    case Users.one_by_username(author) do
      {:ok, user} -> Map.get(user, :verified, false)
      _ -> false
    end
  end

  def one_of_comment_likes?(user_id, comment_id) do
    comment_id
    |> PostClient.get_comment_likes()
    |> Enum.map(fn like -> like |> Like.erl_changeset() |> Like.build() |> elem(1) end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  def comment_like_color(user_id, comment_id) do
    if one_of_comment_likes?(user_id, comment_id), do: "text-blue-500", else: "text-gray-500"
  end

  def get_user_avatar(author) do
    case Account.Users.one_by_username(author) do
      {:ok, user} ->
        if user.avatar_url do
          Mazaryn.config([:media, :ipfs_gateway]) <> user.avatar_url
        else
          ~p"/images/default-user.svg"
        end

      {:error, _} ->
        ""
    end
  end

  def maybe_close_likes_modal(socket) do
    if socket.assigns[:show_likes_modal] do
      Phoenix.Component.assign(socket, %{
        show_likes_modal: false,
        liked_users: [],
        likes_loading: false
      })
    else
      socket
    end
  end

  # ====================== FOLLOW/LIKE/COMMENT HELPERS ==========================
  defp one_of_following?(id, username) do
    id
    |> UserClient.get_following()
    |> Enum.any?(&(&1 == username))
  end

  def follow_text(id, username),
    do: if(one_of_following?(id, username), do: "Unfollow", else: "Follow")

  def follow_event(id, username),
    do: if(one_of_following?(id, username), do: "unfollow_user", else: "follow_user")

  def one_of_likes?(user_id, post_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(fn like -> like |> Like.erl_changeset() |> Like.build() |> elem(1) end)
    |> Enum.any?(&(&1.user_id == user_id))
  end

  def like_icon(user_id, post_id),
    do: if(one_of_likes?(user_id, post_id), do: "hand-thumb-down", else: "hand-thumb-up")

  def like_event(user_id, post_id),
    do: if(one_of_likes?(user_id, post_id), do: "unlike_post", else: "like_post")

  def find_user_like(post_id, user_id) do
    post_id
    |> PostClient.get_likes()
    |> Enum.map(&(&1 |> Like.erl_changeset() |> Like.build() |> elem(1)))
    |> Enum.find(&(&1.user_id == user_id))
  end

  def rebuild_post(post_id) do
    {:ok, post} =
      PostClient.get_by_id(post_id)
      |> Mazaryn.Schema.Post.erl_changeset()
      |> Mazaryn.Schema.Post.build()

    post
  end

  # ============================ CONTENT PROCESSING =============================
  def activate_content_characters(post, socket) do
    process_start = :erlang.system_time(:millisecond)
    IO.puts("📝 Starting content processing for post #{post.id}")

    result =
      try do
        content_str = get_processed_content(post)
        process_content_string(content_str, socket)
      catch
        type, reason ->
          IO.puts("❌ Unexpected error in content processing: #{inspect({type, reason})}")
          "Error processing content" |> Earmark.as_html!(compact_output: true) |> apply_styles()
      end

    process_end = :erlang.system_time(:millisecond)
    IO.puts("📝 Content processing completed in #{process_end - process_start}ms")
    result
  end

  def get_image_url(post_id) do
    try do
      case PostClient.get_media(to_charlist(post_id)) do
        media_binary when is_binary(media_binary) and byte_size(media_binary) > 0 ->
          PostClient.display_real_media(media_binary)

        _ ->
          nil
      end
    rescue
      _ -> nil
    end
  end

  defp get_processed_content(post) do
    case Core.PostClient.get_post_content_by_id(post.id) do
      nil -> process_post_content(post.content)
      ipfs_content -> process_ipfs_content(ipfs_content)
    end
  end

  defp get_processed_content(post) do
    case Core.PostClient.get_post_content_by_id(post.id) do
      nil -> process_post_content(post.content)
      ipfs_content -> process_ipfs_content(ipfs_content)
    end
  end

  defp process_ipfs_content(ipfs_content) do
    cond do
      is_binary(ipfs_content) -> ipfs_content
      is_list(ipfs_content) -> List.to_string(ipfs_content)
      true -> "No content available"
    end
  end

  defp process_post_content(content) do
    case content do
      content when is_binary(content) -> content
      content when is_list(content) -> List.to_string(content)
      _ -> "No content available"
    end
  end

  defp process_content_string(content_str, socket) do
    if content_str == "" do
      "No content available"
    else
      content_str
      |> String.split()
      |> Enum.map(&process_content_token(&1, socket))
      |> Enum.join(" ")
      |> Earmark.as_html!(compact_output: true)
      |> apply_styles()
    end
  end

  defp apply_styles(html) do
    html
    |> String.replace("<a", "<a class=\"text-blue-500\"")
    |> String.replace("</a>", "</a>")
  end

  defp process_content_token(token, socket) do
    patterns = %{
      mention: ~r/@\S[a-zA-Z]*/,
      hashtag: ~r/#\S[a-zA-Z]*/,
      url: ~r/([\w+]+\:\/\/)?([\w\d-]+\.)*[\w-]+[\.\:]\w+([\/\?\=\&\#\.]?[\w-]+)*\/?/
    }

    matches = Enum.into(patterns, %{}, fn {type, regex} -> {type, check_regex(token, regex)} end)

    case matches do
      %{mention: [[mention]], hashtag: [], url: []} -> activate_mention_only(mention, socket)
      %{mention: [], hashtag: [[hashtag]], url: []} -> activate_hashtag_only(hashtag, socket)
      %{mention: [], hashtag: [], url: [[url | _]]} -> activate_url_only(url)
      _ -> escape_char(token)
    end
  end

  defp activate_hashtag_only(hashtag, socket) do
    tag =
      hashtag
      |> String.trim_leading("#")
      |> String.downcase()

    locale = Gettext.get_locale(MazarynWeb.Gettext)
    path = Routes.live_path(socket, MazarynWeb.HashtagLive.Index, locale, tag)

    "[##{tag}](#{path})"
  end

  defp activate_mention_only(mention, socket) do
    username = String.trim_leading(mention, "@")
    path = create_user_path(username, socket)
    "[@#{username}](#{path})"
  end

  defp activate_url_only("http" <> _ = url), do: "[#{url}](#{url})"
  defp activate_url_only(url), do: "[#{url}](https://#{url})"

  defp escape_char("#"), do: "#"
  defp escape_char(con), do: con

  defp check_regex(con, regex) do
    cond do
      con == "#" -> "#"
      con == "@" -> "@"
      true -> Regex.scan(regex, con)
    end
  end

  defp create_user_path(username, socket) do
    case Users.one_by_username(username) do
      :ok ->
        "#"

      {:ok, _user} ->
        locale = Gettext.get_locale(MazarynWeb.Gettext)
        Routes.live_path(socket, MazarynWeb.UserLive.Profile, locale, username)
    end
  end

  # ============================== MEDIA HELPERS ================================
  def post_has_media?(post_id) do
    try do
      case PostClient.get_media(to_charlist(post_id)) do
        media_binary when is_binary(media_binary) and byte_size(media_binary) > 0 ->
          true

        _ ->
          false
      end
    rescue
      _ -> false
    catch
      _, _ -> false
    end
  end

  # =========================== UPLOADS / CHANGES ===============================
  def setup_uploads(socket) do
    Phoenix.LiveView.allow_upload(socket, :media, accept: ~w(.png .jpg .jpeg), max_entries: 2)
  end

  def prepare_changesets do
    %{
      changeset: Comment.changeset(%Comment{}),
      update_comment_changeset: Comment.changeset(%Comment{}),
      update_post_changeset: Post.changeset(%Post{})
    }
  end

  # ======================= BATCH POST PROCESSING ===============================
  def process_posts_optimized(list_of_assigns, changesets) do
    list_of_assigns
    |> Enum.chunk_every(@batch_size)
    |> Enum.flat_map(&process_batch(&1, changesets))
  end

  defp process_batch(batch, changesets) do
    IO.puts("Processing batch of #{length(batch)} posts")

    batch
    |> Task.async_stream(&process_single_post(&1, changesets),
      max_concurrency: 4,
      timeout: 5_000,
      on_timeout: :kill_task
    )
    |> Enum.map(&handle_task_result(&1, batch))
  end

  defp handle_task_result({:ok, result}, _batch), do: result

  defp handle_task_result({:exit, reason}, batch) do
    IO.puts("❌ Task failed with reason: #{inspect(reason)}")
    {assigns, socket} = hd(batch)
    Phoenix.Component.assign(socket, basic_assigns(assigns))
  end

  defp process_single_post({assigns, socket}, changesets) do
    try do
      post_start = :erlang.system_time(:millisecond)
      IO.puts("--- Processing post #{assigns.post.id} ---")

      tasks = create_async_tasks(assigns.post.id)
      results = await_tasks_with_fallbacks(tasks, assigns.post.id)
      final_assigns = build_final_assigns(assigns, results, changesets)

      post_end = :erlang.system_time(:millisecond)
      IO.puts("✅ Post #{assigns.post.id} completed in #{post_end - post_start}ms")

      Phoenix.Component.assign(socket, final_assigns)
    rescue
      e ->
        IO.puts("❌ Error processing post #{assigns.post.id}: #{inspect(e)}")
        Phoenix.Component.assign(socket, basic_assigns(assigns))
    end
  end

  defp create_async_tasks(post_id) do
    task_definitions = [
      {:comments, fn -> CommentHandler.get_comments_with_content_optimized(post_id) end},
      {:ipns, fn -> IpnsManager.get_ipns_fast(post_id) end},
      {:likes, fn -> get_likes_count_cached(post_id) end},
      {:content, fn -> get_post_content_optimized(post_id) end}
    ]

    Enum.into(task_definitions, %{}, fn {name, func} ->
      {name,
       Task.async(fn ->
         start = :erlang.system_time(:millisecond)
         result = func.()
         duration = :erlang.system_time(:millisecond) - start

         IO.puts(
           "#{task_emoji(name)} #{String.capitalize(to_string(name))} fetch for post #{post_id} took #{duration}ms"
         )

         result
       end)}
    end)
  end

  defp task_emoji(:comments), do: "📝"
  defp task_emoji(:ipns), do: "🔗"
  defp task_emoji(:likes), do: "📊"
  defp task_emoji(:content), do: "📄"

  defp await_tasks_with_fallbacks(tasks, post_id) do
    fallbacks = %{
      comments: [],
      ipns_id: nil,
      likes_count: 0,
      post_content_cached: "Content loading..."
    }

    timeouts = %{comments: 2000, ipns_id: 200, likes_count: 500, post_content_cached: 1500}

    Enum.into(tasks, %{}, fn {key, task} ->
      timeout = Map.get(timeouts, key, 1000)
      fallback = Map.get(fallbacks, key, nil)
      result_key = if key == :ipns, do: :ipns_id, else: key
      result_key = if key == :likes, do: :likes_count, else: result_key
      result_key = if key == :content, do: :post_content_cached, else: result_key

      {result_key, await_with_fallback(task, timeout, fallback, to_string(key), post_id)}
    end)
  end

  defp build_final_assigns(assigns, results, changesets) do
    base_assigns = %{
      follow_event: follow_event(assigns.current_user.id, assigns.post.author),
      follow_text: follow_text(assigns.current_user.id, assigns.post.author),
      like_icon: like_icon(assigns.current_user.id, assigns.post.id),
      like_event: like_event(assigns.current_user.id, assigns.post.id),
      report_action: false,
      like_action: false,
      is_liked: false,
      supported_translation_langs: Mazaryn.Translator.supported_targets(),
      repost_count: get_repost_count(assigns.post.id),
      user_has_reposted: has_user_reposted?(assigns.current_user.id, assigns.post.id)
    }

    assigns |> Map.merge(base_assigns) |> Map.merge(results) |> Map.merge(changesets)
  end

  defp basic_assigns(assigns) do
    %{
      follow_event: follow_event(assigns.current_user.id, assigns.post.author),
      follow_text: follow_text(assigns.current_user.id, assigns.post.author),
      like_icon: like_icon(assigns.current_user.id, assigns.post.id),
      like_event: like_event(assigns.current_user.id, assigns.post.id),
      changeset: Comment.changeset(%Comment{}),
      update_comment_changeset: Comment.changeset(%Comment{}),
      update_post_changeset: Post.changeset(%Post{}),
      comments: [],
      report_action: false,
      like_action: false,
      is_liked: false,
      ipns_id: nil,
      likes_count: 0,
      post_content_cached: "Content loading..."
    }
  end

  defp await_with_fallback(task, timeout, fallback, task_name, post_id) do
    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, result} ->
        IO.puts("✅ #{task_name} task completed for post #{post_id}")
        result

      nil ->
        IO.puts("⚠️ #{task_name} task timeout (#{timeout}ms) for post #{post_id} - using fallback")
        fallback
    end
  end

  # =========================== CACHE & CONTENT FETCH ============================
  def ensure_cache_table do
    unless :ets.whereis(@content_cache) != :undefined do
      :ets.new(@content_cache, [:set, :public, :named_table, {:read_concurrency, true}])
    end
  end

  def get_post_content_optimized(post_id) do
    cache_key = {:post_content, post_id}

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, content, timestamp}] ->
        if cache_valid?(timestamp, @cache_ttl.content) do
          IO.puts("📦 Content Cache HIT for post #{post_id}")
          content
        else
          spawn_content_refresh(post_id)
          content
        end

      [] ->
        case get_post_content_with_timeout(post_id, @timeouts.content) do
          {:ok, content} ->
            cache_content(cache_key, content)
            content

          _ ->
            "Content loading..."
        end
    end
  end

  def get_likes_count_cached(post_id) do
    cache_key = {:likes_count, post_id}

    case :ets.lookup(@content_cache, cache_key) do
      [{^cache_key, count, timestamp}] ->
        if cache_valid?(timestamp, @cache_ttl.likes) do
          IO.puts("📦 Likes Cache HIT for post #{post_id}")
          count
        else
          spawn_likes_refresh(post_id)
          count
        end

      [] ->
        case get_likes_count_with_timeout(post_id, @timeouts.likes) do
          count when is_integer(count) ->
            cache_likes_count(cache_key, count)
            count

          _ ->
            0
        end
    end
  end

  defp cache_valid?(timestamp, ttl_seconds) do
    age = :erlang.system_time(:second) - timestamp
    age < ttl_seconds
  end

  defp cache_content(cache_key, content) do
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {cache_key, content, timestamp})
  end

  defp cache_likes_count(cache_key, count) do
    timestamp = :erlang.system_time(:second)
    :ets.insert(@content_cache, {cache_key, count, timestamp})
  end

  defp spawn_content_refresh(post_id) do
    Task.start(fn ->
      case get_post_content_with_timeout(post_id, @timeouts.post_update) do
        {:ok, content} -> cache_content({:post_content, post_id}, content)
        _ -> :ok
      end
    end)
  end

  defp spawn_likes_refresh(post_id) do
    Task.start(fn ->
      try do
        count = get_likes_count(post_id)
        cache_likes_count({:likes_count, post_id}, count)
      rescue
        _ -> :ok
      end
    end)
  end

  def warm_likes_cache(recent_post_ids) do
    Task.start(fn ->
      recent_post_ids
      |> Enum.take(10)
      |> Enum.each(fn post_id ->
        spawn_likes_refresh(post_id)
        Process.sleep(100)
      end)
    end)
  end

  def clear_content_cache({type, id}) do
    :ets.delete(@content_cache, {type, id})
  end

  def clear_content_cache({type, id1, id2}) do
    :ets.delete(@content_cache, {type, id1, id2})
  end

  def clear_content_cache(key), do: :ets.delete(@content_cache, key)

  def clear_all_cache_for_post(post_id) do
    [:post, :ipns, :likes_count, :post_content]
    |> Enum.each(fn type -> :ets.delete(@content_cache, {type, post_id}) end)

    IpnsManager.clear_cache(post_id)
  end

  def get_post_content_with_timeout(post_id, timeout) do
    task =
      Task.async(fn ->
        try do
          case Core.PostClient.get_post_content_by_id(post_id) do
            content when is_binary(content) and content != "" ->
              {:ok, content}

            content when is_list(content) ->
              case List.to_string(content) do
                "" -> {:ok, "No content available"}
                str -> {:ok, str}
              end

            _ ->
              {:ok, "No content available"}
          end
        catch
          type, reason ->
            IO.puts("❌ Error fetching post content: #{inspect({type, reason})}")
            {:error, {type, reason}}
        end
      end)

    case Task.yield(task, timeout) || Task.shutdown(task) do
      {:ok, result} -> result
      nil -> {:timeout, "Timeout after #{timeout}ms"}
    end
  end

  def get_repost_count(post_id) do
    try do
      post_id |> to_charlist() |> Core.RepostClient.get_repost_count()
    rescue
      _ -> 0
    end
  end

  def has_user_reposted?(user_id, post_id) do
    try do
      Core.RepostClient.is_reposted_by_user(
        String.to_charlist(user_id),
        to_charlist(post_id)
      )
    rescue
      _ -> false
    end
  end

  def get_repost_icon(user_id, post_id) do
    if has_user_reposted?(user_id, post_id), do: "reposted", else: "repost"
  end

  def format_repost_display(post) do
    if post.is_repost do
      case post.repost_type do
        "simple" ->
          %{
            type: :simple,
            show_original: true,
            show_comment: false
          }

        "with_comment" ->
          %{
            type: :with_comment,
            show_original: true,
            show_comment: true,
            comment: get_repost_comment(post.id)
          }

        _ ->
          %{type: :unknown, show_original: false, show_comment: false}
      end
    else
      %{type: :regular, show_original: false, show_comment: false}
    end
  end

  defp get_repost_comment(post_id) do
    try do
      case Core.PostClient.get_repost_comment(to_charlist(post_id)) do
        {:error, _} -> nil
        comment when is_binary(comment) -> comment
        comment when is_list(comment) -> List.to_string(comment)
        _ -> nil
      end
    rescue
      _ -> nil
    end
  end

  def get_likes_count_with_timeout(post_id, timeout) do
    task = Task.async(fn -> get_likes_count(post_id) end)

    case Task.yield(task, timeout) || Task.shutdown(task, :brutal_kill) do
      {:ok, count} -> count
      nil -> 0
    end
  end

  def get_likes_count(post_id) do
    try do
      post_id |> to_charlist() |> PostClient.get_likes() |> Enum.count()
    rescue
      e ->
        IO.puts("❌ Error getting likes count: #{inspect(e)}")
        0
    end
  end

  def get_repost_count(post_id) do
    try do
      post_id |> to_charlist() |> Core.RepostClient.get_repost_count()
    rescue
      _ -> 0
    end
  end

  def has_user_reposted?(user_id, post_id) do
    try do
      Core.RepostClient.is_reposted_by_user(
        String.to_charlist(user_id),
        to_charlist(post_id)
      )
    rescue
      _ -> false
    end
  end

  def get_repost_icon(user_id, post_id) do
    if has_user_reposted?(user_id, post_id), do: "reposted", else: "repost"
  end

  def format_repost_display(post) do
    if post.is_repost do
      case post.repost_type do
        "simple" ->
          %{
            type: :simple,
            show_original: true,
            show_comment: false
          }

        "with_comment" ->
          %{
            type: :with_comment,
            show_original: true,
            show_comment: true,
            comment: get_repost_comment(post.id)
          }

        _ ->
          %{type: :unknown, show_original: false, show_comment: false}
      end
    else
      %{type: :regular, show_original: false, show_comment: false}
    end
  end

  defp get_repost_comment(post_id) do
    try do
      case Core.PostClient.get_repost_comment(to_charlist(post_id)) do
        {:error, _} -> nil
        comment when is_binary(comment) -> comment
        comment when is_list(comment) -> List.to_string(comment)
        _ -> nil
      end
    rescue
      _ -> nil
    end
  end

  def is_repost?(post) do
    Map.get(post, :is_repost, false) == true
  end

  def get_original_post(post) do
    if is_repost?(post) and post.original_post_id do
      case Core.PostClient.get_by_id(to_charlist(post.original_post_id)) do
        original when is_tuple(original) ->
          case Mazaryn.Schema.Post.erl_changeset(original) |> Mazaryn.Schema.Post.build() do
            {:ok, original_post} -> {:ok, original_post}
            _ -> {:error, :invalid_original}
          end

        _ ->
          {:error, :not_found}
      end
    else
      {:error, :not_a_repost}
    end
  end

  def get_repost_comment_content(post) do
    cond do
      not Map.get(post, :is_repost, false) ->
        nil

      Map.get(post, :repost_type) == "simple" ->
        nil

      Map.get(post, :repost_type) == "with_comment" ->
        case Core.RepostClient.get_repost_comment_content(post.id) do
          nil ->
            nil

          "" ->
            nil

          content when is_binary(content) ->
            trimmed = String.trim(content)
            if trimmed == "", do: nil, else: trimmed
        end

      true ->
        nil
    end
  end
end
