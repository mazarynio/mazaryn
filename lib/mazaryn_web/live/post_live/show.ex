defmodule MazarynWeb.PostLive.Show do
  use MazarynWeb, :live_view

  import Phoenix.Component

  alias Mazaryn.Posts
  alias Core.PostClient
  alias Mazaryn.Schema.Post
  alias MazarynWeb.HomeLive.CommentHandler
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, _session, socket) do
    current_user =
      case socket.assigns[:current_user] do
        %{} = user ->
          user

        email when is_binary(email) ->
          case Users.one_by_email(email) do
            {:ok, user} -> user
            _ -> nil
          end

        _ ->
          nil
      end

    if current_user do
      {:ok, assign(socket, :current_user, current_user)}
    else
      Logger.error("❌ Could not load current_user in PostLive.Show")

      {:ok,
       socket |> put_flash(:error, "Authentication required") |> push_navigate(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(%{"post_id" => post_id}, _url, socket) do
    Logger.info("📄 Loading single post view for post_id: #{post_id}")

    case get_post_by_id(post_id) do
      {:ok, post} ->
        comments = CommentHandler.get_comments_with_content(post.id)

        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, comments)
         |> assign(:page_title, page_title(socket.assigns.live_action, post))
         |> assign(:locale, socket.assigns[:locale] || "en")}

      {:error, reason} ->
        Logger.error("❌ Failed to load post: #{inspect(reason)}")

        {:noreply,
         socket
         |> put_flash(:error, "Post not found")
         |> push_navigate(to: "/#{socket.assigns[:locale] || "en"}/home")}
    end
  end

  def handle_params(%{"id" => id}, _url, socket) do
    case Posts.one_by_id(id) do
      {:ok, post} ->
        comments = CommentHandler.get_comments_with_content(post.id)

        {:noreply,
         socket
         |> assign(:post, post)
         |> assign(:comments, comments)
         |> assign(:page_title, page_title(socket.assigns.live_action, post))
         |> assign(:locale, socket.assigns[:locale] || "en")}

      {:error, _reason} ->
        {:noreply,
         socket
         |> put_flash(:error, "Post not found")
         |> push_navigate(to: "/#{socket.assigns[:locale] || "en"}/home")}
    end
  end

  def handle_params(_params, _url, socket) do
    {:noreply,
     socket
     |> assign(:page_title, "Post")
     |> assign(:locale, socket.assigns[:locale] || "en")}
  end

  defp get_post_by_id(post_id) do
    try do
      post_id_charlist =
        if is_binary(post_id) do
          String.to_charlist(post_id)
        else
          post_id
        end

      case PostClient.get_by_id(post_id_charlist) do
        post_tuple when is_tuple(post_tuple) ->
          case Post.erl_changeset(post_tuple) |> Post.build() do
            {:ok, post} ->
              Logger.info("✅ Successfully loaded post #{post.id}")
              {:ok, post}

            {:error, reason} ->
              Logger.error("❌ Failed to build post: #{inspect(reason)}")
              {:error, :build_failed}
          end

        nil ->
          Logger.error("❌ Post not found in database")
          {:error, :not_found}

        other ->
          Logger.error("❌ Unexpected result from PostClient: #{inspect(other)}")
          {:error, :unexpected_result}
      end
    rescue
      e ->
        Logger.error("❌ Exception loading post: #{inspect(e)}")
        {:error, :exception}
    end
  end

  defp page_title(:show, post), do: "Post by #{post.author}"
  defp page_title(:edit, _post), do: "Edit Post"
  defp page_title(nil, post), do: "Post by #{post.author}"
  defp page_title(_, _), do: "Post"
end
