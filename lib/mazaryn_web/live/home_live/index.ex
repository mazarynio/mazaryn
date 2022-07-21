defmodule MazarynWeb.HomeLive.Index do
  use MazarynWeb, :live_view

  alias Core.PostClient, as: PostClient

  import MazarynWeb.Live.Helper, only: [signing_salt: 0]
  alias MazarynWeb.Component.SelectLive
  alias Home.Post
  require Logger

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    PostClient.start()
    post_changeset = Post.changeset(%Post{})

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_id: user_id)
      |> assign(posts: get_post())

    {:ok, socket}
  end

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    post_changeset = Post.changeset(%Post{})

    socket =
      socket
      |> assign(post_changeset: post_changeset)
      |> assign(user_id: get_user_id(session_uuid))

    {:ok, socket}
    # user = Account.User{email: get_user_id(session_uuid)}
    # {:ok, assign(socket, Account.User, user)}
  end

  @impl true
  def handle_event("messages", _param, socket) do
    random_id = "/messages/" <> "1"
    {:noreply, push_redirect(socket, to: random_id)}
  end

  # @impl true
  # def handle_event("toggle", _param, socket) do
  #   Phoenix.LiveView.JS.toggle(to: ".dropdown-menu", in: "fade-in-scale", out: "fade-out-scale")
  #   {:noreply, assign(socket, :temperature, new_temp)}
  # end
  #

  defp get_post, do: PostClient.get_posts()

  defp get_user_id(session_uuid) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        case Phoenix.Token.verify(MazarynWeb.Endpoint, signing_salt(), token, max_age: 806_400) do
          {:ok, user_id} ->
            user_id

          _ ->
            nil
        end

      _ ->
        nil
    end
  end
end
