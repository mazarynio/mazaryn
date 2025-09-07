defmodule MazarynWeb.Router do
  use MazarynWeb, :router

  import MazarynWeb.Plug.Session,
    only: [redirect_unauthorized: 2, validate_session: 2, check_if_admin: 2]

  import MazarynWeb.Plug.CheckAllowedUser

  @supported_locales ~w(en fa ru zh az ar es vi hi de fr it pt fil id)
  @skip_prefixes ["/assets", "/uploads", "/images", "/fonts", "/favicon.ico", "/robots.txt", "/live", "/phoenix"]

  defp localized_path?(<<"/", rest::binary>>) do
    case String.split(rest, "/", trim: true) do
      [seg | _] -> seg in @supported_locales
      _ -> false
    end
  end

  defp should_skip_redirect?(path),
    do: Enum.any?(@skip_prefixes, &String.starts_with?(path, &1))

  defp maybe_redirect_locale(conn, _opts) do
    path = conn.request_path

    cond do
      should_skip_redirect?(path) ->
        conn

      localized_path?(path) ->
        conn

      true ->
        target = "/en" <> path
        Phoenix.Controller.redirect(conn, to: target) |> Plug.Conn.halt()
    end
  end

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash

    plug :maybe_redirect_locale

    plug :put_root_layout, {MazarynWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :validate_session
    plug MazarynWeb.Plugs.SetLocale
  end

  pipeline :restricted do
    plug :browser
    plug :redirect_unauthorized
  end

  pipeline :admins do
    plug :browser
    plug :check_if_admin
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/en/api" do
    pipe_through :api

    forward "/graphiql", Absinthe.Plug.GraphiQL,
      schema: MazarynWeb.Schema,
      interface: :simple

    forward "/", Absinthe.Plug, schema: MazarynWeb.Schema
  end

  scope "/:locale", MazarynWeb do
    pipe_through :browser

    get "/logout", LogoutController, :index
    get "/confirm/:token", ConfirmAccountController, :index
    get "/2908017.txt", FileController, :serve_empty_file

    live_session :default,
      on_mount: [MazarynWeb.UserLiveAuth] do
      live "/login", AuthLive.Login
      live "/blog", BlogLive.Index
      live "/blog/mazaryn", BlogLive.Post.Mazaryn
      live "/blog/polkadot", BlogLive.Post.Polkadot
      live "/blog/kusama", BlogLive.Post.Kusama
      live "/blog/cardano", BlogLive.Post.Cardano
      live "/blog/aeternity", BlogLive.Post.Aeternity
      live "/blog/joy-of-elixir-and-phoenix", BlogLive.Post.Joyofelixirandphoenix
      live "/blog/nft", BlogLive.Post.NFT
      live "/blog/phoenix-and-absinthe", BlogLive.Post.Phoenixandabsinthe
      live "/blog/ai-nft", BlogLive.Post.AINFT
      live "/blog/blockchain", BlogLive.Post.Blockchain
      live "/reset", AuthLive.Reset
      live "/signup", AuthLive.Signup
    end

    get "/", PageController, :index
    get "/contact", PageController, :contact
    get "/about", PageController, :about
    get "/privacy-policy", PageController, :privacy
    get "/careers", PageController, :careers
    get "/865853.txt", PageController, :empty_page
  end

  scope "/:locale", MazarynWeb do
    pipe_through :restricted

    live_session :restricted,
      on_mount: [{MazarynWeb.UserLiveAuth, :restricted}, {MazarynWeb.UserLiveAuth, :default}] do
      live "/home", HomeLive.Home
      live "/approve", HomeLive.Approve
      live "/coins", CoinLive.Index
      live "/notifications", HomeLive.Notification
      live "/videos", VideoLive.Index
      live "/videos/:id", VideoLive.Show

      scope "/chats" do
        live "/", ChatsLive.Index, :index
        live "/:recipient_id", ChatsLive.Index, :index
      end

      scope "/manage" do
        pipe_through :admins
        live "/", UserLive.Manage
      end

      live "/search", SearchLive.Index
      live "/posts", PostLive.Index
      live "/dashboard", DashboardLive.Index
      live "/dashboard/hedera-wallet", DashboardLive.Wallet.HederaWallet
      live "/notifications", NotificationLive.Index
      live "/user_blog", UserBlog.Index

      live "/hashtag/:hashtag_name", HashtagLive.Index
      live "/:username", UserLive.Profile
      live "/:username/:locale", UserLive.Profile
    end
  end

  if Application.get_env(:mazaryn, :env) in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: MazarynWeb.Telemetry
    end
  end

  if Application.get_env(:mazaryn, :env) == :dev do
    scope "/dev" do
      pipe_through :browser
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
