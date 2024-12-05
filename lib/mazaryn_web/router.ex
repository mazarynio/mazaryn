defmodule MazarynWeb.Router do
  use MazarynWeb, :router

  import MazarynWeb.Plug.Session,
    only: [redirect_unauthorized: 2, validate_session: 2, check_if_admin: 2]

  import MazarynWeb.Plug.CheckAllowedUser

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, {MazarynWeb.LayoutView, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(:validate_session)
    plug(MazarynWeb.Plugs.SetLocale)
  end

  pipeline :restricted do
    plug(:browser)
    plug(:redirect_unauthorized)
  end

  pipeline :admins do
    plug(:browser)
    plug(:check_if_admin)
    # plug(:get_current_user_username)
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  # pipeline :liveview do
  #   plug :ensure_user_authenticated
  #   plug :ensure_user_confirmed
  # end

  scope "/api" do
    pipe_through(:api)

    forward("/graphiql", Absinthe.Plug.GraphiQL,
      schema: MazarynWeb.Schema,
      interface: :simple
    )

    forward("/", Absinthe.Plug, schema: MazarynWeb.Schema)
  end
  get "/", MazarynWeb.PageController, :add_locale

  scope "/:locale", MazarynWeb do
    pipe_through(:browser)

    get("/logout", LogoutController, :index)
    get("/confirm/:token", ConfirmAccountController, :index)
    get "/2908017.txt", FileController, :serve_empty_file

    live_session :default,
      on_mount: [MazarynWeb.UserLiveAuth] do
      live("/login", AuthLive.Login)
      live("/blog", BlogLive.Index)
      live("/blog/mazaryn", BlogLive.Post.Mazaryn)
      live("/blog/polkadot", BlogLive.Post.Polkadot)
      live("/blog/kusama", BlogLive.Post.Kusama)
      live("/blog/cardano", BlogLive.Post.Cardano)
      live("/blog/aeternity", BlogLive.Post.Aeternity)
      live("/blog/joy-of-elixir-and-phoenix", BlogLive.Post.Joyofelixirandphoenix)
      live("/blog/nft", BlogLive.Post.NFT)
      live("/blog/phoenix-and-absinthe", BlogLive.Post.Phoenixandabsinthe)
      live("/blog/ai-nft", BlogLive.Post.AINFT)
      live("/blog/blockchain", BlogLive.Post.Blockchain)
      live("/reset", AuthLive.Reset)
      live("/signup", AuthLive.Signup)
    end

    get("/", PageController, :index)

    get("/contact", PageController, :contact)
    get("/about", PageController, :about)
    get("/privacy-policy", PageController, :privacy)
    get("/careers", PageController, :careers)
    get("/865853.txt", PageController, :empty_page)
  end

  scope "/:locale", MazarynWeb do
    pipe_through(:restricted)

    live_session :restricted,
      on_mount: [{MazarynWeb.UserLiveAuth, :restricted}, {MazarynWeb.UserLiveAuth, :default}] do
      live("/home", HomeLive.Home)
      live("/approve", HomeLive.Approve)
      live("/coins", CoinLive.Index)
      live("/notifications", HomeLive.Notification)
      live("/videos", VideoLive.Index)
      live("/videos/:id", VideoLive.Show)

      # CHATS
      scope "/chats" do
        live("/", ChatsLive.Index, :index)
        live("/:recipient_id", ChatsLive.Index, :index)
      end

      # Manage
      scope "/manage" do
        pipe_through(:admins)
        live("/", UserLive.Manage)
      end

      # profile
      live("/search", SearchLive.Index)
      live("/posts", PostLive.Index)
      live("/dashboard", DashboardLive.Index)
      live("/dashboard/hedera-wallet", DashboardLive.Wallet.HederaWallet)
      live("/notifications", NotificationLive.Index)
      live("/user_blog", UserBlog.Index)
      # hashtags
      live "/hashtag/:hashtag_name", HashtagLive.Index
      live("/:username", UserLive.Profile)
      live("/:username/:locale", UserLive.Profile)
    end
  end
  # Other scopes may use custom stacks.
  # scope "/api", MazarynWeb do
  #   pipe_through :api
  # end

  # Enables LiveDashboard only for development
  #
  # If you want to use the LiveDashboard in production, you should put
  # it behind authentication and allow only admins to access it.
  # If your application does not have an admins-only section yet,
  # you can use Plug.BasicAuth to set up some basic authentication
  # as long as you are also using SSL (which you should anyway).
  if Mix.env() in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through(:browser)

      live_dashboard("/dashboard", metrics: MazarynWeb.Telemetry)
    end
  end

  # if Mix.env == :dev do
  #   # If using Phoenix
  #   forward "/sent_emails", Bamboo.SentEmailViewerPlug

  #   # If using Plug.Router, make sure to add the `to`
  #   forward "/sent_emails", to: Bamboo.SentEmailViewerPlug
  # end

  # Enables the Swoosh mailbox preview in development.
  #
  # Note that preview only shows emails that were sent by the same
  # node running the Phoenix server.
  if Mix.env() == :dev do
    scope "/dev" do
      pipe_through(:browser)

      forward("/mailbox", Plug.Swoosh.MailboxPreview)
    end
  end
end
