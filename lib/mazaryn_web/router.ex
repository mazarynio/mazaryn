defmodule MazarynWeb.Router do
  use MazarynWeb, :router

  import MazarynWeb.Plug.Session,
    only: [redirect_unauthorized: 2, validate_session: 2, check_if_admin: 2]

  import MazarynWeb.Plug.CheckAllowedUser

  @env Application.get_env(:mazaryn, :env)

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
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  pipeline :api_binary do
    plug(:accepts, ["json", "zip", "octet-stream"])
  end

  scope "/api", MazarynWeb do
    pipe_through(:api_binary)

    get("/datasets/:dataset_id/download", DatasetDownloadController, :download)
  end

  scope "/en/api" do
    pipe_through(:api)

    forward("/graphiql", Absinthe.Plug.GraphiQL,
      schema: MazarynWeb.Schema,
      interface: :simple
    )

    forward("/", Absinthe.Plug, schema: MazarynWeb.Schema)
  end

  get("/", MazarynWeb.PageController, :add_locale)

  scope "/:locale", MazarynWeb do
    pipe_through(:browser)

    get("/logout", LogoutController, :index)
    get("/confirm/:token", ConfirmAccountController, :index)
    get("/verify-email/:token", EmailVerificationController, :verify, as: :email_verification)
    get("/2908017.txt", FileController, :serve_empty_file)
    get("/ipfs/:hash", IpfsController, :show)

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

    get("/downloads/:id/file", DownloadController, :download_file)
    get("/ai/datasets/:dataset_id/zip", DatasetZipController, :serve)

    live_session :restricted,
      on_mount: [{MazarynWeb.UserLiveAuth, :restricted}, {MazarynWeb.UserLiveAuth, :default}] do
      live("/home", HomeLive.Home)
      live("/post/:post_id", PostLive.Show)
      live("/dashboard", DashboardLive.Dashboard)
      live("/approve", HomeLive.Approve)
      live("/coins", CoinLive.Index)
      live("/notifications", HomeLive.Notification)

      live("/videos", MediaLive.Video.Index, :index)
      live("/videos/upload", MediaLive.Video.Upload, :upload)
      live("/videos/my-videos", MediaLive.Video.MyVideos, :my_videos)
      live("/videos/:id", MediaLive.Video.Show, :show)

      live("/ai", AiLive.Index)
      live("/ai/datasets", AiLive.Datasets, :index)
      live("/ai/datasets/new", AiLive.DatasetNew, :new)
      live("/ai/datasets/:id", AiLive.DatasetShow, :show)
      live("/ai/datasets/:id/edit", AiLive.DatasetEdit, :edit)
      live("/ai/datasets/:id/versions", AiLive.DatasetVersions, :versions)
      live("/ai/datasets/:id/collaborators", AiLive.DatasetCollaborators, :collaborators)
      live("/ai/competitions", AiLive.Competitions, :index)
      live("/ai/competitions/new", AiLive.CompetitionNew, :new)
      live("/ai/competitions/:id", AiLive.CompetitionShow, :show)
      live("/ai/competitions/:id/edit", AiLive.CompetitionEdit, :edit)
      live("/ai/competitions/:id/leaderboard", AiLive.CompetitionLeaderboard, :leaderboard)
      live("/ai/competitions/:id/submit", AiLive.CompetitionSubmit, :submit)
      live("/ai/competitions/:id/teams", AiLive.CompetitionTeams, :teams)

      live("/ai/notebooks", AiLive.Notebooks, :index)
      live("/ai/notebooks/:id", AiLive.NotebookShow, :show)

      live("/ai/models", AiLive.Models, :index)
      live("/ai/models/new", AiLive.ModelNew, :new)
      live("/ai/models/:id", AiLive.ModelShow, :show)
      live("/ai/models/:id/edit", AiLive.ModelEdit, :edit)
      live("/ai/models/:id/versions", AiLive.ModelVersions, :versions)
      live("/ai/models/:id/deploy", AiLive.ModelDeploy, :deploy)
      live("/ai/discussions", AiLive.Discussions, :index)
      live("/ai/discussions/new", AiLive.DiscussionNew, :new)
      live("/ai/discussions/:id", AiLive.DiscussionShow, :show)

      live("/downloads", DownloadManagerLive.Index, :index)

      scope "/chats" do
        live("/", ChatsLive.Index, :index)
        live("/:recipient_id", ChatsLive.Index, :index)
      end

      scope "/manage" do
        pipe_through(:admins)
        live("/", UserLive.Manage)
      end

      live("/search", SearchLive.Index)
      live("/posts", PostLive.Index)
      live("/dashboard", DashboardLive.Index)
      live("/dashboard/hedera-wallet", DashboardLive.Wallet.HederaWallet)
      live("/notifications", NotificationLive.Index)
      live("/user_blog", UserBlog.Index)
      live("/hashtag/:hashtag_name", HashtagLive.Index)
      live("/:username", UserLive.Profile)
      live("/:username/:locale", UserLive.Profile)
    end
  end

  if @env in [:dev, :test] do
    import Phoenix.LiveDashboard.Router

    scope "/" do
      pipe_through(:browser)
      live_dashboard("/dashboard", metrics: MazarynWeb.Telemetry)
    end
  end

  if @env == :dev do
    scope "/dev" do
      pipe_through(:browser)
      forward("/mailbox", Plug.Swoosh.MailboxPreview)
    end
  end
end
