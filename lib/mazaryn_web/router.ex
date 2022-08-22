defmodule MazarynWeb.Router do
  use MazarynWeb, :router

  import MazarynWeb.Plug.Session, only: [redirect_unauthorized: 2, validate_session: 2]

  pipeline :browser do
    plug(:accepts, ["html"])
    plug(:fetch_session)
    plug(:fetch_live_flash)
    plug(:put_root_layout, {MazarynWeb.LayoutView, :root})
    plug(:protect_from_forgery)
    plug(:put_secure_browser_headers)
    plug(:validate_session)
  end

  pipeline :restricted do
    plug(:browser)
    plug(:redirect_unauthorized)
  end

  pipeline :api do
    plug(:accepts, ["json"])
  end

  # pipeline :liveview do
  #   plug :ensure_user_authenticated
  #   plug :ensure_user_confirmed
  # end

  scope "/", MazarynWeb do
    pipe_through(:browser)

    get("/logout", LogoutController, :index)

    live_session :default,
      on_mount: [MazarynWeb.UserLiveAuth] do
      live("/login", AuthLive.Login)

      live("/reset", AuthLive.Reset)
      live("/signup", AuthLive.Signup)
      live("/messages/:id", ChatLive.Index)
    end

    get("/", PageController, :index)
  end

  scope "/", MazarynWeb do
    pipe_through(:restricted)
    live("/home", HomeLive.Home)
    live("/coins", CoinLive.Index)
    live("/profile", UserLive.Profile)
    live("/profile/edit", UserLive.Profile, :edit)
    live("/posts", PostLive.Index)
    live("/dashboard", DashboardLive.Index)
    live("/search", SearchLive.Index)
  end

  scope "/api" do
    pipe_through(:api)

    forward("/graphiql", Absinthe.Plug.GraphiQL,
      schema: MazarynWeb.Schema,
      interface: :simple
    )

    forward("/", Absinthe.Plug, schema: MazarynWeb.Schema)
  end

  # Other scopes may use custom stacks.

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
