defmodule MazarynWeb.Router do
  use MazarynWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, {MazarynWeb.LayoutView, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  # pipeline :liveview do
  #   plug :ensure_user_authenticated
  #   plug :ensure_user_confirmed
  # end

  scope "/", MazarynWeb do
    pipe_through :browser

    live_session :default,
    on_mount: [MazarynWeb.UserLiveAuth] do
      live "/login", AuthLive.Login
      live "/home", HomeLive.Index
      live "/reset", AuthLive.Reset
      live "/signup", AuthLive.Signup
    end

    get "/", PageController, :index


  end

  # Other scopes may use custom stacks.
  scope "/api/v1", MazarynWeb do
    pipe_through :api

    #User
    get "/users", UserController, :get_all_user
    #get "/user/:username", UserController, :get_user_by_username
    #get "/user/:email", UserController, :get_user_by_email
    # get "/user/:username", UserController, :get_followers
    get "/user/follow", UserController, :follow_user

    #Authentication
    post "/auth/user/register", UserController, :register
    post "/auth/user/login", UserController, :login



    #Post
    post "/create/post", UserController, :create_post



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
      pipe_through :browser

      live_dashboard "/dashboard", metrics: MazarynWeb.Telemetry
    end
  end

  # Enables the Swoosh mailbox preview in development.
  #
  # Note that preview only shows emails that were sent by the same
  # node running the Phoenix server.
  if Mix.env() == :dev do
    scope "/dev" do
      pipe_through :browser

      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
