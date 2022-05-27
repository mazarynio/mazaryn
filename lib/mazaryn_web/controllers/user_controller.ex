defmodule MazarynWeb.UserController do
  use MazarynWeb, :controller

  alias Account.Users
  alias Mazaryn.Token


  @registeration_schema %{
    username: [type: :string, length: [min: 3], required: true],
    password: [type: :string, required: true],
    email: [type: :string, required: true],
  }

  @login_schema %{
    username: [type: :string, required: true],
    password: [type: :string, required: true],
  }

  @value_schema %{
    value: [type: :string, length: [min: 3], required: true]
  }

  @follow_schema %{
    follower: [type: :string, required: true],
    following: [type: :string, required: true],
  }

  

  def get_all_user(conn, _param) do
    users = Users.list()
    render(conn, "index.json", users: users)
  end

  def register(conn, %{"username" => username, "password" => password, "email" => email}) do
    with {:ok, better_params} <- Tarams.cast(%{"username" => username, "password" => password, "email" => email}, @registeration_schema) do
     case Users.register(better_params.username, better_params.password, better_params.email) do
      {:ok, msg } ->
        IO.inspect(msg)
        render(conn, "show.json", user: %{
          id: msg.id,
          # username: msg.username,
          # password: msg.password,
          # email: msg.email,
          token: Token.generate_and_sign!(msg),
        })
      {:error, msg} ->
        IO.inspect(msg)
        render(conn, "error.json", message: msg)
     end
    else
      # return params error
      {:error, error} -> conn |> put_status(:bad_request) |> render("error.json", message: error)
    end
  end

  def login(conn, %{"username" => username, "password" => password}) do
    with {:ok, better_params} <- Tarams.cast(%{"username" => username, "password" => password}, @login_schema) do
     case Users.login(better_params.username, better_params.password) do
      {:ok, msg } ->
        IO.inspect(msg)
        render(conn, "show.json", user: %{
          id: msg.id,
          # username: msg.username,
          # password: msg.password,
          # email: msg.email,
          token: Token.generate_and_sign!(msg),
        })
      {:error, msg} ->
        IO.inspect(msg)
        render(conn, "error.json", message: msg)
     end
    else
      # return params error
      {:error, error} -> conn |> put_status(:bad_request) |> render("error.json", message: error)
    end
  end
  
  def follow_user(conn, %{"follower" => follower, "following" => following}) do
    with {:ok, better_params} <- Tarams.cast(%{"follower" => follower, "following" => following}, @follow_schema) do
     case Users.follow(better_params.follower, better_params.following) do
      {:ok, msg} ->
        IO.inspect(msg)
        render(conn, "success.json", message: "following")
      {:error, msg} ->
        IO.inspect(msg)
        render(conn, "error.json", message: msg)
     end
    else
      # return params error
      {:error, error} -> conn |> put_status(:bad_request) |> render("error.json", message: error)
    end
  end

  def get_user_by_username(conn, %{"username" => username}) do
      with {:ok, better_params} <- Tarams.cast(%{"value" => username}, @value_schema) do
        case Users.one_by_username(better_params.value) do
        {:ok, msg } ->
          IO.inspect(msg)
          render(conn, "show.json", user: %{
            id: msg.id,
            username: msg.username,
            following: msg.following,
            follower: msg.follower,
            saved_posts: msg.saved_posts,
            other_info: msg.other_info,
            date_created: msg.date_created,
            date_updated: msg.date_updated,
          })
        {:error, msg} ->
          IO.inspect(msg)
          render(conn, "error.json", message: msg)
        end
      else
        # return params error
        {:error, error} -> conn |> put_status(:bad_request) |> render("error.json", message: error)
      end
  end

  def get_user_by_email(conn, %{"email" => email}) do
    with {:ok, better_params} <- Tarams.cast(%{"value" => email}, @value_schema) do
      case Users.one_by_email(better_params.value) do
      {:ok, msg } ->
        IO.inspect(msg)
        render(conn, "show.json", user: %{
          id: msg.id,
          username: msg.username,
          following: msg.following,
          follower: msg.follower,
          saved_posts: msg.saved_posts,
          other_info: msg.other_info,
          date_created: msg.date_created,
          date_updated: msg.date_updated,
        })
      {:error, msg} ->
        IO.inspect(msg)
        render(conn, "error.json", message: msg)
      end
    else
      # return params error
      {:error, error} -> conn |> put_status(:bad_request) |> render("error.json", message: error)
    end
  end

  # def get_followers(conn, %{"username" => username}) do
  #   with {:ok, better_params} <- Tarams.cast(%{"value" => username}, @value_schema) do
  #     case UserClient.follow_user(better_params.value) do
  #      {:ok, msg } ->
  #       #  IO.inspect(msg)
  #        render(conn, "show.json", user: %{
  #          id: msg.id,
  #          username: msg.username,
  #          password: msg.password,
  #          token: Token.generate_and_sign!(msg),
  #        })
  #      {:error, msg} ->
  #       #  IO.inspect(msg)
  #        render(conn, "error.json", message: msg)
  #     end
  #    end
  # end

  
end

