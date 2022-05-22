defmodule MazarynWeb.UserController do
  use MazarynWeb, :controller

  alias Mazaryn.Core.UserClient
  alias Mazaryn.Core.EventClient
  alias Mazaryn.Token


  @registeration_schema %{
    username: [type: :string, length: [min: 3], required: true],
    password: [type: :string, length: [min: 8], required: true]
  }

  @username_schema %{
    username: [type: :string, length: [min: 3], required: true]
  }

  @content %{
    content: [type: :string, length: [min: 5], required: true]
  }

  def get_all_user(conn, _param) do
    users = UserClient.getting_users()
    render(conn, "index.json", users: users)
  end

  def register(conn, %{"username" => username, "password" => password}) do
    with {:ok, better_params} <- Tarams.cast(%{"username" => username, "password" => password}, @registeration_schema) do
     case UserClient.register(better_params.username, better_params.password) do
      {:ok, msg } ->
        IO.inspect(msg)
        render(conn, "show.json", user: %{
          id: msg.id,
          username: msg.username,
          password: msg.password,
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

  def get_user(conn, %{"username" => username}) do
      with {:ok, better_params} <- Tarams.cast(%{"username" => username}, @username_schema) do
        case UserClient.getting_user(better_params.username) do
        {:ok, msg } ->
          IO.inspect(msg)
          render(conn, "show.json", user: %{
            id: msg.id,
            username: msg.username,
            password: msg.password,
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

  def get_followers(conn, %{"username" => username}) do
    with {:ok, better_params} <- Tarams.cast(%{"username" => username}, @username_schema) do
      case UserClient.follow_user(better_params.username) do
       {:ok, msg } ->
        #  IO.inspect(msg)
         render(conn, "show.json", user: %{
           id: msg.id,
           username: msg.username,
           password: msg.password,
           token: Token.generate_and_sign!(msg),
         })
       {:error, msg} ->
        #  IO.inspect(msg)
         render(conn, "error.json", message: msg)
      end
     end
  end

  def create_post(conn, %{"content" => content}) do
    with {:ok, better_params} <- Tarams.cast(%{"content" => content}, @content) do
      case UserClient.creating_post(better_params.content) do
       {:ok, msg } ->
         IO.inspect(msg)
         render(conn, "post.json", post: msg)
       {:error, msg} ->
         IO.inspect(msg)
         render(conn, "error.json", message: msg)
      end
      else
        # return params error
        {:error, error} -> conn |> put_status(:bad_request) |> render("error.json", message: error)
      end
    end
  end

