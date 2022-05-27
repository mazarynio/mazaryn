defmodule MazarynWeb.UserView do
  use MazarynWeb, :view

  def render("index.json", %{users: users}) do
      %{status: true, data: render_many(users, UserView, "user.json")}
  end

  def render("error.json", %{message: message}) do
      %{
        status: false,
        message: message
      }
  end

  def render("success.json", %{message: message}) do
    %{
      status: true,
      message: message
    }
end



  def render("show.json", %{user: user}) do
      %{
        status: true,
        data: render_one(user, UserView, "user.json")
      }
  end

  def render("post.json", %{post: post}) do
    %{
      status: true,
      data: %{
        id: post.id,
        content: post.content,
        created_at: post.created_at,
        updated_at: post.updated_at,
      }
    }
end


  def render("user.json", %{user: user}) do
      %{
        id: user.id,
        username: user.username,
        password: user.password,
      }
  end

  def render("error.json", %{changeset: changeset}) do
    errors = Enum.map(changeset.errors, fn {field, detail} -> "#{field} #{render_detail(detail)}" end)
    %{
      status: false,
      message: errors
    }
  end

  def render_detail({message, values}) do
    Enum.reduce values, message, fn {k, v}, acc ->
      String.replace(acc, "%{#{k}}", to_string(v))
    end
  end

  def render_detail(message) do
    message
  end



end
