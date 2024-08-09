defmodule MazarynWeb.AuthLive.Reset do
  use MazarynWeb, :live_view

  alias Account.User
  alias Account.Users

  @reset_schema %{
    email: [type: :string, required: true]
  }

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  def handle_event("reset", %{"email" => email} = _session, socket) do
    with {:ok, _better_params} <- Tarams.cast(%{"email" => email}, @reset_schema),
         {:ok, %User{} = user} <- Users.one_by_email(email) do
      case Users.reset_password(user) do
        {:ok, :reseted} ->
          # TODO: handle
          {:noreply, socket}

        {:error, _res} ->
          {:noreply, redirect(socket, to: "/reset")}
      end
    else
      # TODO: Display error msg on frontend
      {:error, _msg} ->
        {:noreply, redirect(socket, to: "/reset")}
    end
  end
end
