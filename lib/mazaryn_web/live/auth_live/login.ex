defmodule MazarynWeb.AuthLive.Login do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper

  alias Account.User
  alias Account.Users
  alias Mazaryn.Token
  alias Mazaryn.Login
  require Logger

  @login_schema %{
    email: [type: :string, required: true],
    password: [type: :string, required: true]
  }

  @impl true
  def mount(_params, %{"session_uuid" => key}, socket) do
    # Logger.info(socket: socket)
    changeset =
      Login.Form.changeset(%Login.Form{}, %{})
      |> Map.put(:action, :insert)

    {:ok, assign(socket, key: key, changeset: changeset)}
  end

  @impl true
  def handle_event("save", %{"form" => params}, %{assigns: %{:key => key}} = socket) do
    if Map.get(params, "form_disabled", nil) != "true" do
      changeset =
        Login.Form.changeset(%Login.Form{}, params)
        |> Ecto.Changeset.put_change(:form_submitted, true)
        |> Ecto.Changeset.put_change(:form_disabled, true)
        |> Map.put(:action, :insert)

      case Login.Form.get_user_by_email(changeset) do
        %Account.User{email: email} ->
          insert_session_token(key, email)

          {:noreply,
           push_redirect(socket, to: Routes.live_path(socket, MazarynWeb.HomeLive.Home))}

        changeset ->
          changeset =
            changeset
            |> Ecto.Changeset.put_change(:form_disabled, false)

          {:noreply, assign(socket, changeset: changeset)}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("save", param, socket) do
    Logger.info(params: param)
    {:noreply, socket}
  end

  @impl true
  def handle_event("validate", %{"form" => params}, socket) do
    changeset = Login.Form.changeset(%Login.Form{}, params) |> Map.put(:action, :insert)
    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_event("blur_email", _value, socket) do
    blur_event("email", socket)
  end

  @impl true
  def handle_event("blur_password", _value, socket) do
    blur_event("password", socket)
  end

  def blur_event(field, %{assigns: %{:changeset => changeset}} = socket) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:"#{field}_touched", true)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end
end
