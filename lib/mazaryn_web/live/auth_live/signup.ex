defmodule MazarynWeb.AuthLive.Signup do
  use MazarynWeb, :live_view

  import MazarynWeb.Live.Helper
  alias Mazaryn.Signup

  alias MazarynWeb.Router.Helpers, as: Routes
  require Logger

  @signup_schema %{
    username: [type: :string, required: true],
    email: [type: :string, required: true],
    password: [type: :string, required: true],
    repassword: [type: :string, require: true],
    check: [type: :boolean, require: true]
  }

  # def mount(_params, _session, socket) do
  #   if connected?(socket), do: socket |> IO.inspect(label: 1)

  #   {:ok, socket}
  # end

  @impl true
  def mount(_params, %{"session_uuid" => key}, socket) do
    changeset =
      Signup.Form.changeset(%Signup.Form{})
      |> Map.put(:action, :insert)

    {:ok, assign(socket, key: key, changeset: changeset)}
  end

  @impl true
  def handle_event("save", %{"form" => params}, socket) do
    if Map.get(params, "form_disabled", nil) != "true" do
      changeset =
        Signup.Form.changeset(%Signup.Form{}, params)
        |> Ecto.Changeset.put_change(:form_submitted, true)
        |> Ecto.Changeset.put_change(:form_disabled, true)
        |> Map.put(:action, :insert)

      send(self(), {:disable_form, changeset})

      {:noreply, assign(socket, changeset: changeset)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("validate", %{"form" => params}, socket) do
    changeset = Signup.Form.changeset(%Signup.Form{}, params) |> Map.put(:action, :insert)
    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_event("blur_username", _value, socket) do
    blur_event("username", socket)
  end

  @impl true
  def handle_event("blur_email", _value, socket) do
    blur_event("email", socket)
  end

  @impl true
  def handle_event("blur_password", _value, socket) do
    blur_event("password", socket)
  end

  @impl true
  def handle_event(
        "toggle",
        %{"value" => _value},
        %{assigns: %{:changeset => changeset}} = socket
      ) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:accepts_conditions, true)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_event("toggle", _value, %{assigns: %{:changeset => changeset}} = socket) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:accepts_conditions, false)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end

  def blur_event(field, %{assigns: %{:changeset => changeset}} = socket) do
    changeset =
      changeset
      |> Ecto.Changeset.put_change(:"#{field}_touched", true)
      |> Map.put(:action, :insert)

    {:noreply, assign(socket, changeset: changeset)}
  end

  @impl true
  def handle_info({:disable_form, changeset}, %{assigns: %{:key => key}} = socket) do
    case Signup.Form.create_user(changeset) do
      %Account.User{email: email} ->
        insert_session_token(key, email)
        if connected?(socket), do: {:noreply, push_redirect(socket, to: "/login")}

      :username_existed ->
        changeset =
          changeset
          |> Ecto.Changeset.add_error(:password, "This account has been created before.")
          |> Ecto.Changeset.put_change(:form_disabled, false)

        {:noreply, assign(socket, changeset: changeset)}

      changeset ->
        changeset =
          changeset
          |> Ecto.Changeset.put_change(:form_disabled, false)

        Logger.info(changeset: changeset)
        {:noreply, assign(socket, changeset: changeset)}
    end
  end
end
