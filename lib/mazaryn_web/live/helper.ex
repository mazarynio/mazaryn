defmodule MazarynWeb.Live.Helper do
  import Phoenix.LiveView
  import Phoenix.LiveView.Helpers

  alias MazarynWeb.Router.Helpers, as: Routes

  alias Phoenix.LiveView.JS
  require Logger

  def is_disabled(changeset) do
    if Ecto.Changeset.get_field(changeset, :form_disabled) == true do
      "disabled"
    else
      ""
    end
  end

  def showing_error(f, changeset, name) do
    error = error_visible(f, changeset, name)
    hint = hint_visible(f, changeset, name)

    if !error && !hint do
      "mb-6"
    else
      ""
    end
  end

  def inline_error(f, changeset, name) do
    if error_visible(f, changeset, name) do
      "rounded-b-none border-b-0 focus:border-red-600 border-red-600"
    else
      if hint_visible(f, changeset, name) do
        "rounded-b-none border-b-0 focus:border-shop-blue border-shop-blue"
      else
        "focus:border-shop-blue"
      end
    end
  end

  def error_visible(f, changeset, name) do
    submitted = Ecto.Changeset.get_field(changeset, :form_submitted)
    touched = Ecto.Changeset.get_field(changeset, :"#{name}_touched")

    if submitted || touched do
      if f.errors[name] do
        true
      else
        false
      end
    end
  end

  def hint_visible(f, changeset, name) do
    if error_visible(f, changeset, name) do
      false
    else
      focused = Ecto.Changeset.get_field(changeset, :"#{name}_focused")

      if focused && f.errors[name] do
        true
      else
        false
      end
    end
  end

  def aria_hidden(f, changeset, name) do
    error = error_visible(f, changeset, name)
    hint = hint_visible(f, changeset, name)

    if error || hint do
      false
    else
      true
    end
  end

  def detail_error(f, changeset, name) do
    error = error_visible(f, changeset, name)
    hint = hint_visible(f, changeset, name)

    classes =
      if error do
        "border-red-600"
      else
        "border-blue"
      end

    visibility =
      if error || hint do
        "mb-1"
      else
        "hidden"
      end

    "#{classes} #{visibility}"
  end

  def submit_value(changeset, default_value) do
    if Ecto.Changeset.get_field(changeset, :form_disabled) do
      "SUBMITTING"
    else
      default_value
    end
  end

  def signing_salt do
    salt = MazarynWeb.Endpoint.config(:live_view)[:signing_salt]

    salt ||
      raise MazarynWeb.AuthenticationError, message: "missing signing_salt"
  end

  def insert_session_token(key, user_id) do
    token = Phoenix.Token.sign(MazarynWeb.Endpoint, signing_salt(), user_id)
    :ets.insert(:mazaryn_auth_table, {:"#{key}", token})
  end

  def get_user_id(session_uuid) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        case Phoenix.Token.verify(MazarynWeb.Endpoint, signing_salt(), token, max_age: 806_400) do
          {:ok, user_id} ->
            user_id

          _ ->
            nil
        end

      _ ->
        nil
    end
  end
end
