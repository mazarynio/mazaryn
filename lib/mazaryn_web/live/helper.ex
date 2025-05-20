defmodule MazarynWeb.Live.Helper do
  use MazarynWeb, :live_view

  alias Phoenix.LiveView.JS
  require Logger

  @impl Phoenix.LiveView
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  @impl Phoenix.LiveView
  def render(assigns) do
    ~H"""
    <div></div>
    """
  end

  def handle_avatar(user) do
    avatar_url = user.avatar_url
    if avatar_url do
      Mazaryn.config([:media, :ipfs_gateway]) <> avatar_url
    else
       ~p"/images/default-user.svg"
    end
  end

  def is_disabled(changeset) do
    if Ecto.Changeset.get_field(changeset, :form_disabled) == true do
      "disabled"
    else
      ""
    end
  end

  @spec showing_error(any(), Ecto.Changeset.t(), any()) :: <<_::_*32>>
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

  def insert_session_token(key, email) do
    token = Phoenix.Token.sign(MazarynWeb.Endpoint, signing_salt(), email)
    :ets.insert(:mazaryn_auth_table, {:"#{key}", token})
  end

  def get_user_id(session_uuid) do
    case :ets.lookup(:mazaryn_auth_table, :"#{session_uuid}") do
      [{_, token}] ->
        case Phoenix.Token.verify(MazarynWeb.Endpoint, signing_salt(), token, max_age: 806_400) do
          {:ok, email} ->
            email

          _ ->
            nil
        end

      _ ->
        nil
    end
  end

  @doc """
  <button phx-click={Phoenix.LiveView.JS.toggle(to: "#modal")}>Open Modal</button>


  <.modal>
    ...content
  </modal>
  """

  def modal(assigns) do
    ~H"""
    <div id="modal" class="hidden phx-modal fade-in">
      <div class="phx-overlay" phx-click={hide_modal()}></div>
      <div
        id="modal-content"
        class="phx-modal-content mx-auto border-[#888] border-0 md:border my-[60px] md:my-[30px] md:rounded-[20px]"
        phx-click-away={JS.dispatch("click", to: "#close")}
        phx-window-keydown={JS.dispatch("click", to: "#close")}
        phx-key="escape"
      >
        <a
          id="close"
          data-phx-link="patch"
          data-phx-link-state="push"
          class="phx-modal-close"
          phx-click={hide_modal()}
        >
          <svg
            class="icon-close"
            width="10"
            height="10"
            viewBox="0 0 11 10"
            fill="none"
            xmlns="http://www.w3.org/2000/svg"
          >
            <path
              d="M5.5416 5.00003L10.2297 9.36919M0.853516 9.36919L5.5416 5.00003L0.853516 9.36919ZM10.2297 0.630859L5.5407 5.00003L10.2297 0.630859ZM5.5407 5.00003L0.853516 0.630859L5.5407 5.00003Z"
              stroke="#5D5F63"
              stroke-width="1.25"
              stroke-linecap="round"
              stroke-linejoin="round"
            />
          </svg>
        </a>
        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end

  def open_modal(js \\ %JS{}) do
    js
    |> JS.show(to: "#modal", transition: "fade-in")
  end

  def hide_modal(js \\ %JS{}) do
    js
    |> JS.hide(to: "#modal", transition: "fade-out")
  end

  def show_modal(js \\ %JS{}) do
    js
    |> JS.show(to: "#modal", transition: "fade-in")
  end
end
