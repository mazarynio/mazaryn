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

  @impl true
  def modal(assigns) do
    assigns = assign_new(assigns, :return_to, fn -> nil end)

    ~H"""
    <div id="modal" class="phx-modal fade-in" phx-remove={hide_modal()}>
      <div
        id="modal-content"
        class="phx-modal-content fade-in-scale mx-auto border-[#888] border-0 md:border my-[60px] md:my-[30px] md:rounded-[20px]"
        phx-click-away={JS.dispatch("click", to: "#close")}
        phx-window-keydown={JS.dispatch("click", to: "#close")}
        phx-key="escape"
      >
        <%= if @return_to do %>

          <%= live_patch to: @return_to, id: "close", class: "phx-modal-close p", phx_click: hide_modal() do %>
            <svg class="icon-close" width="12" height="12" viewBox="0 0 11 10" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path d="M5.5416 5.00003L10.2297 9.36919M0.853516 9.36919L5.5416 5.00003L0.853516 9.36919ZM10.2297 0.630859L5.5407 5.00003L10.2297 0.630859ZM5.5407 5.00003L0.853516 0.630859L5.5407 5.00003Z" stroke="#5D5F63" stroke-width="1.25" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
          <% end %>
        <% else %>
          <a id="close" data-phx-link="patch" data-phx-link-state="push" href="/profile" class="phx-modal-close" phx-click={hide_modal()}>
            <svg class="icon-close" width="12" height="12" viewBox="0 0 11 10" fill="none" xmlns="http://www.w3.org/2000/svg">
              <path d="M5.5416 5.00003L10.2297 9.36919M0.853516 9.36919L5.5416 5.00003L0.853516 9.36919ZM10.2297 0.630859L5.5407 5.00003L10.2297 0.630859ZM5.5407 5.00003L0.853516 0.630859L5.5407 5.00003Z" stroke="#5D5F63" stroke-width="1.25" stroke-linecap="round" stroke-linejoin="round"/>
            </svg>
          </a>
        <% end %>

        <%= render_slot(@inner_block) %>
      </div>
    </div>
    """
  end

  defp hide_modal(js \\ %JS{}) do
    js
    |> JS.hide(to: "#modal", transition: "fade-out")
    |> JS.hide(to: "#modal-content", transition: "fade-out-scale")
  end


end
