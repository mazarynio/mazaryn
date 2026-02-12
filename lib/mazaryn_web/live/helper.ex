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

  def get_username(nil), do: ""

  def get_username(%Account.User{} = user) do
    case user.username do
      u when is_binary(u) and u != "" -> u
      u when is_list(u) -> List.to_string(u)
      _ -> ""
    end
  end

  def get_username(user) when is_tuple(user) do
    if tuple_size(user) > 8 do
      username = elem(user, 8)

      case username do
        u when is_list(u) ->
          str = List.to_string(u)
          if String.starts_with?(str, ["/ip4", "/ip6"]), do: "", else: str

        u when is_binary(u) ->
          if String.starts_with?(u, ["/ip4", "/ip6"]), do: "", else: u

        _ ->
          ""
      end
    else
      ""
    end
  end

  def get_username(_), do: ""

  def get_user_id(nil), do: ""

  def get_user_id(%Account.User{} = user) do
    case user.id do
      id when is_list(id) -> List.to_string(id)
      id when is_binary(id) -> id
      _ -> ""
    end
  end

  def get_user_id(user) when is_tuple(user) do
    if tuple_size(user) > 1 do
      id = elem(user, 1)

      case id do
        id when is_list(id) -> List.to_string(id)
        id when is_binary(id) -> id
        _ -> ""
      end
    else
      ""
    end
  end

  def get_user_id(_), do: ""

  def handle_avatar(nil), do: ~p"/images/default-user.svg"

  def handle_avatar(%Account.User{} = user) do
    cond do
      user.avatar_url && user.avatar_url != "" ->
        avatar_url =
          if is_list(user.avatar_url),
            do: List.to_string(user.avatar_url),
            else: user.avatar_url

        if String.starts_with?(avatar_url, ["http://", "https://", "/"]) do
          avatar_url
        else
          gateway = Mazaryn.config([:media, :ipfs_gateway]) || "https://ipfs.io/ipfs/"
          gateway <> avatar_url
        end

      true ->
        ~p"/images/default-user.svg"
    end
  end

  def handle_avatar(user) when is_tuple(user) do
    if tuple_size(user) > 17 do
      avatar_url = elem(user, 17)

      if avatar_url && avatar_url != "" do
        avatar_str = if is_list(avatar_url), do: List.to_string(avatar_url), else: avatar_url

        if String.starts_with?(avatar_str, ["http://", "https://", "/"]) do
          avatar_str
        else
          gateway = Mazaryn.config([:media, :ipfs_gateway]) || "https://ipfs.io/ipfs/"
          gateway <> avatar_str
        end
      else
        ~p"/images/default-user.svg"
      end
    else
      ~p"/images/default-user.svg"
    end
  end

  def handle_avatar(_), do: ~p"/images/default-user.svg"

  def get_user_from_session(%{"session_uuid" => session_uuid}) when session_uuid != nil do
    case Account.Users.get_by_session_uuid(session_uuid) do
      {:ok, user} -> user
      _ -> nil
    end
  end

  def get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    charlist_id = if is_list(user_id), do: user_id, else: String.to_charlist(user_id)

    case Core.UserClient.get_user_by_id(charlist_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  def get_user_from_session(_), do: nil

  def is_admin_user(username) when is_binary(username) and username != "" do
    admin_usernames = ["arvand", "mazaryn", "zaryn"]
    normalized = username |> String.trim() |> String.downcase()
    Enum.member?(admin_usernames, normalized)
  end

  def is_admin_user(_), do: false

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
