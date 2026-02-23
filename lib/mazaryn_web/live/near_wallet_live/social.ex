defmodule MazarynWeb.NearWalletLive.Social do
  use MazarynWeb, :live_view
  require Logger
  alias Account.Users
  alias Mazaryn.NearWallets

  @max_post_length 1000

  @impl true
  def mount(
        %{"wallet_id" => wallet_id} = _params,
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    case Users.get_by_session_uuid(session_uuid) do
      {:ok, user} ->
        case NearWallets.get_wallet(wallet_id) do
          {:ok, wallet} ->
            posts =
              case NearWallets.get_wallet_social_posts(wallet_id) do
                {:ok, p} -> p
                _ -> []
              end

            {:ok,
             socket
             |> assign(:user, user)
             |> assign(:current_user, user)
             |> assign(:wallet, wallet)
             |> assign(:wallet_id, wallet_id)
             |> assign(:posts, posts)
             |> assign(:show_new_post_modal, false)
             |> assign(:post_text, "")
             |> assign(:post_tags, "")
             |> assign(:post_media_urls, "")
             |> assign(:selected_post, nil)
             |> assign(:posting, false)
             |> assign(:form_errors, %{})
             |> assign(:chars_remaining, @max_post_length)}

          {:error, _} ->
            {:ok,
             socket
             |> put_flash(:error, "NEAR wallet not found")
             |> redirect(to: "/en/wallet")}
        end

      {:error, _} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_params(%{"live_action" => :new}, _url, socket) do
    {:noreply,
     socket
     |> assign(:show_new_post_modal, true)
     |> assign(:post_text, "")
     |> assign(:post_tags, "")
     |> assign(:post_media_urls, "")
     |> assign(:form_errors, %{})
     |> assign(:chars_remaining, @max_post_length)}
  end

  @impl true
  def handle_params(_params, _url, socket), do: {:noreply, socket}

  @impl true
  def handle_event("open_new_post_modal", _params, socket) do
    {:noreply,
     socket
     |> assign(:show_new_post_modal, true)
     |> assign(:post_text, "")
     |> assign(:post_tags, "")
     |> assign(:post_media_urls, "")
     |> assign(:form_errors, %{})
     |> assign(:chars_remaining, @max_post_length)}
  end

  @impl true
  def handle_event("close_new_post_modal", _params, socket) do
    {:noreply, assign(socket, :show_new_post_modal, false)}
  end

  @impl true
  def handle_event(
        "validate_post",
        %{"post_text" => text, "post_tags" => tags, "post_media_urls" => media_urls},
        socket
      ) do
    chars_remaining = max(0, @max_post_length - String.length(text))

    {:noreply,
     socket
     |> assign(:post_text, text)
     |> assign(:post_tags, tags)
     |> assign(:post_media_urls, media_urls)
     |> assign(:chars_remaining, chars_remaining)
     |> assign(:form_errors, %{})}
  end

  @impl true
  def handle_event(
        "create_post",
        %{"post_text" => text, "post_tags" => tags, "post_media_urls" => media_urls},
        socket
      ) do
    errors = validate_post_form(text)

    if map_size(errors) > 0 do
      {:noreply, assign(socket, :form_errors, errors)}
    else
      socket = assign(socket, :posting, true)

      parsed_tags =
        tags
        |> String.split([",", " ", "#"], trim: true)
        |> Enum.map(&String.trim/1)
        |> Enum.reject(&(&1 == ""))
        |> Enum.uniq()

      parsed_media_urls =
        media_urls
        |> String.split([",", "\n"], trim: true)
        |> Enum.map(&String.trim/1)
        |> Enum.reject(&(&1 == ""))

      post_data = %{
        account_id: socket.assigns.wallet.account_id,
        contract: "social.near",
        text: text,
        tags: parsed_tags,
        media_urls: parsed_media_urls
      }

      case NearWallets.create_social_post(socket.assigns.wallet_id, post_data) do
        {:ok, _post_id} ->
          posts =
            case NearWallets.get_wallet_social_posts(socket.assigns.wallet_id) do
              {:ok, p} -> p
              _ -> []
            end

          {:noreply,
           socket
           |> assign(:posting, false)
           |> assign(:show_new_post_modal, false)
           |> assign(:post_text, "")
           |> assign(:post_tags, "")
           |> assign(:post_media_urls, "")
           |> assign(:posts, posts)
           |> put_flash(:info, "Post published on NEAR Social")}

        {:error, reason} ->
          Logger.error("NEAR social post failed: #{inspect(reason)}")

          {:noreply,
           socket
           |> assign(:posting, false)
           |> assign(:form_errors, %{general: "Failed to publish post. Please try again."})}
      end
    end
  end

  @impl true
  def handle_event("select_post", %{"post_id" => post_id}, socket) do
    post = Enum.find(socket.assigns.posts, &(to_string(&1.post_id) == post_id))
    {:noreply, assign(socket, :selected_post, post)}
  end

  @impl true
  def handle_event("close_post_modal", _params, socket) do
    {:noreply, assign(socket, :selected_post, nil)}
  end

  @impl true
  def handle_event("refresh", _params, socket) do
    posts =
      case NearWallets.get_wallet_social_posts(socket.assigns.wallet_id) do
        {:ok, p} -> p
        _ -> []
      end

    {:noreply,
     socket
     |> assign(:posts, posts)
     |> put_flash(:info, "Posts refreshed")}
  end

  @impl true
  def handle_event("stop_propagation", _params, socket), do: {:noreply, socket}

  defp validate_post_form(text) do
    errors = %{}

    errors =
      if String.trim(text) == "" do
        Map.put(errors, :post_text, "Post content is required")
      else
        errors
      end

    errors =
      if String.length(text) > @max_post_length do
        Map.put(errors, :post_text, "Post must be #{@max_post_length} characters or fewer")
      else
        errors
      end

    errors
  end

  def truncate_key(nil), do: "—"
  def truncate_key(:undefined), do: "—"
  def truncate_key(key) do
    s = to_string(key)
    if String.length(s) > 16,
      do: String.slice(s, 0, 8) <> "...." <> String.slice(s, -8, 8),
      else: s
  end

  def format_datetime(nil), do: "—"
  def format_datetime(:undefined), do: "—"
  def format_datetime({{y, m, d}, {h, mn, s}}) do
    :io_lib.format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B", [y, m, d, h, mn, s])
    |> IO.iodata_to_binary()
  end
  def format_datetime(%NaiveDateTime{} = dt), do: NaiveDateTime.to_string(dt) |> String.slice(0, 16)
  def format_datetime(%DateTime{} = dt), do: DateTime.to_string(dt) |> String.slice(0, 16)
  def format_datetime(_), do: "—"

  def chars_color(remaining) when remaining < 50,  do: "text-red-400"
  def chars_color(remaining) when remaining < 150, do: "text-yellow-400"
  def chars_color(_),                              do: "text-slate-500"

  def truncate_text(nil, _limit), do: ""
  def truncate_text(text, limit) do
    if String.length(text) > limit,
      do: String.slice(text, 0, limit) <> "…",
      else: text
  end
end
