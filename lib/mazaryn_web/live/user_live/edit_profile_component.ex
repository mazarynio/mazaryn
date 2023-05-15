defmodule MazarynWeb.UserLive.EditProfileComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper
  alias Phoenix.LiveView.JS

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> allow_upload(:avatar_url,
       accept: ~w(.png .jpg .jpeg),
       max_entries: 1,
       max_file_size: 20_000_000,
       chunk_size: 64_000 * 3
     )
     |> allow_upload(:banner_url,
       accept: ~w(.png .jpg .jpeg),
       max_entries: 1,
       max_file_size: 20_000_000,
       chunk_size: 64_000 * 3
     )}
  end

  def handle_event("validate-profile-photo", params, socket) do
    {:noreply, socket}
  end

  def handle_event("validate-info", params, socket) do
    {:noreply, socket}
  end

  def handle_event("save-info", %{"user" => params}, socket) do
    current_user = socket.assigns.current_user
    fields = Map.keys(params)
    values = Map.values(params)

    IO.inspect(current_user, label: "=========================user")

    Core.UserClient.set_user_info(current_user.id, fields, values)

    {:ok, current_user} = Account.Users.get_by_session_uuid(socket.assigns.session_uuid)

    current_user |> IO.inspect(label: "=============================")

    {:noreply, socket}
  end

  def handle_event("save-profile-photo", params, socket) do
    current_user = socket.assigns.current_user

    case consume_upload(socket, :avatar_url) do
      [avatar_url] ->
        Account.Users.insert_avatar(current_user.id, avatar_url)
        {:noreply, socket}

      [] ->
        {:noreply, socket}
    end
  end

  def handle_event("validate-banner", params, socket) do
    {:noreply, socket}
  end

  def handle_event("save-banner", params, socket) do
    current_user = socket.assigns.current_user

    case consume_upload(socket, :banner_url) do
      [banner_url] ->
        Account.Users.insert_banner(current_user.id, banner_url)
        {:noreply, socket}

      [] ->
        {:noreply, socket}
    end
  end

  def preload(list_of_assigns) do
    Enum.map(list_of_assigns, fn assigns ->
      assigns
    end)
  end

  defp consume_upload(socket, field) do
    consume_uploaded_entries(socket, field, fn %{path: path}, entry ->
      dir = Mazaryn.config([:media, :uploads_dir])

      dest = Path.join(dir, "#{entry.uuid}.#{ext(entry)}")
      File.mkdir_p!(Path.dirname(dest))
      File.cp!(path, dest)
      {:ok, Routes.static_path(socket, "/uploads/#{Path.basename(dest)}")}
    end)
  end

  defp ext(entry) do
    [ext | _] = MIME.extensions(entry.client_type)
    ext
  end

  def toggle_tab1(js \\ %JS{}) do
    js
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-2", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-3", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-4", transition: "fade-out-scale")
    |> JS.add_class("acc-active", to: ".js--accordion.acc-1", transition: "ease-in duration-300")
  end

  def toggle_tab2(js \\ %JS{}) do
    js
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-1", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-3", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-4", transition: "fade-out-scale")
    |> JS.add_class("acc-active", to: ".js--accordion.acc-2", transition: "ease-in duration-300")
  end

  def toggle_tab3(js \\ %JS{}) do
    js
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-1", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-2", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-4", transition: "fade-out-scale")
    |> JS.add_class("acc-active",
      to: ".js--accordion.acc-3",
      transition: "ease-in duration-300"
    )
  end

  def toggle_tab4(js \\ %JS{}) do
    js
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-1", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-2", transition: "fade-out-scale")
    |> JS.remove_class("acc-active",
      to: ".js--accordion.acc-3",
      transition: "ease-in duration-300"
    )
    |> JS.add_class("acc-active", to: ".js--accordion.acc-4", transition: "fade-out-scale")
  end
end
