defmodule MazarynWeb.UserLive.EditProfileComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper

  alias Phoenix.LiveView.JS
  alias Core.PostClient

  @impl true
  def mount(socket) do
    {:ok,
     socket
     |> assign(:uploaded_files, [])
     |> assign(:success_msg, nil)
     |> assign(:failure_msg, nil)
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

  @impl true
  def handle_event("validate-profile-photo", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("validate-info", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("save-info", %{"user" => params}, socket) do
    {:noreply, save_user_info(socket, params)}
  end

  def handle_event("save-bio", %{"user" => params}, socket) do
    {:noreply, save_user_info(socket, params)}
  end

  def handle_event("save-profile", _params, socket) do
    current_user = socket.assigns.current_user

    case consume_upload(socket, :avatar_url) do
      [avatar_url] ->
        Account.Users.insert_avatar(current_user.id, avatar_url)
        {:noreply, socket}

      [] ->
        {:noreply, socket}
    end
  end

  def handle_event("save-profile-pic", _params, socket) do
    current_user = socket.assigns.current_user
    # save the file to database
    case uploads(:avatar_url, socket) do
      [upload_url] ->

        avatar_cid =
          upload_url
          |> PostClient.upload_media()
          |> List.to_string()

        Account.Users.insert_avatar(current_user.id, avatar_cid)
        {:noreply, socket}

      [] ->
        {:noreply, socket}
    end
  end

  def handle_event("validate-banner", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("save-banner", _params, socket) do
    current_user = socket.assigns.current_user

    case consume_upload(socket, :banner_url) do
      [banner_url] ->
        Account.Users.insert_banner(current_user.id, banner_url)
        {:noreply, socket}

      [] ->
        {:noreply, socket}
    end
  end

  def handle_event("save-banner-pic", _params, socket) do
    current_user = socket.assigns.current_user

    case uploads(:banner_url, socket) do
      [upload_url] ->

        banner_cid =
          upload_url
          |> PostClient.upload_media()
          |> List.to_string()

          dbg(banner_cid)

        Account.Users.insert_banner(current_user.id, banner_cid)
        {:noreply, socket}

      [] ->
        {:noreply, socket}
    end
  end

  @impl Phoenix.LiveComponent
  def update_many(assigns_socket_list) do
    Enum.map(assigns_socket_list, fn {assigns, socket} ->
      assign(socket, assigns)
    end)
  end

  defp uploads(name, socket) do
    consume_uploaded_entries(socket, name, fn %{path: path}, entry ->
      dir = Mazaryn.config([:media, :uploads_dir])

      dest = Path.join(dir, "#{entry.uuid}.#{ext(entry)}")
      File.mkdir_p!(Path.dirname(dest))
      File.cp!(path, dest)

      {:ok, dest}
    end)
  end

  # TODO: delete below once edit profile bug is fixed
  # def preload(list_of_assigns) do
  #   IO.inspect(list_of_assigns, label: "[PRELOAD]")

  #   Enum.map(list_of_assigns, fn assigns ->
  #     assigns
  #   end)
  # end

  defp error_to_string(:too_large), do: "Too large"
  defp error_to_string(:not_accepted), do: "You have selected an unacceptable file type"

  defp save_user_info(socket, params) do
    current_user = socket.assigns.current_user
    fields = Map.keys(params)
    values = Map.values(params)

    case Core.UserClient.set_user_info(current_user.id, fields, values) do
      :ok -> socket |> assign(:success_msg, "Successfully Saved")
      _ -> socket |> assign(:failure_msg, "Something went wrong not saved")
    end
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
