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

  def handle_event("save-profile-pic", _params, socket) do
    current_user = socket.assigns.current_user

    case uploads(:avatar_url, socket) do
      [upload_url] ->
        avatar_cid =
          upload_url
          |> PostClient.upload_media()
          |> List.to_string()

        Account.Users.insert_avatar(current_user.id, avatar_cid)

        socket =
          socket
          |> assign(:success_msg, "Profile picture updated successfully!")
          |> assign(:failure_msg, nil)

        {:noreply, socket}

      [] ->
        socket =
          socket
          |> assign(:failure_msg, "Please select an image to upload")
          |> assign(:success_msg, nil)

        {:noreply, socket}
    end
  end

  def handle_event("validate-banner", _params, socket) do
    {:noreply, socket}
  end

  def handle_event("save-banner-pic", _params, socket) do
    current_user = socket.assigns.current_user

    case uploads(:banner_url, socket) do
      [upload_url] ->
        banner_cid =
          upload_url
          |> PostClient.upload_media()
          |> List.to_string()

        Account.Users.insert_banner(current_user.id, banner_cid)

        socket =
          socket
          |> assign(:success_msg, "Banner image updated successfully!")
          |> assign(:failure_msg, nil)

        {:noreply, socket}

      [] ->
        socket =
          socket
          |> assign(:failure_msg, "Please select an image to upload")
          |> assign(:success_msg, nil)

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
