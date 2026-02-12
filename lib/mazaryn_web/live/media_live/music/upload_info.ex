defmodule MazarynWeb.MediaLive.Music.UploadInfo do
  use MazarynWeb, :live_view
  require Logger

  @impl true
  def mount(_params, session, socket) do
    user = get_user_from_session(session)

    {:ok,
     socket
     |> assign(:page_title, "Upload Information")
     |> assign(:user, user)
     |> assign(:current_user, user)
     |> assign(:locale, socket.assigns[:locale] || "en")
     |> assign(:search_query, "")
     |> assign(:tag_input, "")
     |> assign(:form, %{
       title: "",
       slug: "",
       cover_image: nil,
       description: "",
       visibility: "public",
       tags: []
     })
     |> assign(:known_tags, [
       "Dance",
       "Disco",
       "Country",
       "Classical",
       "Rock",
       "Jazz",
       "Hip Hop",
       "Electronic",
       "Pop",
       "R&B"
     ])}
  end

  @impl true
  def handle_params(_params, _url, socket) do
    {:noreply, socket}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    {:noreply, socket |> assign(:search_query, query)}
  end

  @impl true
  def handle_event(
        "update_form",
        %{"title" => title, "slug" => slug, "description" => description},
        socket
      ) do
    form = socket.assigns.form
    updated_form = %{form | title: title, slug: slug, description: description}
    {:noreply, socket |> assign(:form, updated_form)}
  end

  @impl true
  def handle_event("update_tag_input", %{"value" => tag_input}, socket) do
    {:noreply, socket |> assign(:tag_input, tag_input)}
  end

  @impl true
  def handle_event("add_tag", %{"key" => "Enter", "value" => tag_input}, socket) do
    add_tag_to_form(socket, tag_input)
  end

  @impl true
  def handle_event("add_tag_from_input", _params, socket) do
    add_tag_to_form(socket, socket.assigns.tag_input)
  end

  @impl true
  def handle_event("toggle_tag", %{"tag" => tag}, socket) do
    form = socket.assigns.form

    updated_tags =
      if tag in form.tags do
        List.delete(form.tags, tag)
      else
        if length(form.tags) < 5 do
          [tag | form.tags]
        else
          form.tags
        end
      end

    updated_form = %{form | tags: updated_tags}
    {:noreply, socket |> assign(:form, updated_form)}
  end

  @impl true
  def handle_event("set_visibility", %{"visibility" => visibility}, socket) do
    form = socket.assigns.form
    updated_form = %{form | visibility: visibility}
    {:noreply, socket |> assign(:form, updated_form)}
  end

  @impl true
  def handle_event("upload_cover", %{"cover_image" => cover_image}, socket) do
    Logger.info("Uploading cover image: #{cover_image.filename}")

    form = socket.assigns.form
    updated_form = %{form | cover_image: "https://placehold.co/400x400"}

    {:noreply,
     socket
     |> assign(:form, updated_form)
     |> put_flash(:info, "Cover image uploaded successfully!")}
  end

  @impl true
  def handle_event("save_info", _params, socket) do
    Logger.info("Saving upload information: #{inspect(socket.assigns.form)}")

    {:noreply,
     socket
     |> put_flash(:info, "Information saved successfully!")
     |> push_navigate(to: ~p"/#{socket.assigns.locale}/music")}
  end

  defp get_user_from_session(%{"user_id" => user_id}) when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(%{"session_uuid" => _session_uuid, "user_id" => user_id})
       when user_id != nil do
    case Core.UserClient.get_user_by_id(user_id) do
      {:error, _} -> nil
      user_tuple when is_tuple(user_tuple) -> user_tuple
      _ -> nil
    end
  end

  defp get_user_from_session(_), do: nil

  defp add_tag_to_form(socket, tag_input) do
    tag_input = String.trim(tag_input)

    if tag_input != "" do
      form = socket.assigns.form

      if length(form.tags) < 5 do
        updated_tags = [tag_input | form.tags]
        updated_form = %{form | tags: updated_tags}

        {:noreply,
         socket
         |> assign(:form, updated_form)
         |> assign(:tag_input, "")}
      else
        {:noreply,
         socket
         |> put_flash(:error, "Maximum 5 tags allowed")}
      end
    else
      {:noreply, socket}
    end
  end
end
