defmodule MazarynWeb.HomeLive.CreatePostComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Component.SelectLive
  alias Home.Post

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <.form let={f} for={@changeset} phx-change="validate-post" phx-submit="save-post" phx-target={@myself} multipart class="flex flex-col justify-between align-center py-5">
        <div class="flex items-center px-5">
          <img class="h-11 w-11 rounded-full" src="https://placeimg.com/192/192/people" />
          <div class="ml-4 text-sm leading-tight w-full">
            <%= textarea f, :body, class: "w-full border-none resize-none text-gray-300 font-normal block", placeholder: "Write a comment" %>
              <span class="text-sm"><%= push_error_tag f, :body %></span>
          </div>


        </div>
        <div class="flex flex-row justify-between items-center pt-10 px-5">
          <div class="flex flex-row items-center">
            <i><%= Heroicons.icon("emoji-happy", class: "h-5 w-5 mr-3 fill-gray-500" ) %></i>
            <i><%= Heroicons.icon("hashtag", class: "h-5 w-5 mr-3 fill-gray-500" ) %></i>
            <i><%= Heroicons.icon("photograph", class: "h-5 w-5 mr-3 fill-gray-500" ) %></i>
            <%= live_component SelectLive, id: "privacy-select", f: f, name: :privacy, options: ["public", "private", "only me"] %> 


    </div>
    <div>
    <%= submit "Post", class: "bg-blue-600 text-white border rounded-lg py-1.5 px-6 mx-5 self-auto", phx_disabled_with: "Posting..." %>
    </div>
    </div>
    </.form>
    </div>
    """
  end

  @impl true
  def handle_event("validate-post", %{"post" => post_params} = _params, socket) do
    post_params = Map.put(post_params, "user_id", socket.assigns.user_id)

    changeset =
      %Post{}
      |> Post.changeset(post_params)
      |> Ecto.Changeset.put_change(:user_id, socket.assigns.user_id)
      |> Map.put(:action, :validate)

    socket =
      socket
      |> assign(:changeset, changeset)

    {:noreply, socket}
  end

  def handle_event("save-post", %{"post" => post_params} = _params, socket) do
    post_params = Map.put(post_params, "user_id", socket.assigns.user_id)
    changeset = Post.changeset(%Post{}, post_params)
    author = Ecto.Changeset.get_field(changeset, :user_id)
    content = Ecto.Changeset.get_field(changeset, :body)

    case Core.PostClient.create_post(author, content) do
      :ok ->
        {:noreply, assign(socket, socket)}

      changeset ->
        {:noreply, assign(socket, :changeset, changeset)}
    end
  end
end
