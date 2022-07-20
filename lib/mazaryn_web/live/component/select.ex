defmodule MazarynWeb.Component.SelectLive do
  use MazarynWeb, :live_component

  @impl true
  def render(assigns) do
    ~H"""
    <div class="relative inline-block text-left">
      <button  phx-click={Phoenix.LiveView.JS.toggle(to: ".dropdown-menu-globe", in: "fade-in-scale", out: "fade-out-scale" )} type="button"
    class="dropdown inline-flex justify-center w-full rounded-md border border-slate-100 shadow-sm px-4 py-1.5 bg-slate-100 text-sm font-medium text-gray-700 flex-shrink-0 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-offset-gray-100 focus:ring-indigo-500" aria-expanded="true" aria-haspopup="true">
      <div class="rounded-full  flex-shrink-0">
        <i><%= Heroicons.icon("globe", class: "h-5 w-5 mr-3 fill-gray-500" ) %></i>
      </div>
      <p><%= @selected_option %></p>
      <i><%= Heroicons.icon("chevron-down", class: "-mr-1 ml-2 h-5 w-5") %> </i>
    </button>
    <ul class="dropdown-menu-globe hidden origin-top-right absolute right-0 mt-2 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5 focus:outline-none"
    role="menu" aria-orientation="vertical" aria-labelledby="menu-button" tabindex="-1">
        <%= for {option, idx} <- Enum.with_index(@options) do %>
          <li phx-click="select-item" phx-value-selected-item={"#{option}"} phx-target={@myself}  id={"#{idx}"} class="text-gray-700 block px-4 py-2 text-sm hover:bg-gray-100" role="menuitem" tabindex="-1">
            <%= option %> 
          </li>
          <% end %>
          <%= hidden_input @f, @name, value: @selected_option %>
    </ul>
    </div>
    """
  end

  @impl true
  def update(assigns, socket) do
    %{f: f, name: name, options: options} = assigns

    value =
      Map.get(f.params, "#{name}") ||
        Map.get(f.data, name)

    selected_option = Enum.find(options, &(&1 == value)) || List.first(options)

    socket =
      socket
      |> assign(:selected_option, selected_option)

    {:ok, assign(socket, assigns)}
  end

  def handle_event("select-item", %{"selected-item" => selected_item} = _params, socket) do
    socket = assign(socket, selected_option: selected_item)
    {:noreply, socket}
  end
end
