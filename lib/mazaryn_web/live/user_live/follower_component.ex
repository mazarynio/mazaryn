defmodule MazarynWeb.UserLive.FollowerComponent do
  use MazarynWeb, :live_component
  # import MazarynWeb.Live.Helper

  def render(assigns) do
    ~H"""
    <div id="followers">
      <div class="mb-6">
        <div class="relative">
          <input
            type="text"
            placeholder="Search followers..."
            value={@search_query}
            phx-keyup="search_followers"
            phx-target={@myself}
            phx-debounce="300"
            class="w-full px-4 py-2 pr-10 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent"
          />
          <div class="absolute inset-y-0 right-0 flex items-center pr-3">
            <svg
              xmlns="http://www.w3.org/2000/svg"
              fill="none"
              viewBox="0 0 24 24"
              strokeWidth={1.5}
              stroke="currentColor"
              class="w-5 h-5 text-gray-400"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                d="M21 21l-5.197-5.197m0 0A7.5 7.5 0 105.196 5.196a7.5 7.5 0 0010.607 10.607z"
              />
            </svg>
          </div>
        </div>
      </div>

      <%= if @search_query != "" and @search_result == :user_not_exist do %>
        <div class="text-center py-8 text-gray-500">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            strokeWidth={1.5}
            stroke="currentColor"
            class="w-12 h-12 mx-auto mb-4 text-gray-300"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              d="M15.75 6a3.75 3.75 0 11-7.5 0 3.75 3.75 0 017.5 0zM4.501 20.118a7.5 7.5 0 0114.998 0A17.933 17.933 0 0112 21.75c-2.676 0-5.216-.584-7.499-1.632z"
            />
          </svg>
          <p class="text-lg">No follower found with username "<%= @search_query %>"</p>
        </div>
      <% else %>
        <%= for user <- @displayed_followers do %>
          <div class="flex w-full justify-between items-center my-4 p-3 border border-gray-200 rounded-lg hover:bg-gray-50 transition-colors">
            <div class="mr-3">
              <%= if user.avatar_url do %>
                <img alt="..." src={"#{user.avatar_url}"} class="h-9 w-9 object-cover rounded-full" />
              <% else %>
                <img
                  alt="Default user"
                  src={Routes.static_path(@socket, "/images/default-user.svg")}
                  class="h-9 w-9 object-cover rounded-full"
                />
              <% end %>
            </div>
            <div class="flex grow flex-col">
              <div class="font-medium">
                <%= user.username %>
              </div>
              <div class="text-slate-400 text text-sm">
                <%= user.media %>
              </div>
            </div>
            <div class="flex items-center px-4 h-8 bg-blue-500 text-white rounded-md cursor-pointer hover:bg-blue-600 transition-colors">
              <svg
                xmlns="http://www.w3.org/2000/svg"
                fill="none"
                viewBox="0 0 24 24"
                strokeWidth={1.5}
                stroke="currentColor"
                class="w-5 h-5 mr-1"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  d="M12 9v6m3-3H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z"
                />
              </svg>
              <div class="text-sm">
                <%= live_redirect("View Profile",
                  to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @locale, user.username)
                ) %>
              </div>
            </div>
          </div>
        <% end %>
      <% end %>


      <%= if @search_query == "" do %>
        <div class="text-sm text-gray-500 mt-4">
          Total followers: <%= length(@followers) %>
        </div>
      <% else %>
        <%= if @search_result != :user_not_exist do %>
          <div class="text-sm text-gray-500 mt-4">
            Search results for "<%= @search_query %>"
          </div>
        <% end %>
      <% end %>
    </div>
    """
  end

  def update(%{current_user: %Account.User{follower: follower_ids} = current_user} = assigns, socket) do
    followers = get_followers(follower_ids)

    {:ok,
     socket
     |> assign(assigns)
     |> assign(followers: followers)
     |> assign(displayed_followers: followers)
     |> assign(search_query: "")
     |> assign(search_result: nil)
     |> assign(current_user_id: get_user_id(current_user))}
  end

  def handle_event("search_followers", %{"value" => query}, socket) do
    query = String.trim(query)

    if query == "" do

      {:noreply,
       socket
       |> assign(search_query: query)
       |> assign(displayed_followers: socket.assigns.followers)
       |> assign(search_result: nil)}
    else

      case Core.UserClient.search_followers(socket.assigns.current_user_id, query) do
        :user_not_exist ->
          {:noreply,
           socket
           |> assign(search_query: query)
           |> assign(displayed_followers: [])
           |> assign(search_result: :user_not_exist)}

        user_tuple ->

          user = convert_user_tuple_to_struct(user_tuple)
          {:noreply,
           socket
           |> assign(search_query: query)
           |> assign(displayed_followers: [user])
           |> assign(search_result: :found)}
      end
    end
  end

  defp get_followers(follower_ids) do
    follower_ids
    |> Enum.map(fn follower_id -> follower_id |> Account.Users.get_user_by_id() end)
  end


  defp get_user_id(%Account.User{} = user) do

    user.id
  end


  defp convert_user_tuple_to_struct(user_tuple) do


    case user_tuple do
      {:user, _, _, _, _, _, _, _, username, _, _, _} ->
        %{
          username: convert_charlist_to_string(username),
          avatar_url: nil,
          media: "Follower"
        }

      {:user, _, _, _, _, _, _, _, username, _, _, _, _} ->
        %{
          username: convert_charlist_to_string(username),
          avatar_url: nil,
          media: "Follower"
        }

      {:user, _, _, _, _, _, _, _, username, _, _, _, _, _} ->
        %{
          username: convert_charlist_to_string(username),
          avatar_url: nil,
          media: "Follower"
        }

      tuple when is_tuple(tuple) and tuple_size(tuple) > 8 ->
        username = elem(tuple, 8)
        %{
          username: convert_charlist_to_string(username),
          avatar_url: nil,
          media: "Follower"
        }


      _ ->
        %{
          username: "Unknown",
          avatar_url: nil,
          media: "Follower"
        }
    end
  end

  defp convert_charlist_to_string(value) when is_list(value) do
    case :io_lib.printable_unicode_list(value) do
      true -> List.to_string(value)
      false -> "Unknown"
    end
  end

  defp convert_charlist_to_string(value) when is_binary(value), do: value
  defp convert_charlist_to_string(_), do: "Unknown"
end
