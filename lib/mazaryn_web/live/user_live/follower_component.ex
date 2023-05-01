defmodule MazarynWeb.UserLive.FollowerComponent do
    use MazarynWeb, :live_component
    import MazarynWeb.Live.Helper

    def render(assigns) do
        ~H"""
        <div id="followers">
        <%= for {:ok, user} <- @followers do %>
        <div class="flex w-full justify-between items-center my-4">
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
            <div>
              <%= user.username %>
            </div>
            <div class="text-slate-400 text text-sm">
              <%= user.media %>
            </div>
          </div>
          <div class="flex items-center px-4 h-8 bg-blue-500 text-white rounded-md cursor-pointer">
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
                to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, user.username)
              ) %>
            </div>
          </div>
        </div>
      <% end %>
    </div>
        
        """
    end
      
    def update(%{current_user: %Account.User{follower: follower_ids}} = assigns, socket) do
        {:ok,
        socket
        |> assign(assigns)
        |> assign(followers: get_followers(follower_ids))}
    end
      
    defp get_followers(follower_ids) do
      follower_ids
    #   |> Core.PostClient.get_likes()
    #   |> Enum.map(&(&1 |> Home.Like.erl_changeset() |> Home.Like.build() |> elem(1)))
      |> Enum.map(fn follower_id -> follower_id |> Core.UserClient.get_user_by_id() end)
      |> Enum.map(&(&1 |> elem(2) |> Account.Users.one_by_username()))
    end
end




# <.modal 
# delete_action={@delete_action}
# is_hidden={@is_hidden}
# >
# <%= if @delete_action == :true do %>
#   <.live_component
#     module={MazarynWeb.UserLive.DeleteProfileComponent}
#     id={@user.username}
#     username={@user.username}
#     title="Delete Profile"
#     current_user={@current_user}
#     changeset={@user_changeset}
#   />
# <% else %>
#   <.live_component
#     module={MazarynWeb.UserLive.EditProfileComponent}
#     id={@user.username}
#     title="Edit Profile"
#     current_user={@current_user}
#     changeset={@user_changeset}
#   />
# <% end %>
# </.modal>