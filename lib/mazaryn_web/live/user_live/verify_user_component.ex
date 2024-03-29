defmodule MazarynWeb.UserLive.VerifyUserComponent do
  use MazarynWeb, :live_component

  @impl true
  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)}
  end

  def render(assigns) do
    ~H"""
    <div class="">
      <h2 class="left-0 w-full font-semibold text-[20px] leading-[24px] text-black border-b border-b-[#AAA] pb-5 mr-10">
        Account Verification
      </h2>
      <p class="my-5">
        <%= get_display_text(@user) %>
      </p>
      <div class="wrap-delete-btn">
        <button
          phx-click={@action_name}
          phx-value-username={@user.username}
          class="w-full py-2.5 px-5 text-base text-[#FAFAFA] focus:outline-none rounded-[10px] bg-red-400 hover:bg-red-500 hover:text-white focus:z-10 focus:ring-4 focus:ring-gray-200"
        >
          <%= if @user.verified, do: "Unverify User", else: "Verify User" %>
        </button>
      </div>
    </div>
    """
  end

  defp get_display_text(user) do
    "This will mark #{user.username} as " <> if user.verified, do: "Unverified", else: "Verified"
  end
end
