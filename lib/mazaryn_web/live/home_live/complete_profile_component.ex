defmodule MazarynWeb.HomeLive.CompleteProfileComponent do
  use MazarynWeb, :live_component

  def render(assigns) do
    ~H"""
    <div class="w-full bg-white white:bg-gray-800 shadow p-4 rounded-xl border">
      <h1 class="block text-l font-semibold py-4 px-2">Complete your profile</h1>
      <div class="border-gray-300 border border-x-0 border-b-0 py-5 px-10"></div>
      <div class="w-full bg-gray-200 h-1">
        <div class="bg-blue-600 h-1" style="width: 45%"></div>
      </div>
      <div class="flex justify-between align-center items-center py-5">

        <div class="flex justify-center items-center">
          <ul class="">
            <li class="active">
              <%= live_patch "Verify your email", to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @user.username), class: "block text-l px-2 py-4 text-gray-300 font-semibold" %>
            </li>
            <li class="active">
              <%= live_patch "Set your username", to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @user.username), class: "block text-l px-2 py-4 text-gray-500 font-semibold" %>
            </li>
            <li class="active">
              <%= live_patch "Set your profile picture", to: Routes.live_path(@socket, MazarynWeb.UserLive.Profile, @user.username), class: "block text-l px-2 py-4 text-gray-500 font-semibold" %>
            </li>
          </ul>
        </div>
      </div>
    </div>
    """
  end
end
