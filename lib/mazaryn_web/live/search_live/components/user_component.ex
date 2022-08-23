defmodule MazarynWeb.SearchLive.Components.UserComponent do
  use Phoenix.LiveComponent

  def render(assigns) do
    ~H"""
    <div class="flex">
      <div>picture</div>
      <div class="flex flex-col">
        <div>name</div>
        <div>tag</div>
      </div>
      <div>
        <svg class="icon-close" width="12" height="12" viewBox="0 0 11 10" fill="none" xmlns="http://www.w3.org/2000/svg">
          <path d="M5.5416 5.00003L10.2297 9.36919M0.853516 9.36919L5.5416 5.00003L0.853516 9.36919ZM10.2297 0.630859L5.5407 5.00003L10.2297 0.630859ZM5.5407 5.00003L0.853516 0.630859L5.5407 5.00003Z" stroke="#5D5F63" stroke-width="1.25" stroke-linecap="round" stroke-linejoin="round"/>
        </svg>
        <div>Follow</div>
      </div>

      <%= @found_user %>
    </div>
    """
  end
end
