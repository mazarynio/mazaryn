defmodule MazarynWeb.BlogLive.Post.Blockchain do
  use MazarynWeb, :live_view

  alias Phoenix.LiveView.JS

  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  def toggle_menu(js \\ %JS{}) do
    js
    |> JS.toggle(to: ".menu-content", in: "fade-overflow")
  end
end
