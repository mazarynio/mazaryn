defmodule MazarynWeb.UserLive.EditProfileComponent do
  use MazarynWeb, :live_component

  import MazarynWeb.Live.Helper
  alias Phoenix.LiveView.JS

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket}
  end

  def toggle_tab1(js \\ %JS{}) do
    js
    |> IO.inspect()
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-2", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-3", transition: "fade-out-scale")
    |> JS.add_class("acc-active", to: ".js--accordion.acc-1", transition: "ease-in duration-300")
  end

  def toggle_tab2(js \\ %JS{}) do
    js
    |> IO.inspect()
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-1", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-3", transition: "fade-out-scale")
    |> JS.add_class("acc-active", to: ".js--accordion.acc-2", transition: "ease-in duration-300")
  end

  def toggle_tab3(js \\ %JS{}) do
    js
    |> IO.inspect()
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-1", transition: "fade-out-scale")
    |> JS.remove_class("acc-active", to: ".js--accordion.acc-2", transition: "fade-out-scale")
    |> JS.add_class("acc-active", to: ".js--accordion.acc-3", transition: "ease-in duration-300")
  end
end
