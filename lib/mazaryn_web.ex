defmodule MazarynWeb do

  def static_paths, do: ~w(assets fonts images uploads favicon.ico robots.txt)

  def controller do
    quote do
      use Phoenix.Controller, namespace: MazarynWeb

      import Plug.Conn
      import MazarynWeb.Gettext
      alias MazarynWeb.Router.Helpers, as: Routes

      unquote(verified_routes())
    end
  end

  def view do
    quote do
      use Phoenix.View,
        root: "lib/mazaryn_web/templates",
        namespace: MazarynWeb

      import Phoenix.Controller,
        only: [get_flash: 1, get_flash: 2, view_module: 1, view_template: 1]

      unquote(view_helpers())
    end
  end

  def live_view do
    quote do
      use Phoenix.LiveView,
        layout: {MazarynWeb.LayoutView, :live}

      unquote(view_helpers())
    end
  end

  def live_component do
    quote do
      use Phoenix.LiveComponent

      unquote(view_helpers())

       def icon(name, opts \\ []) do
        class = Keyword.get(opts, :class, "")

        icon_svg = case name do
          "hero-users-solid" ->
            """
            <svg class="#{class}" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
              <path d="M7 8a3 3 0 100-6 3 3 0 000 6zM14.5 9a2.5 2.5 0 100-5 2.5 2.5 0 000 5zM1.615 16.428a1.224 1.224 0 01-.569-1.175 6.002 6.002 0 0111.908 0c.058.467-.172.92-.57 1.174A9.953 9.953 0 017 18a9.953 9.953 0 01-5.385-1.572zM14.5 16h-.106c.07-.297.088-.611.048-.933a7.47 7.47 0 00-1.588-3.755 4.502 4.502 0 015.874 2.636.818.818 0 01-.36.98A7.465 7.465 0 0114.5 16z"/>
            </svg>
            """
          _ -> ""
        end

        Phoenix.HTML.raw(icon_svg)
      end
    end
  end

  def component do
    quote do
      use Phoenix.Component

      unquote(view_helpers())
    end
  end

  def router do
    quote do
      use Phoenix.Router

      import Plug.Conn
      import Phoenix.Controller
      import Phoenix.LiveView.Router
    end
  end

  def channel do
    quote do
      use Phoenix.Channel
      import MazarynWeb.Gettext
    end
  end

  defp view_helpers do
    quote do
      import Phoenix.HTML
      import Phoenix.HTML.Form
      use PhoenixHTMLHelpers

      import Phoenix.LiveView.Helpers

      import Phoenix.View

      import MazarynWeb.ErrorHelpers
      import MazarynWeb.Gettext
      alias MazarynWeb.Router.Helpers, as: Routes

      unquote(verified_routes())
    end
  end

  def html do
  quote do
    use Phoenix.Component

    import Phoenix.HTML
    import MazarynWeb.Gettext

    unquote(verified_routes())

    def icon(name, opts \\ []) do
      class = Keyword.get(opts, :class, "")

      icon_svg = case name do
        "hero-users-solid" ->
          """
          <svg class="#{class}" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 20 20" fill="currentColor">
            <path d="M7 8a3 3 0 100-6 3 3 0 000 6zM14.5 9a2.5 2.5 0 100-5 2.5 2.5 0 000 5zM1.615 16.428a1.224 1.224 0 01-.569-1.175 6.002 6.002 0 0111.908 0c.058.467-.172.92-.57 1.174A9.953 9.953 0 017 18a9.953 9.953 0 01-5.385-1.572zM14.5 16h-.106c.07-.297.088-.611.048-.933a7.47 7.47 0 00-1.588-3.755 4.502 4.502 0 015.874 2.636.818.818 0 01-.36.98A7.465 7.465 0 0114.5 16z"/>
          </svg>
          """
        _ -> ""
      end

      Phoenix.HTML.raw(icon_svg)
    end
  end
end

  def verified_routes do
    quote do
      use Phoenix.VerifiedRoutes,
        endpoint: MazarynWeb.Endpoint,
        router: MazarynWeb.Router,
        statics: MazarynWeb.static_paths()
    end
  end

  defmacro __using__(which) when is_atom(which) do
    apply(__MODULE__, which, [])
  end
end
