defmodule MazarynWeb.LayoutView do
  use MazarynWeb, :view

  # Phoenix LiveDashboard is available only in development by default,
  # so we instruct Elixir to not warn if the dashboard route is missing.
  @compile {:no_warn_undefined, {Routes, :live_dashboard_path, 2}}

  def set_locale(locale, conn) do
    current_locale = Gettext.get_locale(MazarynWeb.Gettext)

    "#{String.replace(conn.request_path, "/#{current_locale}", "/#{locale}")}#{(conn.query_string != "" && "?#{conn.query_string}") || ""}"
  end
end
