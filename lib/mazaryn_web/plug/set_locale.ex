defmodule MazarynWeb.Plugs.SetLocale do
  @moduledoc false
  require Logger
  @supported_locales Gettext.known_locales(MazarynWeb.Gettext)

  def init(_options), do: nil

  def call(%Plug.Conn{params: %{"locale" => locale}} = conn, _options)
      when locale in @supported_locales do
    Logger.warning("Current locale set to: #{locale}")
    Gettext.put_locale(MazarynWeb.Gettext, locale)

    conn
    |> Plug.Conn.assign(:locale, locale)
    |> Plug.Conn.put_session(:locale, locale)
  end

  def call(conn, _options), do: conn |> tap(& &1.current_path |> dbg())
end
