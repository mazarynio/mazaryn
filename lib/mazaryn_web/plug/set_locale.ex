defmodule MazarynWeb.Plugs.SetLocale do
  @moduledoc false
  @supported_locales Gettext.known_locales(MazarynWeb.Gettext)

  def init(_options), do: nil

  def call(%Plug.Conn{params: %{"locale" => locale}} = conn, _options)
      when locale in @supported_locales do
    Gettext.put_locale(MazarynWeb.Gettext, locale) 

    conn
  end

  def call(conn, _options), do: conn
end