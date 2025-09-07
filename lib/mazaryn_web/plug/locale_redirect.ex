# lib/mazaryn_web/plugs/locale_redirect.ex
defmodule MazarynWeb.Plugs.LocaleRedirect do
  import Plug.Conn
  import Phoenix.Controller, only: [redirect: 2]

  @skip_prefixes [
    "/assets", "/uploads", "/images", "/fonts",
    "/favicon.ico", "/robots.txt", "/live", "/phoenix"
  ]

  @locale_re ~r/^\/(en|fa|ru|zh|az|ar|es|vi|hi|de|fr|it|pt|fil|id)(\/|$)/

  def init(opts), do: opts

  def call(%Plug.Conn{request_path: path, method: method} = conn, _opts) do
    cond do
      Enum.any?(@skip_prefixes, &String.starts_with?(path, &1)) ->
        conn

      path =~ @locale_re ->
        conn

      method == "GET" ->
        redirect(conn, to: "/en" <> path) |> halt()

      true ->
        conn
    end
  end
end
