defmodule MazarynWeb.ChatsLive.Components do
  use MazarynWeb, :component
  use Phoenix.Component
  import MazarynWeb.Live.Helper, only: [handle_avatar: 1]

  embed_templates "components/*"
end
