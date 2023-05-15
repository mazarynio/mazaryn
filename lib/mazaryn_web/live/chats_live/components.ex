defmodule MazarynWeb.ChatsLive.Components do
  use MazarynWeb, :component
  import MazarynWeb.Live.Helper, only: [handle_avatar: 1]

  embed_templates "components/*"
end
