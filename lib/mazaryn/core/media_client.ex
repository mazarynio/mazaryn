defmodule Core.MediaClient do

  def insert_music(username, single) do
    :media_server.insert_music(username, single)
  end

  def insert_video(username, single) do
    :media_server.insert_video(username, single)
  end
end
