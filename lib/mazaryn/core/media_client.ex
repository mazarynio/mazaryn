defmodule Core.MediaClient do

  def insert_media(user_id, file) do
    :media_server.insert_media(user_id, file)
  end

  def delete_file(media_id) do
    :media_server.delete_file(media_id)
  end

  def get_media(media_id) do
    :media_server.get_media(media_id)
  end

  def get_all_media(user_id) do
    :media_server.get_all_media(user_id)
  end

  def report_media(my_id, media_id, type, description) do
    :media_server.report_media(my_id, media_id, type, description)
  end
end
