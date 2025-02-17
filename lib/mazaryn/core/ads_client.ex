defmodule Client.Ads do
  def insert(user_id, ad_type, content) do
    :ads_server.insert(user_id, ad_type, content)
  end

  def get_ads_by_ads_id(id) do
    :ads_server.get_ads_by_ads_id(id)
  end

  def get_ads_by_user_id(id) do
    :ads_server.get_ads_by_user_id(id)
  end

  def get_ads_by_username(username) do
    :ads_server.get_ads_by_username(username)
  end
end
