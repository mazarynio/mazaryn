defmodule Core.BusinessClient do

  def insert(user_id, company_name, industry, business_email) do
    :business_server.insert(user_id, company_name, industry, business_email)
  end

  def get_business_account_by_business_id(id) do
    :business_server.get_business_account_by_business_id(id)
  end

  def get_business_account_by_user_id(id) do
    :business_server.get_business_account_by_user_id(id)
  end

  def get_business_account_by_username(username)do
    :business_server.get_business_account_by_username(username)
  end
end
