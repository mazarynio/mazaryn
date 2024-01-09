defmodule ManagePost do
  ## Get post information by post_id
  def get_post(post_id) do
    :manage_post.get_post(post_id)
  end

  ## Remove post by post_id
  def delete_post(post_id) do
    :manage_post.delete_post(post_id)
  end
end
