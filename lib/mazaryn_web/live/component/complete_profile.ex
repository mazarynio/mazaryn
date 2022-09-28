defmodule MazarynWeb.Component.CompleteProfile do
  use MazarynWeb, :live_component

  def mount(socket) do
    {:ok, socket}
  end

  def profile_percentage(user) do
    cond do
      is_email_verified?(user) and is_username_set?(user) and is_avatar_set?(user) -> 100
      is_email_verified?(user) and is_username_set?(user) -> 45
      is_email_verified?(user) -> 20
      true -> 0
    end
  end

  def verified_email(user),
    do: if(is_email_verified?(user), do: "text-gray-300", else: "text-gray-500")

  def username_set(user),
    do: if(is_username_set?(user), do: "text-gray-300", else: "text-gray-500")

  def avatar_set(user), do: if(is_avatar_set?(user), do: "text-gray-300", else: "text-gray-500")

  def is_email_verified?(user), do: if(user.email, do: true, else: false)
  def is_username_set?(user), do: if(user.username, do: true, else: false)
  def is_avatar_set?(user), do: if(user.avatar_url, do: true, else: false)
end
