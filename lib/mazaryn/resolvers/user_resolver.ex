defmodule Resolvers.UserResolver do
  alias Account.Users

  defp format_user(user) when is_map(user) do
    user
    |> format_date_field(:date_created)
    |> format_date_field(:last_activity)
  end

  defp format_user(user), do: user

  defp format_date_field(user, field) do
    case Map.get(user, field) do
      {{year, month, day}, {hour, minute, second}} ->
        formatted_date = "#{year}-#{pad_zero(month)}-#{pad_zero(day)} #{pad_zero(hour)}:#{pad_zero(minute)}:#{pad_zero(second)}"
        Map.put(user, field, formatted_date)

      %DateTime{} = dt ->
        Map.put(user, field, DateTime.to_iso8601(dt))

      %NaiveDateTime{} = dt ->
        Map.put(user, field, NaiveDateTime.to_iso8601(dt))

      nil ->
        Map.put(user, field, nil)

      other when is_binary(other) ->
        user

      other ->
        Map.put(user, field, to_string(other))
    end
  end

  defp pad_zero(num) when num < 10, do: "0#{num}"
  defp pad_zero(num), do: to_string(num)

  def all(_parent, _args, _resolution) do
    users = Users.list_users()
    formatted_users = Enum.map(users, &format_user/1)
    IO.inspect(formatted_users, label: "Formatted Users")
    {:ok, formatted_users}
  end

  def create_user(_parent, args, _resolution) do
    username = args[:username]
    password = args[:password]
    email = args[:email]

    case Users.create_user(username, password, email) do
      {:ok, user} -> {:ok, format_user(user)}
      {:error, changeset} -> {:error, format_errors(changeset)}
    end
  end

  def find_user_by_id(_parent, %{id: id}, _resolution) do
    case Users.one_by_id(id) do
      nil -> {:error, "User not found"}
      user -> {:ok, format_user(user)}
    end
  end

  def find_user_by_username(_parent, %{username: username}, _resolution) do
    case Users.one_by_username(username) do
      nil -> {:error, "User not found"}
      user -> {:ok, format_user(user)}
    end
  end

  def find_user_by_email(_parent, %{email: email}, _resolution) do
    case Users.one_by_email(email) do
      nil -> {:error, "User not found"}
      user -> {:ok, format_user(user)}
    end
  end

  def user_login(_parent, args, _resolution) do
    email = args[:email]
    username = args[:username]
    password = args[:password]

    identifier = email || username

    case Users.authenticate_user(identifier, password) do
      {:ok, user} -> {:ok, [format_user(user)]}
      {:error, _reason} -> {:error, "Invalid credentials"}
    end
  end

  defp format_errors(changeset) do
    changeset
    |> Ecto.Changeset.traverse_errors(fn {msg, opts} ->
      Enum.reduce(opts, msg, fn {key, value}, acc ->
        String.replace(acc, "%{#{key}}", to_string(value))
      end)
    end)
  end
end
