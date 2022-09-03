# defmodule Account.UserToken do
#   use Ecto.Schema
#   import Ecto.Query
#   alias Account.UserNotifier

#   @hash_algorithm :sha256
#   @rand_size 32

#   @reset_password_validity_in_days 1
#   @confirm_validity_in_days 7
#   @change_email_validity_in_days 7
#   @session_validity_in_days 60

#   @optional_fields ~w(
#     sent_to
#   )a

#   @required_fields ~w(
#     token
#     context
#   )a

#   embedded_schema do
#     field(:token, :binary)
#     field(:context, :string)
#     field(:sent_to, :string)
#   end

#   def erl_changeset(
#         {:user_token, token, context, sent_to}
#       ) do
#     %__MODULE__{}
#     |> changeset(%{
#       token: token,
#       context: context,
#       sent_to: sent_to
#     })
#   end

#   def changeset(user_token, attrs \\ %{}) do
#     user_token
#     |> cast(attrs, @optional_fields ++ @required_fields)
#     |> validate_required(@required_fields)
#   end

#   def build(changeset) do
#     apply_action(changeset, :build)
#   end
# end
