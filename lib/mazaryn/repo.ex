defmodule Mazaryn.Repo do
  use Ecto.Repo,
    otp_app: :mazaryn,
    adapter: Ecto.Adapters.Postgres
end
