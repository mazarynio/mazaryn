defmodule Mazaryn.Schema.Report do
  @moduledoc """
  Embedded schema to represent Mazaryn.Schema.Report
  """

  use Ecto.Schema

  import Ecto.Changeset

  @optional_fields ~w(
    id
    type
    user
    post
    media
    date_created
    data
  )a

  @required_fields ~w(
    reporter
    description
  )a

  embedded_schema do
    field(:type, :string)
    field(:description, :string)
    field(:reporter, :string)
    field(:user, :string)
    field(:post, :string)
    field(:media, :string)
    field(:date_created, :utc_datetime)
    field(:data, :map)
  end

  def erl_changeset({:report, id, type, description, reporter, user, post, media,
   date_created, data}) do
    %__MODULE__{}
    |> change(%{
      id: id,
      type: type,
      description: description,
      reporter: reporter,
      user: user,
      post: post,
      media: media,
      date_created: handle_datetime(date_created),
      data: data
    })
  end

  def erl_changeset(_), do: %{}

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  def changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, @optional_fields ++ @required_fields)
    |> validate_required(@required_fields)
  end

  def update_changeset(%__MODULE__{} = struct, attrs \\ %{}) do
    struct
    |> cast(attrs, [:id, :description])
    |> validate_required([:id, :description])
  end

  def build(map) when map == %{}, do: %{}

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
