defmodule Mazaryn.Schema.Media do
  use Ecto.Schema
  alias Timex
  import Ecto.Changeset

  @optional_fields ~w(
    id
    ai_media_id
    user_id
    files
    type
    date_created
    report
    data
  )a

  @required_fields ~w(
    file
  )a

  embedded_schema do
    field(:ai_media_id, :string)
    field(:file, {:array, :string}, default: [])
    field(:files, {:array, :string}, default: [])
    field(:user_id, :string)
    field(:type, :string)
    field(:date_created, :utc_datetime)
    field(:date_updated, :utc_datetime)
    field(:report, {:array, :string}, default: [])
    field(:data, :map)
  end

  def erl_changeset(
        {:media, id, ai_media_id, file, files, user_id, type, date_created, date_updated, report, data}
      ) do
    %__MODULE__{}
    |> change(%{
      id: id,
      ai_media_id: ai_media_id,
      file: file,
      files: files,
      user_id: user_id,
      type: type,
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      report: report,
      data: data
    })
  end

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(datetime), do: Timex.to_naive_datetime(datetime)

  def changeset(media, attrs \\ %{}) do
    media
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end
end
