defmodule Core.CompetitionClient do
  @moduledoc """
  Elixir wrapper for Erlang competitiondb module with safe error handling
  """
  require Logger

  def create_competition(
        creator_id,
        title,
        description,
        dataset_ids,
        start_time,
        end_time,
        reward_type,
        reward_value,
        rules,
        evaluation_metric,
        submission_limit,
        team_size_limit
      ) do
    try do
      :competitiondb.create_competition(
        to_charlist(creator_id),
        to_charlist(title),
        to_charlist(description),
        convert_dataset_ids(dataset_ids),
        start_time,
        end_time,
        reward_type,
        reward_value,
        rules,
        evaluation_metric,
        submission_limit,
        team_size_limit
      )
    rescue
      error ->
        Logger.error("Error creating competition: #{inspect(error)}")
        {:error, :creation_failed}
    end
  end

  def delete_competition(competition_id, user_id) do
    try do
      case :competitiondb.delete_competition(
             to_charlist(to_string(competition_id)),
             to_charlist(to_string(user_id))
           ) do
        :ok -> :ok
        {:error, reason} -> {:error, reason}
        _ -> {:error, :unknown_error}
      end
    rescue
      error ->
        Logger.error("Error deleting competition: #{inspect(error)}")
        {:error, :deletion_failed}
    end
  end

  def start_competition(competition_id, user_id) do
    safe_call(:start_competition, [
      to_charlist(to_string(competition_id)),
      to_charlist(to_string(user_id))
    ])
  end

  def end_competition(competition_id, user_id) do
    safe_call(:end_competition, [
      to_charlist(to_string(competition_id)),
      to_charlist(to_string(user_id))
    ])
  end

  def join_competition(competition_id, user_id) do
    safe_call(:join_competition, [
      to_charlist(to_string(competition_id)),
      to_charlist(to_string(user_id))
    ])
  end

  def leave_competition(competition_id, user_id) do
    safe_call(:leave_competition, [
      to_charlist(to_string(competition_id)),
      to_charlist(to_string(user_id))
    ])
  end

  def get_public_competitions do
    safe_call(:get_public_competitions, [])
  end

  def get_active_competitions do
    safe_call(:get_active_competitions, [])
  end

  def get_featured_competitions do
    safe_call(:get_featured_competitions, [])
  end

  def get_upcoming_competitions do
    safe_call(:get_upcoming_competitions, [])
  end

  def get_competitions_by_status(status) do
    safe_call(:get_competitions_by_status, [status])
  end

  def get_trending_competitions(limit) do
    safe_call(:get_trending_competitions, [limit])
  end

  def get_user_competition_history(user_id) do
    safe_call(:get_user_competition_history, [to_charlist(to_string(user_id))])
  end

  def get_competitions_by_creator(creator_id) do
    safe_call(:get_competitions_by_creator, [to_charlist(to_string(creator_id))])
  end

  def get_my_competitions(user_id) do
    try do
      :competitiondb.get_my_competitions(to_charlist(to_string(user_id)))
    rescue
      UndefinedFunctionError ->
        get_competitions_by_creator(user_id)

      error ->
        Logger.error("Error getting my competitions: #{inspect(error)}")
        []
    end
  end


  defp safe_call(function, args) do
    try do
      apply(:competitiondb, function, args)
    rescue
      UndefinedFunctionError ->
        Logger.error("Function :competitiondb.#{function} not found")
        {:error, :function_not_found}

      error ->
        Logger.error("Error calling :competitiondb.#{function}: #{inspect(error)}")
        {:error, :call_failed}
    end
  end

  defp convert_dataset_ids(dataset_ids) when is_list(dataset_ids) do
    Enum.map(dataset_ids, fn id ->
      to_charlist(to_string(id))
    end)
  end

  defp convert_dataset_ids(_), do: []
end
