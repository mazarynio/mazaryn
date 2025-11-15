defmodule Core.CompetitionClient do
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
    :competitiondb.create_competition(
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
    )
  end

  def update_competition(
        competition_id,
        creator_id,
        new_title,
        new_description,
        new_dataset_ids,
        new_start_time,
        new_end_time,
        new_reward_type,
        new_reward_value,
        new_rules,
        new_evaluation_metric,
        new_submission_limit,
        new_team_size_limit
      ) do
    :competitiondb.update_competition(
      competition_id,
      creator_id,
      new_title,
      new_description,
      new_dataset_ids,
      new_start_time,
      new_end_time,
      new_reward_type,
      new_reward_value,
      new_rules,
      new_evaluation_metric,
      new_submission_limit,
      new_team_size_limit
    )
  end

  def delete_competition(competition_id, user_id) do
    :competitiondb.delete_competition(competition_id, user_id)
  end

  def get_competition_by_id(competition_id) do
    :competitiondb.get_competition_by_id(competition_id)
  end

  def get_competitions_by_creator(creator_id) do
    :competitiondb.get_competitions_by_creator(creator_id)
  end

  def get_active_competitions() do
    :competitiondb.get_active_competitions()
  end

  def get_public_competitions() do
    :competitiondb.get_public_competitions()
  end

  def get_competitions_by_status(status) do
    :competitiondb.get_competitions_by_status(status)
  end

  def get_competitions_by_tag(tag) do
    :competitiondb.get_competitions_by_tag(tag)
  end

  def get_featured_competitions() do
    :competitiondb.get_featured_competitions()
  end

  def get_competitions_by_difficulty(difficulty_level) do
    :competitiondb.get_competitions_by_difficulty(difficulty_level)
  end

  def start_competition(competition_id, creator_id) do
    :competitiondb.start_competition(competition_id, creator_id)
  end

  def end_competition(competition_id, creator_id) do
    :competitiondb.end_competition(competition_id, creator_id)
  end

  def archive_competition(competition_id, creator_id) do
    :competitiondb.archive_competition(competition_id, creator_id)
  end

  def set_competition_status(competition_id, creator_id, new_status) do
    :competitiondb.set_competition_status(competition_id, creator_id, new_status)
  end

  def add_dataset_to_competition(competition_id, creator_id, dataset_id) do
    :competitiondb.add_dataset_to_competition(competition_id, creator_id, dataset_id)
  end

  def remove_dataset_from_competition(competition_id, creator_id, dataset_id) do
    :competitiondb.remove_dataset_from_competition(competition_id, creator_id, dataset_id)
  end

  def get_competition_datasets(competition_id) do
    :competitiondb.get_competition_datasets(competition_id)
  end

  def join_competition(competition_id, user_id) do
    :competitiondb.join_competition(competition_id, user_id)
  end

  def leave_competition(competition_id, user_id) do
    :competitiondb.leave_competition(competition_id, user_id)
  end

  def get_competition_participants(competition_id) do
    :competitiondb.get_competition_participants(competition_id)
  end

  def is_participant(competition_id, user_id) do
    :competitiondb.is_participant(competition_id, user_id)
  end

  def create_team(competition_id, creator_id, team_name, initial_members) do
    :competitiondb.create_team(competition_id, creator_id, team_name, initial_members)
  end

  def get_team_by_id(team_id) do
    :competitiondb.get_team_by_id(team_id)
  end

  def invite_to_team(team_id, inviter_id, invitee_id) do
    :competitiondb.invite_to_team(team_id, inviter_id, invitee_id)
  end

  def accept_team_invitation(team_id, user_id) do
    :competitiondb.accept_team_invitation(team_id, user_id)
  end

  def reject_team_invitation(team_id, user_id) do
    :competitiondb.reject_team_invitation(team_id, user_id)
  end

  def remove_team_member(team_id, remover_id, member_to_remove) do
    :competitiondb.remove_team_member(team_id, remover_id, member_to_remove)
  end

  def disband_team(team_id, captain_id) do
    :competitiondb.disband_team(team_id, captain_id)
  end

  def get_competition_teams(competition_id) do
    :competitiondb.get_competition_teams(competition_id)
  end

  def get_user_team_in_competition(competition_id, user_id) do
    :competitiondb.get_user_team_in_competition(competition_id, user_id)
  end

  def merge_teams(team_id1, team_id2, requester_id, competition_id) do
    :competitiondb.merge_teams(team_id1, team_id2, requester_id, competition_id)
  end

  def submit_entry(competition_id, user_id, submission_content, notebook_id, team_id) do
    :competitiondb.submit_entry(competition_id, user_id, submission_content, notebook_id, team_id)
  end

  def get_submission_by_id(submission_id) do
    :competitiondb.get_submission_by_id(submission_id)
  end

  def get_user_submissions(competition_id, user_id) do
    :competitiondb.get_user_submissions(competition_id, user_id)
  end

  def get_team_submissions(competition_id, team_id) do
    :competitiondb.get_team_submissions(competition_id, team_id)
  end

  def evaluate_submission(submission_id, score) do
    :competitiondb.evaluate_submission(submission_id, score)
  end

  def disqualify_submission(submission_id, admin_id, reason) do
    :competitiondb.disqualify_submission(submission_id, admin_id, reason)
  end

  def get_competition_submissions(competition_id) do
    :competitiondb.get_competition_submissions(competition_id)
  end

  def get_best_submission(competition_id, user_id) do
    :competitiondb.get_best_submission(competition_id, user_id)
  end

  def get_leaderboard(competition_id) do
    :competitiondb.get_leaderboard(competition_id)
  end

  def update_leaderboard(competition_id) do
    :competitiondb.update_leaderboard(competition_id)
  end

  def get_public_leaderboard(competition_id) do
    :competitiondb.get_public_leaderboard(competition_id)
  end

  def get_private_leaderboard(competition_id) do
    :competitiondb.get_private_leaderboard(competition_id)
  end

  def get_user_rank(competition_id, user_id) do
    :competitiondb.get_user_rank(competition_id, user_id)
  end

  def get_team_rank(competition_id, team_id) do
    :competitiondb.get_team_rank(competition_id, team_id)
  end

  def add_discussion(competition_id, creator_id, title, content) do
    :competitiondb.add_discussion(competition_id, creator_id, title, content)
  end

  def get_competition_discussions(competition_id) do
    :competitiondb.get_competition_discussions(competition_id)
  end

  def pin_discussion(discussion_id, admin_id, competition_id) do
    :competitiondb.pin_discussion(discussion_id, admin_id, competition_id)
  end

  def solve_discussion(discussion_id, admin_id, competition_id) do
    :competitiondb.solve_discussion(discussion_id, admin_id, competition_id)
  end

  def set_prize_distribution(competition_id, prize_map) do
    :competitiondb.set_prize_distribution(competition_id, prize_map)
  end

  def get_prize_distribution(competition_id) do
    :competitiondb.get_prize_distribution(competition_id)
  end

  def distribute_prizes(competition_id) do
    :competitiondb.distribute_prizes(competition_id)
  end

  def set_evaluation_script(competition_id, script_content) do
    :competitiondb.set_evaluation_script(competition_id, script_content)
  end

  def get_evaluation_script(competition_id) do
    :competitiondb.get_evaluation_script(competition_id)
  end

  def run_evaluation(submission_id, evaluator_id) do
    :competitiondb.run_evaluation(submission_id, evaluator_id)
  end

  def set_compute_quota(competition_id, quota_map) do
    :competitiondb.set_compute_quota(competition_id, quota_map)
  end

  def get_compute_quota(competition_id) do
    :competitiondb.get_compute_quota(competition_id)
  end

  def check_quota_available(competition_id, resource_type) do
    :competitiondb.check_quota_available(competition_id, resource_type)
  end

  def consume_quota(competition_id, resource_type, amount) do
    :competitiondb.consume_quota(competition_id, resource_type, amount)
  end

  def feature_competition(competition_id, admin_id) do
    :competitiondb.feature_competition(competition_id, admin_id)
  end

  def unfeature_competition(competition_id, admin_id) do
    :competitiondb.unfeature_competition(competition_id, admin_id)
  end

  def report_competition(reporter_id, competition_id, type, description) do
    :competitiondb.report_competition(reporter_id, competition_id, type, description)
  end

  def search_competitions(query) do
    :competitiondb.search_competitions(query)
  end

  def search_competitions_advanced(search_params) do
    :competitiondb.search_competitions_advanced(search_params)
  end

  def get_trending_competitions(limit) do
    :competitiondb.get_trending_competitions(limit)
  end

  def get_upcoming_competitions() do
    :competitiondb.get_upcoming_competitions()
  end

  def get_ending_soon_competitions(days_threshold) do
    :competitiondb.get_ending_soon_competitions(days_threshold)
  end

  def get_competition_stats(competition_id) do
    :competitiondb.get_competition_stats(competition_id)
  end

  def get_user_competition_history(user_id) do
    :competitiondb.get_user_competition_history(user_id)
  end

  def get_user_competition_stats(user_id) do
    :competitiondb.get_user_competition_stats(user_id)
  end

  def link_notebook_to_submission(submission_id, notebook_id) do
    :competitiondb.link_notebook_to_submission(submission_id, notebook_id)
  end

  def get_submission_notebook(submission_id) do
    :competitiondb.get_submission_notebook(submission_id)
  end
end
