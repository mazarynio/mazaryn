defmodule Mazaryn.Schema.Learning do
  use Ecto.Schema
  import Ecto.Changeset

  @optional_fields ~w(
    id
    title
    description
    creator_id
    creator_name
    creator_family
    approval_status
    approved_by
    approved_at
    rejection_reason
    content_quality_score
    resource_ids
    difficulty_level
    estimated_duration
    certificate_template_cid
    completions
    tags
    visibility
    date_created
    date_updated
    metadata
    track
    category
    subcategory
    prerequisite_paths
    next_paths
    enrollment_count
    completion_rate
    average_rating
    total_reviews
    instructors
    learning_outcomes
    career_outcomes
    job_roles
    industry_relevance
    certification_info
    price
    currency
    discount_info
    access_duration_days
    lifetime_access
    includes_mentorship
    includes_job_placement
    capstone_project_id
    final_assessment_id
    badges_earned
    partner_organizations
    accreditation
    ceu_credits
    featured
    trending
    new_release
    bestseller
    path_modules
    flexible_deadlines
    self_paced
    cohort_based
    live_classes
    recorded_sessions
    language
    subtitles_languages
    thumbnail_cid
    preview_video_cid
    instructor_bio
    instructor_credentials
    last_updated_by
    update_history
    flagged
    flag_reason
    student_feedback
    promotional_video_cid
    syllabus_pdf_cid
    ipns
    total_revenue
    completion_count
  )a

  @required_fields ~w()a

  embedded_schema do
    field(:title, :string)
    field(:description, :string)
    field(:creator_id, :string)
    field(:creator_name, :string)
    field(:creator_family, :string)
    field(:approval_status, :string, default: "pending")
    field(:approved_by, :string)
    field(:approved_at, :naive_datetime)
    field(:rejection_reason, :string)
    field(:content_quality_score, :float, default: 0.0)
    field(:resource_ids, {:array, :string}, default: [])
    field(:difficulty_level, :string)
    field(:estimated_duration, :integer)
    field(:certificate_template_cid, :string)
    field(:completions, :integer, default: 0)
    field(:tags, {:array, :string}, default: [])
    field(:visibility, :string, default: "draft")
    field(:date_created, :naive_datetime)
    field(:date_updated, :naive_datetime)
    field(:metadata, :map, default: %{})
    field(:track, :string)
    field(:category, :string)
    field(:subcategory, :string)
    field(:prerequisite_paths, {:array, :string}, default: [])
    field(:next_paths, {:array, :string}, default: [])
    field(:enrollment_count, :integer, default: 0)
    field(:completion_rate, :float, default: 0.0)
    field(:average_rating, :float, default: 0.0)
    field(:total_reviews, :integer, default: 0)
    field(:instructors, {:array, :string}, default: [])
    field(:learning_outcomes, {:array, :string}, default: [])
    field(:career_outcomes, {:array, :string}, default: [])
    field(:job_roles, {:array, :string}, default: [])
    field(:industry_relevance, {:array, :string}, default: [])
    field(:certification_info, :map, default: %{})
    field(:price, :float, default: 0.0)
    field(:currency, :string, default: "USD")
    field(:discount_info, :map, default: %{})
    field(:access_duration_days, :integer)
    field(:lifetime_access, :boolean, default: true)
    field(:includes_mentorship, :boolean, default: false)
    field(:includes_job_placement, :boolean, default: false)
    field(:capstone_project_id, :string)
    field(:final_assessment_id, :string)
    field(:badges_earned, {:array, :string}, default: [])
    field(:partner_organizations, {:array, :string}, default: [])
    field(:accreditation, {:array, :string}, default: [])
    field(:ceu_credits, :float, default: 0.0)
    field(:featured, :boolean, default: false)
    field(:trending, :boolean, default: false)
    field(:new_release, :boolean, default: false)
    field(:bestseller, :boolean, default: false)
    field(:path_modules, {:array, :string}, default: [])
    field(:flexible_deadlines, :boolean, default: true)
    field(:self_paced, :boolean, default: true)
    field(:cohort_based, :boolean, default: false)
    field(:live_classes, {:array, :string}, default: [])
    field(:recorded_sessions, :boolean, default: true)
    field(:language, :string, default: "en")
    field(:subtitles_languages, {:array, :string}, default: [])
    field(:thumbnail_cid, :string)
    field(:preview_video_cid, :string)
    field(:instructor_bio, :string)
    field(:instructor_credentials, {:array, :string}, default: [])
    field(:last_updated_by, :string)
    field(:update_history, {:array, :map}, default: [])
    field(:flagged, :boolean, default: false)
    field(:flag_reason, :string)
    field(:student_feedback, {:array, :map}, default: [])
    field(:promotional_video_cid, :string)
    field(:syllabus_pdf_cid, :string)
    field(:ipns, :string)
    field(:total_revenue, :float, default: 0.0)
    field(:completion_count, :integer, default: 0)
  end

  def erl_changeset(
        {:learning_path, id, title, description, creator_id, creator_name, creator_family,
         approval_status, approved_by, approved_at, rejection_reason, content_quality_score,
         resource_ids, difficulty_level, estimated_duration, certificate_template_cid,
         completions, tags, visibility, date_created, date_updated, metadata, track, category,
         subcategory, prerequisite_paths, next_paths, enrollment_count, completion_rate,
         average_rating, total_reviews, instructors, learning_outcomes, career_outcomes,
         job_roles, industry_relevance, certification_info, price, currency, discount_info,
         access_duration_days, lifetime_access, includes_mentorship, includes_job_placement,
         capstone_project_id, final_assessment_id, badges_earned, partner_organizations,
         accreditation, ceu_credits, featured, trending, new_release, bestseller, path_modules,
         flexible_deadlines, self_paced, cohort_based, live_classes, recorded_sessions, language,
         subtitles_languages, thumbnail_cid, preview_video_cid, instructor_bio,
         instructor_credentials, last_updated_by, update_history, flagged, flag_reason,
         student_feedback, promotional_video_cid, syllabus_pdf_cid, ipns, total_revenue,
         completion_count}
      ) do
    %__MODULE__{}
    |> change(%{
      id: to_string(id),
      title: to_string(title),
      description: process_cid(description),
      creator_id: to_string(creator_id),
      creator_name: to_string(creator_name),
      creator_family: to_string(creator_family),
      approval_status: atom_to_string(approval_status),
      approved_by: process_string(approved_by),
      approved_at: handle_datetime(approved_at),
      rejection_reason: process_string(rejection_reason),
      content_quality_score: to_float(content_quality_score),
      resource_ids: process_string_list(resource_ids),
      difficulty_level: atom_to_string(difficulty_level),
      estimated_duration: to_integer(estimated_duration),
      certificate_template_cid: process_string(certificate_template_cid),
      completions: to_integer(completions),
      tags: process_string_list(tags),
      visibility: atom_to_string(visibility),
      date_created: handle_datetime(date_created),
      date_updated: handle_datetime(date_updated),
      metadata: process_map(metadata),
      track: to_string(track),
      category: to_string(category),
      subcategory: to_string(subcategory),
      prerequisite_paths: process_string_list(prerequisite_paths),
      next_paths: process_string_list(next_paths),
      enrollment_count: to_integer(enrollment_count),
      completion_rate: to_float(completion_rate),
      average_rating: to_float(average_rating),
      total_reviews: to_integer(total_reviews),
      instructors: process_string_list(instructors),
      learning_outcomes: process_string_list(learning_outcomes),
      career_outcomes: process_string_list(career_outcomes),
      job_roles: process_string_list(job_roles),
      industry_relevance: process_string_list(industry_relevance),
      certification_info: process_map(certification_info),
      price: to_float(price),
      currency: to_string(currency),
      discount_info: process_map(discount_info),
      access_duration_days: process_integer(access_duration_days),
      lifetime_access: to_boolean(lifetime_access),
      includes_mentorship: to_boolean(includes_mentorship),
      includes_job_placement: to_boolean(includes_job_placement),
      capstone_project_id: process_string(capstone_project_id),
      final_assessment_id: process_string(final_assessment_id),
      badges_earned: process_string_list(badges_earned),
      partner_organizations: process_string_list(partner_organizations),
      accreditation: process_string_list(accreditation),
      ceu_credits: to_float(ceu_credits),
      featured: to_boolean(featured),
      trending: to_boolean(trending),
      new_release: to_boolean(new_release),
      bestseller: to_boolean(bestseller),
      path_modules: process_string_list(path_modules),
      flexible_deadlines: to_boolean(flexible_deadlines),
      self_paced: to_boolean(self_paced),
      cohort_based: to_boolean(cohort_based),
      live_classes: process_string_list(live_classes),
      recorded_sessions: to_boolean(recorded_sessions),
      language: to_string(language),
      subtitles_languages: process_string_list(subtitles_languages),
      thumbnail_cid: process_cid(thumbnail_cid),
      preview_video_cid: process_cid(preview_video_cid),
      instructor_bio: process_string(instructor_bio),
      instructor_credentials: process_string_list(instructor_credentials),
      last_updated_by: process_string(last_updated_by),
      update_history: process_list(update_history),
      flagged: to_boolean(flagged),
      flag_reason: process_string(flag_reason),
      student_feedback: process_list(student_feedback),
      promotional_video_cid: process_string(promotional_video_cid),
      syllabus_pdf_cid: process_cid(syllabus_pdf_cid),
      ipns: process_string(ipns),
      total_revenue: to_float(total_revenue),
      completion_count: to_integer(completion_count)
    })
  end

  def erl_changeset(_), do: change(%__MODULE__{}, %{})

  defp handle_datetime(:undefined), do: nil
  defp handle_datetime(nil), do: nil

  defp handle_datetime({{year, month, day}, {hour, minute, second}}) do
    case NaiveDateTime.new(year, month, day, hour, minute, second) do
      {:ok, datetime} -> datetime
      _ -> nil
    end
  end

  defp handle_datetime(datetime) when is_struct(datetime, NaiveDateTime), do: datetime
  defp handle_datetime(_), do: nil

  defp atom_to_string(atom) when is_atom(atom), do: Atom.to_string(atom)
  defp atom_to_string(string) when is_binary(string), do: string
  defp atom_to_string(list) when is_list(list), do: to_string(list)
  defp atom_to_string(_), do: nil

  defp process_string(:undefined), do: nil
  defp process_string(nil), do: nil
  defp process_string(value) when is_binary(value), do: value
  defp process_string(value) when is_list(value), do: to_string(value)
  defp process_string(_), do: nil

  defp process_cid({:pending, _id}), do: nil
  defp process_cid({:pending_update, _id}), do: nil
  defp process_cid({:pending_version, _id}), do: nil
  defp process_cid({:error, _}), do: nil
  defp process_cid(:undefined), do: nil
  defp process_cid(nil), do: nil
  defp process_cid(cid) when is_binary(cid), do: cid
  defp process_cid(cid) when is_list(cid), do: to_string(cid)
  defp process_cid(_), do: nil

  defp process_list(list) when is_list(list), do: list
  defp process_list(:undefined), do: []
  defp process_list(nil), do: []
  defp process_list(_), do: []

  defp process_string_list(list) when is_list(list) do
    Enum.map(list, fn
      item when is_binary(item) -> item
      item when is_list(item) -> to_string(item)
      item when is_atom(item) -> Atom.to_string(item)
      _ -> nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  defp process_string_list(:undefined), do: []
  defp process_string_list(nil), do: []
  defp process_string_list(_), do: []

  defp process_map(map) when is_map(map) do
    Enum.reduce(map, %{}, fn {key, value}, acc ->
      string_key =
        case key do
          k when is_atom(k) -> Atom.to_string(k)
          k when is_binary(k) -> k
          k when is_list(k) -> to_string(k)
          k -> inspect(k)
        end

      processed_value =
        case value do
          v when is_list(v) ->
            if Enum.all?(v, &is_integer/1) and length(v) > 0 do
              try do
                to_string(v)
              rescue
                _ -> v
              end
            else
              v
            end

          v ->
            v
        end

      Map.put(acc, string_key, processed_value)
    end)
  end

  defp process_map(:undefined), do: %{}
  defp process_map(nil), do: %{}
  defp process_map(_), do: %{}

  defp to_float(value) when is_float(value), do: value
  defp to_float(value) when is_integer(value), do: value * 1.0
  defp to_float(_), do: 0.0

  defp to_integer(value) when is_integer(value), do: value
  defp to_integer(:undefined), do: 0
  defp to_integer(nil), do: 0
  defp to_integer(_), do: 0

  defp process_integer(:undefined), do: nil
  defp process_integer(nil), do: nil
  defp process_integer(value) when is_integer(value), do: value
  defp process_integer(_), do: nil

  defp to_boolean(true), do: true
  defp to_boolean(false), do: false
  defp to_boolean(:undefined), do: false
  defp to_boolean(nil), do: false
  defp to_boolean(_), do: false

  def changeset(learning, attrs \\ %{}) do
    learning
    |> cast(attrs, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def build(changeset) do
    apply_action(changeset, :build)
  end

  def is_approved?(%__MODULE__{approval_status: "approved"}), do: true
  def is_approved?(_), do: false

  def is_pending?(%__MODULE__{approval_status: "pending"}), do: true
  def is_pending?(_), do: false

  def is_rejected?(%__MODULE__{approval_status: "rejected"}), do: true
  def is_rejected?(_), do: false

  def is_public?(%__MODULE__{visibility: "public"}), do: true
  def is_public?(_), do: false

  def is_draft?(%__MODULE__{visibility: "draft"}), do: true
  def is_draft?(_), do: false

  def is_featured?(%__MODULE__{featured: true}), do: true
  def is_featured?(_), do: false

  def is_free?(%__MODULE__{price: price}) when price == 0.0, do: true
  def is_free?(_), do: false

  def is_paid?(%__MODULE__{price: price}) when price > 0.0, do: true
  def is_paid?(_), do: false

  def format_price(%__MODULE__{price: price, currency: currency}) do
    "#{currency} #{Float.round(price, 2)}"
  end

  def format_duration(%__MODULE__{estimated_duration: duration}) when is_integer(duration) do
    cond do
      duration >= 3600 ->
        hours = div(duration, 3600)
        "#{hours} hours"

      duration >= 60 ->
        minutes = div(duration, 60)
        "#{minutes} minutes"

      true ->
        "#{duration} seconds"
    end
  end

  def format_duration(_), do: "N/A"

  def completion_percentage(%__MODULE__{enrollment_count: 0}), do: 0.0

  def completion_percentage(%__MODULE__{
        enrollment_count: enrolled,
        completion_count: completed
      })
      when enrolled > 0 do
    completed / enrolled * 100.0
  end

  def completion_percentage(_), do: 0.0

  def quality_tier(%__MODULE__{content_quality_score: score}) when is_number(score) do
    cond do
      score >= 90 -> :excellent
      score >= 75 -> :good
      score >= 50 -> :average
      score >= 25 -> :below_average
      true -> :poor
    end
  end

  def quality_tier(_), do: :unknown

  def format_datetime(nil), do: "N/A"

  def format_datetime(%NaiveDateTime{} = datetime) do
    Calendar.strftime(datetime, "%Y-%m-%d %H:%M:%S")
  end

  def format_datetime(_), do: "N/A"
end
