defmodule MazarynWeb.AiLive.Learning.MyLearning do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(_params, %{"session_uuid" => session_uuid} = _session, socket) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)

      socket =
        socket
        |> assign(user: user)
        |> assign(user_id: user_id)
        |> assign(session_uuid: session_uuid)
        |> assign(enrolled_courses: load_enrolled_courses(user_id))
        |> assign(active_filter: :all)
        |> assign(stats: calculate_stats(user_id))

      {:ok, socket}
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def mount(_params, %{"user_id" => user_id} = _session, socket) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        socket =
          socket
          |> assign(user: user)
          |> assign(user_id: uid)
          |> assign(enrolled_courses: load_enrolled_courses(uid))
          |> assign(active_filter: :all)
          |> assign(stats: calculate_stats(uid))

        {:ok, socket}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("filter_courses", %{"filter" => filter}, socket) do
    filter_atom = String.to_existing_atom(filter)
    filtered = filter_courses(socket.assigns.enrolled_courses, filter_atom)
    {:noreply, assign(socket, active_filter: filter_atom, filtered_courses: filtered)}
  end

  @impl true
  def handle_event("continue_learning", %{"path_id" => path_id}, socket) do
    locale = socket.assigns[:locale] || "en"
    {:noreply, push_navigate(socket, to: "/#{locale}/ai/learning/paths/#{path_id}")}
  end

  defp load_enrolled_courses(user_id) do
    try do
      case :learningdb.get_user_enrollments(user_id) do
        enrollments when is_list(enrollments) ->
          Enum.map(enrollments, fn enrollment ->
            path_id = elem(enrollment, 2)
            path = load_path_details(path_id)
            progress = calculate_course_progress(user_id, path_id)

            %{
              enrollment_id: elem(enrollment, 1),
              path_id: path_id,
              enrolled_at: elem(enrollment, 4),
              last_accessed: elem(enrollment, 5),
              path: path,
              progress: progress,
              status: determine_status(progress)
            }
          end)
          |> Enum.sort_by(& &1.last_accessed, :desc)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp load_path_details(path_id) do
    try do
      case :learningdb.get_learning_path(path_id) do
        path_tuple when is_tuple(path_tuple) ->
          case Mazaryn.Schema.Learning.erl_changeset(path_tuple) do
            changeset when is_map(changeset) ->
              Ecto.Changeset.apply_changes(changeset)

            _ ->
              %{title: "Unknown Course", description: "", category: "general"}
          end

        _ ->
          %{title: "Unknown Course", description: "", category: "general"}
      end
    rescue
      _ -> %{title: "Unknown Course", description: "", category: "general"}
    end
  end

  defp calculate_course_progress(user_id, path_id) do
    try do
      case :learningdb.get_user_progress(user_id, path_id) do
        %{progress_percentage: progress} when is_number(progress) ->
          Float.round(progress, 1)

        _ ->
          0.0
      end
    rescue
      _ -> 0.0
    end
  end

  defp determine_status(progress) when progress == 0.0, do: :not_started
  defp determine_status(progress) when progress >= 100.0, do: :completed
  defp determine_status(_progress), do: :in_progress

  defp calculate_stats(user_id) do
    enrollments = load_enrolled_courses(user_id)

    total = length(enrollments)
    completed = Enum.count(enrollments, &(&1.status == :completed))
    in_progress = Enum.count(enrollments, &(&1.status == :in_progress))
    not_started = Enum.count(enrollments, &(&1.status == :not_started))

    total_time_spent =
      Enum.reduce(enrollments, 0, fn course, acc ->
        path = course.path
        estimated_duration = Map.get(path, :estimated_duration, 0)
        progress = course.progress / 100.0
        acc + round(estimated_duration * progress)
      end)

    %{
      total: total,
      completed: completed,
      in_progress: in_progress,
      not_started: not_started,
      total_hours: div(total_time_spent, 3600)
    }
  end

  defp filter_courses(courses, :all), do: courses

  defp filter_courses(courses, :in_progress),
    do: Enum.filter(courses, &(&1.status == :in_progress))

  defp filter_courses(courses, :completed), do: Enum.filter(courses, &(&1.status == :completed))

  defp filter_courses(courses, :not_started),
    do: Enum.filter(courses, &(&1.status == :not_started))

  defp format_date(timestamp) when is_integer(timestamp) do
    datetime = DateTime.from_unix!(timestamp)
    Calendar.strftime(datetime, "%b %d, %Y")
  end

  defp format_date(_), do: "Unknown"
end
