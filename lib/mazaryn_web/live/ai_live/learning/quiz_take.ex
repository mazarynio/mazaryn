defmodule MazarynWeb.AiLive.Learning.QuizTake do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(
        %{"path_id" => path_id, "quiz_id" => quiz_id},
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)

      case load_quiz_data(quiz_id, path_id, user_id) do
        {:ok, data} ->
          socket =
            socket
            |> assign(user: user)
            |> assign(user_id: user_id)
            |> assign(session_uuid: session_uuid)
            |> assign(path_id: path_id)
            |> assign(quiz_id: quiz_id)
            |> assign(quiz: data.quiz)
            |> assign(questions: data.questions)
            |> assign(user_answers: %{})
            |> assign(current_question: 0)
            |> assign(quiz_started: false)
            |> assign(quiz_submitted: false)
            |> assign(time_remaining: data.quiz.time_limit * 60)
            |> assign(score: nil)
            |> assign(results: nil)
            |> assign(can_retake: data.can_retake)

          {:ok, socket}

        {:error, reason} ->
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, get_error_message(reason))
           |> redirect(to: "/#{locale}/ai/learning/paths/#{path_id}")}
      end
    else
      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "Session expired")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("start_quiz", _params, socket) do
    {:noreply, assign(socket, quiz_started: true)}
  end

  @impl true
  def handle_event("answer_question", %{"question_idx" => idx, "answer" => answer}, socket) do
    idx = String.to_integer(idx)
    answer = String.to_integer(answer)
    user_answers = Map.put(socket.assigns.user_answers, idx, answer)
    {:noreply, assign(socket, user_answers: user_answers)}
  end

  @impl true
  def handle_event("next_question", _params, socket) do
    current = socket.assigns.current_question
    total = length(socket.assigns.questions)

    if current < total - 1 do
      {:noreply, assign(socket, current_question: current + 1)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("prev_question", _params, socket) do
    current = socket.assigns.current_question

    if current > 0 do
      {:noreply, assign(socket, current_question: current - 1)}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("submit_quiz", _params, socket) do
    results = grade_quiz(socket.assigns)
    score = calculate_score(results)

    save_attempt(socket.assigns.user_id, socket.assigns.quiz_id, score, results)

    {:noreply,
     socket
     |> assign(quiz_submitted: true)
     |> assign(score: score)
     |> assign(results: results)}
  end

  @impl true
  def handle_event("retake_quiz", _params, socket) do
    {:noreply,
     socket
     |> assign(user_answers: %{})
     |> assign(current_question: 0)
     |> assign(quiz_started: false)
     |> assign(quiz_submitted: false)
     |> assign(time_remaining: socket.assigns.quiz.time_limit * 60)
     |> assign(score: nil)
     |> assign(results: nil)}
  end

  defp load_quiz_data(quiz_id, path_id, user_id) do
    try do
      case :learningdb.get_quiz(quiz_id) do
        {:error, :not_found} ->
          {:error, :quiz_not_found}

        quiz_tuple when is_tuple(quiz_tuple) ->
          quiz = %{
            id: elem(quiz_tuple, 1),
            lesson_id: elem(quiz_tuple, 2),
            title: elem(quiz_tuple, 4),
            description: elem(quiz_tuple, 5),
            time_limit: elem(quiz_tuple, 7),
            passing_score: elem(quiz_tuple, 8),
            allow_retakes: elem(quiz_tuple, 9),
            shuffle_questions: elem(quiz_tuple, 10)
          }

          questions_json = elem(quiz_tuple, 6)
          questions = Jason.decode!(questions_json)

          shuffled_questions =
            if quiz.shuffle_questions do
              Enum.shuffle(questions)
            else
              questions
            end

          previous_attempts = get_previous_attempts(user_id, quiz_id)
          can_retake = quiz.allow_retakes or length(previous_attempts) == 0

          {:ok,
           %{
             quiz: quiz,
             questions: shuffled_questions,
             can_retake: can_retake
           }}

        _ ->
          {:error, :invalid_quiz}
      end
    rescue
      _ -> {:error, :exception}
    end
  end

  defp grade_quiz(assigns) do
    questions = assigns.questions
    user_answers = assigns.user_answers

    Enum.with_index(questions, fn question, idx ->
      user_answer = Map.get(user_answers, idx)
      correct_answer = question["correct_answer"]
      is_correct = user_answer == correct_answer

      %{
        question_idx: idx,
        question_text: question["text"],
        user_answer: user_answer,
        correct_answer: correct_answer,
        is_correct: is_correct,
        points: if(is_correct, do: question["points"], else: 0),
        explanation: question["explanation"]
      }
    end)
  end

  defp calculate_score(results) do
    total_points = Enum.reduce(results, 0, fn r, acc -> acc + r.points end)

    possible_points =
      Enum.reduce(results, 0, fn r, acc -> acc + if r.is_correct, do: r.points, else: r.points end)

    if possible_points > 0 do
      Float.round(total_points / possible_points * 100, 1)
    else
      0.0
    end
  end

  defp save_attempt(user_id, quiz_id, score, results) do
    try do
      results_json = Jason.encode!(results)

      :learningdb.save_quiz_attempt(
        user_id,
        quiz_id,
        score,
        results_json,
        :os.system_time(:second)
      )
    rescue
      _ -> :ok
    end
  end

  defp get_previous_attempts(user_id, quiz_id) do
    try do
      case :learningdb.get_quiz_attempts(user_id, quiz_id) do
        attempts when is_list(attempts) -> attempts
        _ -> []
      end
    rescue
      _ -> []
    end
  end

  defp get_error_message(:quiz_not_found), do: "Quiz not found"
  defp get_error_message(_), do: "An error occurred"
end
