defmodule MazarynWeb.AiLive.Learning.QuizNew do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  require Logger

  @impl true
  def mount(
        %{"path_id" => path_id, "module_id" => module_id, "lesson_id" => lesson_id},
        %{"session_uuid" => session_uuid} = _session,
        socket
      ) do
    with {:ok, user} <- Users.get_by_session_uuid(session_uuid) do
      user_id = if is_binary(user.id), do: user.id, else: to_string(user.id)

      if is_verified_instructor?(user_id) do
        case load_lesson_and_path(lesson_id, path_id, user_id) do
          {:ok, lesson, path} ->
            socket =
              socket
              |> assign(user: user)
              |> assign(user_id: user_id)
              |> assign(session_uuid: session_uuid)
              |> assign(path_id: path_id)
              |> assign(module_id: module_id)
              |> assign(lesson_id: lesson_id)
              |> assign(path: path)
              |> assign(lesson: lesson)
              |> assign_form_fields()
              |> assign(errors: %{})
              |> assign(submitting: false)

            {:ok, socket}

          {:error, _reason} ->
            locale = socket.assigns[:locale] || "en"

            {:ok,
             socket
             |> put_flash(:error, "Lesson not found or access denied")
             |> redirect(
               to: "/#{locale}/ai/learning/paths/#{path_id}/modules/#{module_id}/lessons"
             )}
        end
      else
        locale = socket.assigns[:locale] || "en"

        {:ok,
         socket
         |> put_flash(:error, "You must be a verified instructor")
         |> redirect(to: "/#{locale}/ai/learning")}
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
  def mount(
        %{"path_id" => path_id, "module_id" => module_id, "lesson_id" => lesson_id},
        %{"user_id" => user_id} = _session,
        socket
      ) do
    case Users.one_by_email(user_id) do
      {:ok, user} ->
        uid = if is_binary(user.id), do: user.id, else: to_string(user.id)

        if is_verified_instructor?(uid) do
          case load_lesson_and_path(lesson_id, path_id, uid) do
            {:ok, lesson, path} ->
              socket =
                socket
                |> assign(user: user)
                |> assign(user_id: uid)
                |> assign(path_id: path_id)
                |> assign(module_id: module_id)
                |> assign(lesson_id: lesson_id)
                |> assign(path: path)
                |> assign(lesson: lesson)
                |> assign_form_fields()
                |> assign(errors: %{})
                |> assign(submitting: false)

              {:ok, socket}

            {:error, _reason} ->
              locale = socket.assigns[:locale] || "en"

              {:ok,
               socket
               |> put_flash(:error, "Lesson not found or access denied")
               |> redirect(
                 to: "/#{locale}/ai/learning/paths/#{path_id}/modules/#{module_id}/lessons"
               )}
          end
        else
          locale = socket.assigns[:locale] || "en"

          {:ok,
           socket
           |> put_flash(:error, "You must be a verified instructor")
           |> redirect(to: "/#{locale}/ai/learning")}
        end

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("validate", params, socket) do
    socket = update_form_fields(socket, params)
    {:noreply, socket}
  end

  @impl true
  def handle_event("add_question", _params, socket) do
    question = %{
      id: generate_temp_id(),
      text: "",
      type: "multiple_choice",
      options: ["", "", "", ""],
      correct_answer: 0,
      points: 1,
      explanation: ""
    }

    questions = socket.assigns.questions ++ [question]
    {:noreply, assign(socket, questions: questions)}
  end

  @impl true
  def handle_event("remove_question", %{"index" => index}, socket) do
    index = String.to_integer(index)
    questions = List.delete_at(socket.assigns.questions, index)
    {:noreply, assign(socket, questions: questions)}
  end

  @impl true
  def handle_event(
        "update_question",
        %{"index" => index, "field" => field, "value" => value},
        socket
      ) do
    index = String.to_integer(index)
    questions = socket.assigns.questions

    updated_question =
      questions
      |> Enum.at(index)
      |> update_question_field(field, value)

    questions = List.replace_at(questions, index, updated_question)
    {:noreply, assign(socket, questions: questions)}
  end

  @impl true
  def handle_event(
        "update_option",
        %{"question_index" => q_idx, "option_index" => o_idx, "value" => value},
        socket
      ) do
    q_idx = String.to_integer(q_idx)
    o_idx = String.to_integer(o_idx)

    questions = socket.assigns.questions
    question = Enum.at(questions, q_idx)
    options = List.replace_at(question.options, o_idx, value)
    updated_question = Map.put(question, :options, options)
    questions = List.replace_at(questions, q_idx, updated_question)

    {:noreply, assign(socket, questions: questions)}
  end

  @impl true
  def handle_event("submit", params, socket) do
    socket = update_form_fields(socket, params)
    errors = validate_quiz(socket.assigns)

    if map_size(errors) == 0 do
      socket = assign(socket, submitting: true)

      case create_quiz(socket.assigns) do
        {:ok, quiz_id} ->
          locale = socket.assigns[:locale] || "en"

          {:noreply,
           socket
           |> put_flash(:info, "Quiz created successfully!")
           |> redirect(
             to:
               "/#{locale}/ai/learning/paths/#{socket.assigns.path_id}/modules/#{socket.assigns.module_id}/lessons"
           )}

        {:error, reason} ->
          {:noreply,
           socket
           |> assign(submitting: false)
           |> put_flash(:error, "Failed to create quiz: #{inspect(reason)}")}
      end
    else
      {:noreply, assign(socket, errors: errors)}
    end
  end

  defp assign_form_fields(socket) do
    socket
    |> assign(title: "")
    |> assign(description: "")
    |> assign(time_limit: "30")
    |> assign(passing_score: "70")
    |> assign(allow_retakes: true)
    |> assign(shuffle_questions: false)
    |> assign(questions: [])
  end

  defp is_verified_instructor?(user_id) do
    case :instructordb.is_verified_instructor(user_id) do
      true -> true
      _ -> false
    end
  rescue
    _ -> false
  end

  defp load_lesson_and_path(lesson_id, path_id, user_id) do
    try do
      case :learningdb.get_lesson(lesson_id) do
        {:error, :not_found} ->
          {:error, :not_found}

        lesson_tuple when is_tuple(lesson_tuple) ->
          lesson = %{
            id: elem(lesson_tuple, 1),
            title: elem(lesson_tuple, 3)
          }

          case :learningdb.get_learning_path(path_id) do
            {:error, :not_found} ->
              {:error, :not_found}

            path_tuple ->
              case Mazaryn.Schema.Learning.erl_changeset(path_tuple) do
                changeset when is_map(changeset) ->
                  path = Ecto.Changeset.apply_changes(changeset)

                  if path.creator_id == user_id do
                    {:ok, lesson, path}
                  else
                    {:error, :unauthorized}
                  end

                _ ->
                  {:error, :invalid_data}
              end
          end
      end
    rescue
      _ -> {:error, :exception}
    end
  end

  defp validate_quiz(assigns) do
    errors = %{}

    errors =
      if String.trim(assigns.title) == "",
        do: Map.put(errors, :title, "Quiz title is required"),
        else: errors

    errors =
      if length(assigns.questions) == 0,
        do: Map.put(errors, :questions, "Add at least one question"),
        else: errors

    errors =
      if length(assigns.questions) > 0 do
        invalid_questions =
          Enum.any?(assigns.questions, fn q ->
            String.trim(q.text) == "" or Enum.all?(q.options, &(String.trim(&1) == ""))
          end)

        if invalid_questions,
          do: Map.put(errors, :questions, "All questions must have text and options"),
          else: errors
      else
        errors
      end

    errors
  end

  defp update_form_fields(socket, params) do
    Enum.reduce(params, socket, fn {key, value}, acc ->
      case key do
        "title" -> assign(acc, title: value)
        "description" -> assign(acc, description: value)
        "time_limit" -> assign(acc, time_limit: value)
        "passing_score" -> assign(acc, passing_score: value)
        "allow_retakes" -> assign(acc, allow_retakes: value == "true")
        "shuffle_questions" -> assign(acc, shuffle_questions: value == "true")
        _ -> acc
      end
    end)
  end

  defp update_question_field(question, field, value) do
    case field do
      "text" -> Map.put(question, :text, value)
      "type" -> Map.put(question, :type, value)
      "points" -> Map.put(question, :points, parse_integer(value))
      "correct_answer" -> Map.put(question, :correct_answer, parse_integer(value))
      "explanation" -> Map.put(question, :explanation, value)
      _ -> question
    end
  end

  defp create_quiz(assigns) do
    lesson_id = assigns.lesson_id
    creator_id = assigns.user_id
    title = assigns.title
    description = assigns.description
    time_limit = parse_integer(assigns.time_limit)
    passing_score = parse_integer(assigns.passing_score)
    questions_data = Jason.encode!(assigns.questions)

    try do
      quiz_id =
        :learningdb.create_quiz(
          lesson_id,
          creator_id,
          title,
          description,
          questions_data,
          time_limit,
          passing_score,
          assigns.allow_retakes,
          assigns.shuffle_questions
        )

      {:ok, quiz_id}
    rescue
      error ->
        Logger.error("Exception creating quiz: #{inspect(error)}")
        {:error, :creation_failed}
    end
  end

  defp generate_temp_id do
    :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)
  end

  defp parse_integer(value) when is_binary(value) do
    case Integer.parse(value) do
      {int, _} -> int
      _ -> 0
    end
  end

  defp parse_integer(value) when is_integer(value), do: value
  defp parse_integer(_), do: 0
end
