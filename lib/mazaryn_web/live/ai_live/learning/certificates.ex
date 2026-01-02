defmodule MazarynWeb.AiLive.Learning.Certificates do
  use MazarynWeb, :live_view
  import MazarynWeb.Live.Helper
  alias Account.Users
  alias Mazaryn.Learning.CertificateGenerator
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
        |> assign(certificates: load_user_certificates(user_id))
        |> assign(generating: false)

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
          |> assign(certificates: load_user_certificates(uid))
          |> assign(generating: false)

        {:ok, socket}

      {:error, _reason} ->
        {:ok,
         socket
         |> put_flash(:error, "User not found")
         |> redirect(to: "/en/login")}
    end
  end

  @impl true
  def handle_event("generate_certificate", %{"path_id" => path_id}, socket) do
    socket = assign(socket, generating: true)

    case generate_course_certificate(socket.assigns.user, path_id) do
      {:ok, certificate_id} ->
        {:noreply,
         socket
         |> assign(certificates: load_user_certificates(socket.assigns.user_id))
         |> assign(generating: false)
         |> put_flash(:info, "Certificate generated successfully!")}

      {:error, reason} ->
        {:noreply,
         socket
         |> assign(generating: false)
         |> put_flash(:error, "Failed to generate certificate: #{inspect(reason)}")}
    end
  end

  @impl true
  def handle_event("download_certificate", %{"certificate_id" => certificate_id}, socket) do
    case get_certificate_pdf(certificate_id) do
      {:ok, pdf_binary} ->
        {:noreply,
         socket
         |> push_event("download_pdf", %{
           filename: "certificate_#{certificate_id}.pdf",
           data: Base.encode64(pdf_binary)
         })}

      {:error, _reason} ->
        {:noreply, put_flash(socket, :error, "Failed to download certificate")}
    end
  end

  @impl true
  def handle_event("view_certificate", %{"path_id" => path_id}, socket) do
    locale = socket.assigns[:locale] || "en"
    {:noreply, push_navigate(socket, to: "/#{locale}/ai/learning/paths/#{path_id}")}
  end

  defp load_user_certificates(user_id) do
    try do
      case :learningdb.get_user_certificates(user_id) do
        certificates when is_list(certificates) ->
          Enum.map(certificates, fn cert ->
            path_id = elem(cert, 3)
            path = load_path_info(path_id)

            %{
              certificate_id: elem(cert, 1),
              user_id: elem(cert, 2),
              path_id: path_id,
              path_title: Map.get(path, :title, "Unknown Course"),
              issued_at: elem(cert, 5),
              completion_date: elem(cert, 6),
              pdf_stored: elem(cert, 7) != nil
            }
          end)
          |> Enum.sort_by(& &1.issued_at, :desc)

        _ ->
          []
      end
    rescue
      _ -> []
    end
  end

  defp load_path_info(path_id) do
    try do
      case :learningdb.get_learning_path(path_id) do
        path_tuple when is_tuple(path_tuple) ->
          case Mazaryn.Schema.Learning.erl_changeset(path_tuple) do
            changeset when is_map(changeset) ->
              Ecto.Changeset.apply_changes(changeset)

            _ ->
              %{title: "Unknown Course"}
          end

        _ ->
          %{title: "Unknown Course"}
      end
    rescue
      _ -> %{title: "Unknown Course"}
    end
  end

  defp generate_course_certificate(user, path_id) do
    try do
      path = load_path_info(path_id)
      user_name = "#{user.username}"
      course_title = path.title
      completion_date = format_date(DateTime.utc_now())
      certificate_id = CertificateGenerator.generate_certificate_id()

      case CertificateGenerator.generate_certificate(
             user_name,
             course_title,
             completion_date,
             certificate_id
           ) do
        {:ok, pdf_binary} ->
          :learningdb.save_certificate(
            certificate_id,
            user.id,
            path_id,
            pdf_binary,
            :os.system_time(:second),
            completion_date
          )

          {:ok, certificate_id}

        {:error, reason} ->
          {:error, reason}
      end
    rescue
      error ->
        Logger.error("Certificate generation error: #{inspect(error)}")
        {:error, :generation_failed}
    end
  end

  defp get_certificate_pdf(certificate_id) do
    try do
      case :learningdb.get_certificate(certificate_id) do
        {:ok, cert} ->
          pdf_binary = elem(cert, 7)
          {:ok, pdf_binary}

        _ ->
          {:error, :not_found}
      end
    rescue
      _ -> {:error, :retrieval_failed}
    end
  end

  defp format_date(datetime) do
    Calendar.strftime(datetime, "%B %d, %Y")
  end

  defp format_timestamp(timestamp) when is_integer(timestamp) do
    datetime = DateTime.from_unix!(timestamp)
    Calendar.strftime(datetime, "%b %d, %Y")
  end

  defp format_timestamp(_), do: "Unknown"
end
