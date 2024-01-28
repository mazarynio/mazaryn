defmodule MazarynWeb.HomeLive.ReportUserComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Component.SelectLive
  alias Core.UserClient

  @types [
    "Inappropriate Content",
    "Inpersonation",
    "Underage",
    "Account may have been hacked"
  ]

  def render(assigns) do
    ~H"""
    <div class="flex flex-col">
      <div class="form-header">Report User</div>
      <.form
        :let={f}
        for={%{}}
        as={:report_user}
        phx-submit="report"
        phx-target={@myself}
        class="flex flex-col justify-between align-center w-full m-0"
      >
        <div class="flex relative items-center p-0">
        <div>
          <label class="form-input-label">Type</label>
        </div>
          <div>
            <%= live_component(SelectLive,
              id: "type",
              f: f,
              name: :type,
              options: @types
            ) %>
          </div>

          <div>
              <label class="form-input-label">Description</label>
            </div>
          <div class="relative top-2.5 ml-1 text-sm leading-tight w-full">
            <%= textarea(
              f,
              :description,
              class:
                "w-full border-none resize-none focus:text-black focus:ring-0 font-normal leading-[24px] block placeholder:text-[#C5C7C8]",
              placeholder: "Type something"
            ) %>
          </div>

          <%= hidden_input(f, :user_id, value: @id) %>

          <%= submit("Submit",
            class:
              "bg-[#4385F5] text-white rounded-[10px] py-1.5 px-6 self-auto min-h-[40px] min-w-[100px]"
          ) %>
        </div>
      </.form>
    </div>
    """
  end

  def update(assigns, socket) do
    {:ok,
     socket
     |> assign(assigns)
     |> assign(types: @types)}
  end

  def handle_event("report", %{"report_user" => params}, socket) do
    reporter_id = socket.assigns.current_user.id
    user_id = params["user_id"] |> to_charlist

    UserClient.report_user(reporter_id, user_id, params["type"], params["description"])
    |> IO.inspect(label: "sniiiitch")

    {:noreply,
     socket
     |> put_flash(:info, "Thank You for reporting!")
     |> push_redirect(to: Routes.live_path(socket, MazarynWeb.UserLive.Profile, socket.assigns.reported_user.username))}
  end
end
