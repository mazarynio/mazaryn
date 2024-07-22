defmodule MazarynWeb.HomeLive.ReportPostsComponent do
  use MazarynWeb, :live_component

  alias MazarynWeb.Component.SelectLive
  alias Core.PostClient

  @types [
    "Spam",
    "Nudity",
    "Hate Speech",
    "Violence",
    "Sale of Illegal goods",
    "Bullying/Harassment",
    "Intellectual Property violation",
    "Suicide/Self Harm",
    "Eating disorders",
    "Scam/Fraud",
    "False Information"
  ]

  def render(assigns) do
    ~H"""
    <div class="flex flex-col">
      <.form
        :let={f}
        for={%{}}
        as={:report_post}
        phx-submit="report"
        phx-target={@myself}
        class="flex flex-col justify-between align-center w-full m-0"
      >
        <div class="flex relative items-center p-0">
          <div>
            <%= live_component(SelectLive,
              id: "type",
              f: f,
              name: :type,
              options: @types
            ) %>
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

          <%= hidden_input(f, :post_id, value: @id) %>

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

  def handle_event("report", %{"report_post" => params}, socket) do
    user_id = socket.assigns.current_user.id
    post_id = params["post_id"] |> to_charlist

    PostClient.report_post(user_id, post_id, params["type"], params["description"])

    {:noreply,
     socket
     |> put_flash(:info, "Report submitted successfully!")
     |> push_navigate(
       to: Routes.live_path(socket, MazarynWeb.HomeLive.Home, socket.assigns.locale),
       replace: true
     )}
  end
end
