defmodule MazarynWeb.AiLive.Components.DeleteConfirmationModal do
  use Phoenix.LiveComponent
  require Logger

  def mount(socket) do
    {:ok, socket}
  end

  def update(%{model_title: model_title, show: show} = assigns, socket) do
    socket =
      socket
      |> assign(assigns)
      |> assign_new(:confirmation_text, fn -> "" end)
      |> assign_new(:can_confirm, fn -> false end)
      |> assign(show: show || false)

    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    <div
      id={@id}
      class={if @show, do: "block fixed inset-0 z-50 overflow-y-auto", else: "hidden"}
      phx-click-away="cancel_delete"
      phx-target={@myself}
    >
      <div class="flex items-center justify-center min-h-screen px-4 pt-4 pb-20 text-center sm:block sm:p-0">
        <div class="fixed inset-0 bg-gray-500 bg-opacity-75 transition-opacity" aria-hidden="true"></div>
        <span class="hidden sm:inline-block sm:align-middle sm:h-screen" aria-hidden="true">&#8203;</span>

        <div class="inline-block align-bottom bg-white rounded-2xl text-left overflow-hidden shadow-xl transform transition-all sm:my-8 sm:align-middle sm:max-w-lg sm:w-full">
          <div class="bg-white px-6 pt-5 pb-4 sm:p-6 sm:pb-4">
            <div class="sm:flex sm:items-start">
              <div class="mx-auto flex-shrink-0 flex items-center justify-center h-12 w-12 rounded-full bg-red-100 sm:mx-0 sm:h-10 sm:w-10">
                <svg class="h-6 w-6 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24" aria-hidden="true">
                  <path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z" />
                </svg>
              </div>

              <div class="mt-3 text-center sm:mt-0 sm:ml-4 sm:text-left">
                <h3 class="text-lg leading-6 font-medium text-gray-900" id="modal-title">
                  Delete Model
                </h3>
                <div class="mt-2">
                  <p class="text-sm text-gray-500">
                    Are you sure you want to permanently delete
                    <strong class="text-gray-900"><%= @model_title %></strong>?
                  </p>
                  <p class="mt-3 text-sm text-red-600 font-medium">
                    This action cannot be undone. All versions, metrics, deployments, and statistics will be lost.
                  </p>
                </div>
              </div>
            </div>

            <div class="mt-6">
              <label for="delete-confirmation" class="block text-sm font-medium text-gray-700">
                Type <span class="font-mono font-bold text-red-600">delete <%= @model_title %></span> to confirm:
              </label>
              <input
                id="delete-confirmation"
                type="text"
                value={@confirmation_text}
                phx-target={@myself}
                phx-keyup="update_confirmation"
                phx-debounce="100"
                placeholder={"delete #{@model_title}"}
                class="mt-1 block w-full border-gray-300 rounded-md shadow-sm focus:ring-red-500 focus:border-red-500 sm:text-sm font-mono"
                autofocus
              />
            </div>
          </div>

          <div class="bg-gray-50 px-6 py-3 sm:flex sm:flex-row-reverse">
            <button
              phx-click="confirm_delete"
              phx-target={@myself}
              disabled={!@can_confirm}
              class={"w-full inline-flex justify-center rounded-md border border-transparent shadow-sm px-6 py-3 text-base font-medium text-white focus:outline-none focus:ring-2 focus:ring-offset-2 sm:ml-3 sm:w-auto sm:text-sm #{if @can_confirm, do: "bg-red-600 hover:bg-red-700 focus:ring-red-500", else: "bg-gray-300 cursor-not-allowed"}"}
            >
              Delete Model
            </button>

            <button
              phx-click="cancel_delete"
              phx-target={@myself}
              type="button"
              class="mt-3 w-full inline-flex justify-center rounded-md border border-gray-300 shadow-sm px-6 py-3 bg-white text-base font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500 sm:mt-0 sm:ml-3 sm:w-auto sm:text-sm"
            >
              Cancel
            </button>
          </div>
        </div>
      </div>
    </div>
    """
  end

  def handle_event("update_confirmation", %{"value" => text}, socket) do
    expected = "delete " <> socket.assigns.model_title
    trimmed_text = String.trim(text)
    trimmed_expected = String.trim(expected)
    can_confirm = String.downcase(trimmed_text) == String.downcase(trimmed_expected)

    {:noreply,
     socket
     |> assign(confirmation_text: text)
     |> assign(can_confirm: can_confirm)}
  end

  def handle_event("confirm_delete", _params, socket) do
    if socket.assigns.can_confirm do
      target_pid = socket.parent_pid || socket.root_pid

      if target_pid do
        send(target_pid, {:delete_model_confirmed, socket.assigns.model_title})
      end
    end

    {:noreply, socket}
  end

  def handle_event("cancel_delete", _params, socket) do
    target_pid = socket.parent_pid || socket.root_pid

    if target_pid do
      send(target_pid, :close_delete_modal)
    end

    {:noreply, socket}
  end
end
