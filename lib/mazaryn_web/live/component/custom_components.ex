defmodule MazarynWeb.Component.CustomComponents do
  @moduledoc false

  use Phoenix.Component

  attr :upload, Phoenix.LiveView.UploadConfig, required: true

  attr :accept, :string

  attr :rest, :global, include: ~w(webkitdirectory required disabled)

  @doc """
    Renders a file input.

    It's a copy of Phoenix.Component.live_file_input/1, with custom styles

    It exposes the ability for further customization, as required
  """
  def custom_live_file_input(%{upload: upload} = assigns) do
    assigns = assign_new(assigns, :accept, fn -> upload.accept != :any && upload.accept end)

    ~H"""
    <input
      type="file"
      class="block w-[100%] text-sm text-[#60616D] file:mr-2 file:py-2 file:px-2 file:rounded-md file:border-0 file:text-sm file:font-semibold file:bg-[#FAFAFA] file:text-[#5C5F63]"
      id={@upload.ref}
      type="file"
      name={@upload.name}
      accept={@accept}
      data-phx-hook="Phoenix.LiveFileUpload"
      data-phx-update="ignore"
      data-phx-upload-ref={@upload.ref}
      data-phx-active-refs={join_refs(for(entry <- @upload.entries, do: entry.ref))}
      data-phx-done-refs={join_refs(for(entry <- @upload.entries, entry.done?, do: entry.ref))}
      data-phx-preflighted-refs={
        join_refs(for(entry <- @upload.entries, entry.preflighted?, do: entry.ref))
      }
      data-phx-auto-upload={@upload.auto_upload?}
      {if @upload.max_entries > 1, do: Map.put(@rest, :multiple, true), else: @rest}
    />
    """
  end

  defp join_refs(entries), do: Enum.join(entries, ",")
end
