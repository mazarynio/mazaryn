defmodule MazarynWeb.ErrorHelpers do
  @moduledoc """
  Conveniences for translating and building error messages.
  """

    import Phoenix.HTML
    import Phoenix.HTML.Form
    use PhoenixHTMLHelpers

  @doc """
  Generates tag for inlined form input errors.
  """
  def error_tag(form, field) do
    Enum.map(Keyword.get_values(form.errors, field), fn error ->
      content_tag(:span, translate_error(error),
        class: "invalid-feedback",
        phx_feedback_for: input_name(form, field)
      )
    end)
  end

  def push_error_tag(form, field) do
    Enum.map(Keyword.get_values(form.errors, field), fn error ->
      content_tag(:span, "#{translate_error(error)}",
        class: "block mt-1 text-sm text-red-700",
        phx_feedback_for: input_name(form, field)
      )
    end)
  end

  @doc """
  Translates an error message using gettext.
  """
  def translate_error({msg, opts}) do
    if count = opts[:count] do
      Gettext.dngettext(MazarynWeb.Gettext, "errors", msg, msg, count, opts)
    else
      Gettext.dgettext(MazarynWeb.Gettext, "errors", msg, opts)
    end
  end
end
