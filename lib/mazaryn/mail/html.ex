defmodule Mail.HTML do

  defp templates_path, do: "lib/mazaryn_web/templates"

  def render_html_body(assigns, project, action) do
    assigns
    |> render_email(project, action)
    |> render_template()
  end

  defp render_email(assigns, project, action) do
    EEx.eval_file("#{templates_path()}/#{project}/#{action}.html.heex",
      assigns: assigns
    )
  end

  defp render_template(inner_content, template \\ "email_base") do
    EEx.eval_file("#{templates_path()}/layout/#{template}.html.heex",
      assigns: [inner_content: inner_content]
    )
  end
end
