defmodule MazarynWeb.AiLive.NotebookShowHelpers do
  def language_badge_color("python"), do: "px-2 py-0.5 rounded-full font-medium bg-blue-100 text-blue-700 border border-blue-200"
  def language_badge_color("julia"), do: "px-2 py-0.5 rounded-full font-medium bg-purple-100 text-purple-700 border border-purple-200"
  def language_badge_color("elixir"), do: "px-2 py-0.5 rounded-full font-medium bg-purple-100 text-purple-700 border border-purple-200"
  def language_badge_color("rust"), do: "px-2 py-0.5 rounded-full font-medium bg-orange-100 text-orange-700 border border-orange-200"
  def language_badge_color("r"), do: "px-2 py-0.5 rounded-full font-medium bg-blue-100 text-blue-700 border border-blue-200"
  def language_badge_color(_), do: "px-2 py-0.5 rounded-full font-medium bg-slate-100 text-slate-700 border border-slate-200"

  def language_gradient("python"), do: "bg-gradient-to-br from-blue-500 to-blue-600"
  def language_gradient("julia"), do: "bg-gradient-to-br from-purple-500 to-pink-500"
  def language_gradient("elixir"), do: "bg-gradient-to-br from-purple-600 to-indigo-600"
  def language_gradient("rust"), do: "bg-gradient-to-br from-orange-600 to-red-600"
  def language_gradient("r"), do: "bg-gradient-to-br from-blue-600 to-indigo-600"
  def language_gradient(_), do: "bg-gradient-to-br from-slate-500 to-slate-600"

  def language_gradient(language) when is_atom(language) do
    language_gradient(Atom.to_string(language))
  end

  def language_icon("python") do
    Phoenix.HTML.raw(~s|<svg class="w-6 h-6 text-white" viewBox="0 0 24 24" fill="currentColor"><path d="M14.25.18l.9.2.73.26.59.3.45.32.34.34.25.34.16.33.1.3.04.26.02.2-.01.13V8.5l-.05.63-.13.55-.21.46-.26.38-.3.31-.33.25-.35.19-.35.14-.33.1-.3.07-.26.04-.21.02H8.77l-.69.05-.59.14-.5.22-.41.27-.33.32-.27.35-.2.36-.15.37-.1.35-.07.32-.04.27-.02.21v3.06H3.17l-.21-.03-.28-.07-.32-.12-.35-.18-.36-.26-.36-.36-.35-.46-.32-.59-.28-.73-.21-.88-.14-1.05-.05-1.23.06-1.22.16-1.04.24-.87.32-.71.36-.57.4-.44.42-.33.42-.24.4-.16.36-.1.32-.05.24-.01h.16l.06.01h8.16v-.83H6.18l-.01-2.75-.02-.37.05-.34.11-.31.17-.28.25-.26.31-.23.38-.2.44-.18.51-.15.58-.12.64-.1.71-.06.77-.04.84-.02 1.27.05zm-6.3 1.98l-.23.33-.08.41.08.41.23.34.33.22.41.09.41-.09.33-.22.23-.34.08-.41-.08-.41-.23-.33-.33-.22-.41-.09-.41.09-.33.22zM21.1 6.11l.28.06.32.12.35.18.36.27.36.35.35.47.32.59.28.73.21.88.14 1.04.05 1.23-.06 1.23-.16 1.04-.24.86-.32.71-.36.57-.4.45-.42.33-.42.24-.4.16-.36.09-.32.05-.24.02-.16-.01h-8.22v.82h5.84l.01 2.76.02.36-.05.34-.11.31-.17.29-.25.25-.31.24-.38.2-.44.17-.51.15-.58.13-.64.09-.71.07-.77.04-.84.01-1.27-.04-1.07-.14-.9-.2-.73-.25-.59-.3-.45-.33-.34-.34-.25-.34-.16-.33-.1-.3-.04-.25-.02-.2.01-.13v-5.34l.05-.64.13-.54.21-.46.26-.38.3-.32.33-.24.35-.2.35-.14.33-.1.3-.06.26-.04.21-.02.13-.01h5.84l.69-.05.59-.14.5-.21.41-.28.33-.32.27-.35.2-.36.15-.36.1-.35.07-.32.04-.28.02-.21V6.07h2.09l.14.01.21.03zm-6.47 14.25l-.23.33-.08.41.08.41.23.33.33.23.41.08.41-.08.33-.23.23-.33.08-.41-.08-.41-.23-.33-.33-.23-.41-.08-.41.08-.33.23z"/></svg>|)
  end

  def language_icon("julia") do
    Phoenix.HTML.raw(~s|<svg class="w-6 h-6 text-white" viewBox="0 0 24 24" fill="currentColor"><circle cx="12" cy="7" r="3.5" fill="#389826"/><circle cx="6" cy="17" r="3.5" fill="#CB3C33"/><circle cx="18" cy="17" r="3.5" fill="#9558B2"/></svg>|)
  end

  def language_icon("elixir") do
    Phoenix.HTML.raw(~s|<svg class="w-6 h-6 text-white" viewBox="0 0 24 24" fill="currentColor"><path d="M19.793 16.575c0 3.752-2.927 7.426-7.743 7.426-5.249 0-7.843-3.71-7.843-7.426 0-5.218 4.571-9.569 6.833-14.833.248.984.917 2.398 1.992 3.895C14.428 8.203 19.793 11.759 19.793 16.575z"/></svg>|)
  end

  def language_icon("rust") do
    Phoenix.HTML.raw(~s|<svg class="w-6 h-6 text-white" viewBox="0 0 24 24" fill="currentColor"><path d="M23.834 11.666l-1.015-.61-.062-.618c-.096-.976-.288-1.93-.572-2.839l-.195-.625.545-.92a.214.214 0 0 0-.052-.283l-.8-.8a.213.213 0 0 0-.282-.052l-.921.545-.625-.195a11.904 11.904 0 0 0-2.838-.572l-.619-.062-.61-1.015a.212.212 0 0 0-.182-.105h-1.133a.213.213 0 0 0-.182.105l-.61 1.015-.618.062c-.977.096-1.93.288-2.839.572l-.625.195-.92-.545a.213.213 0 0 0-.283.052l-.8.8a.213.213 0 0 0-.052.282l.545.921-.195.625c-.284.908-.476 1.862-.572 2.838l-.062.619-1.015.61a.213.213 0 0 0-.105.182v1.133c0 .076.04.147.105.182l1.015.61.062.618c.096.977.288 1.93.572 2.839l.195.625-.545.92a.213.213 0 0 0 .052.283l.8.8c.076.076.206.087.282.052l.921-.545.625.195c.908.284 1.862.476 2.838.572l.619.062.61 1.015a.213.213 0 0 0 .182.105h1.133a.213.213 0 0 0 .182-.105l.61-1.015.618-.062c.977-.096 1.93-.288 2.839-.572l.625-.195.92.545a.213.213 0 0 0 .283-.052l.8-.8a.213.213 0 0 0 .052-.282l-.545-.921.195-.625c.284-.908.476-1.862.572-2.838l.062-.619 1.015-.61a.213.213 0 0 0 .105-.182v-1.133a.213.213 0 0 0-.105-.182z"/></svg>|)
  end

  def language_icon("r") do
    Phoenix.HTML.raw(~s|<svg class="w-6 h-6 text-white" viewBox="0 0 24 24" fill="currentColor"><path d="M12 0C5.373 0 0 5.373 0 12s5.373 12 12 12 12-5.373 12-12S18.627 0 12 0zm4.5 7.5h-9v9h9v-9z"/></svg>|)
  end

  def language_icon(_) do
    Phoenix.HTML.raw(~s|<svg class="w-6 h-6 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" stroke-width="2" d="M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4"/></svg>|)
  end

  def language_icon(language) when is_atom(language) do
    language_icon(Atom.to_string(language))
  end

  def language_display_name("python"), do: "Python"
  def language_display_name("julia"), do: "Julia"
  def language_display_name("elixir"), do: "Elixir"
  def language_display_name("rust"), do: "Rust"
  def language_display_name("r"), do: "R"
  def language_display_name(lang) when is_binary(lang), do: String.capitalize(lang)

  def language_display_name(language) when is_atom(language) do
    language_display_name(Atom.to_string(language))
  end

  def code_editor_class("python"), do: "bg-slate-50/50 focus:bg-white"
  def code_editor_class("julia"), do: "bg-purple-50/30 focus:bg-white"
  def code_editor_class("elixir"), do: "bg-purple-50/30 focus:bg-white"
  def code_editor_class("rust"), do: "bg-orange-50/30 focus:bg-white"
  def code_editor_class("r"), do: "bg-blue-50/30 focus:bg-white"
  def code_editor_class(_), do: "bg-slate-50/50 focus:bg-white"

  def code_editor_class(language) when is_atom(language) do
    code_editor_class(Atom.to_string(language))
  end

  def code_placeholder("python") do
    "# Write your Python code here...
import numpy as np
import pandas as pd

# Press Shift+Enter to execute"
  end

  def code_placeholder("julia") do
    "# Write your Julia code here...
using DataFrames
using Plots

# Press Shift+Enter to execute"
  end

  def code_placeholder("elixir") do
    "# Write your Elixir code here...
defmodule MyModule do
  def hello do
    IO.puts(\"Hello from Elixir!\")
  end
end

# Press Shift+Enter to execute"
  end

  def code_placeholder("rust") do
    "// Write your Rust code here...
println!(\"Hello from Rust!\");
let x = vec![1, 2, 3, 4, 5];
println!(\"Vector: {:?}\", x);

// Press Shift+Enter to execute"
  end

  def code_placeholder("r") do
    "# Write your R code here...
data <- c(1, 2, 3, 4, 5)
mean(data)

# Press Shift+Enter to execute"
  end

  def code_placeholder(_) do
    "# Write your code here...
# Press Shift+Enter to execute"
  end

  def code_placeholder(language) when is_atom(language) do
    code_placeholder(Atom.to_string(language))
  end
end
