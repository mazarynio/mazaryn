defmodule Mazaryn.Translator do
  @moduledoc """
  Wrapper around the Erlang translator `post_text2:translate_only_print/3`.

  - Default src_lang "en"
  - Supported targets: "ru", "it", "es", "az"
  - Returns {:ok, translated_text} | {:error, reason}
  """

  @default_src "en"
  @supported_targets ~w(ru it es az)

  def default_src, do: @default_src
  def supported_targets, do: @supported_targets

  @spec translate(String.t(), String.t() | nil, String.t()) ::
          {:ok, String.t()} | {:error, term()}
  def translate(text, src_lang \\ nil, target_lang)

  def translate(text, src_lang, target_lang)
      when is_binary(text) and byte_size(text) > 0 and is_binary(target_lang) do
    src = (src_lang || @default_src) |> String.downcase()
    tgt = String.downcase(target_lang)

    if tgt in @supported_targets do
      translate_via_print_capture(text, src, tgt)
    else
      {:error, :unsupported_language}
    end
  end

  def translate(_, _, _), do: {:error, :invalid_input}

  defp translate_via_print_capture(text, src, tgt) do
    {:ok, io} = StringIO.open("")
    gl = :erlang.group_leader()

    try do
      :erlang.group_leader(io, self())

      :post_text2.translate_only_print(
        String.to_charlist(text),
        String.to_charlist(src),
        String.to_charlist(tgt)
      )

      {_in, out} = StringIO.contents(io)

      translated =
        out
        |> to_string()
        |> String.trim()
        |> String.split("\n")
        |> Enum.reject(fn line -> String.downcase(String.trim(line)) == "ok" end)
        |> Enum.join("\n")
        |> String.trim()

      if translated == "", do: {:error, :empty_output}, else: {:ok, translated}
    catch
      kind, reason -> {:error, {kind, reason}}
    rescue
      e -> {:error, e}
    after
      :erlang.group_leader(gl, self())
    end
  end
end
