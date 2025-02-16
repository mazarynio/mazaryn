defmodule Translator do
  @api_url "http://localhost:8000/translate"

  def translate_text(text, src_lang \\ "en", dest_lang \\ "it") do
    payload = %{
      text: text,
      src_lang: src_lang,
      dest_lang: dest_lang
    }

    headers = [{"Content-Type", "application/json"}]

    options = [recv_timeout: 999_000]

    case HTTPoison.post(@api_url, Jason.encode!(payload), headers, options) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        # Strip extra quotes from the translated text
        String.trim(body, "\"")

      {:ok, %HTTPoison.Response{status_code: status_code, body: body}} ->
        raise "HTTP error: #{status_code}, #{body}"

      {:error, reason} ->
        raise "HTTP request failed: #{reason}"
    end
  end
end
