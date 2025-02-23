defmodule TranslationClient do
  @base_url "http://localhost:8000"

  def translate_text(text, src_lang \\ "en", dest_lang \\ "az") do
    request_body = %{
      text: text,
      src_lang: src_lang,
      dest_lang: dest_lang
    }

    headers = [{"Content-Type", "application/json"}]

    case HTTPoison.post("#{@base_url}/translate", Jason.encode!(request_body), headers) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        response = Jason.decode!(body)
        response["translated_text"]

      {:ok, %HTTPoison.Response{status_code: status_code, body: body}} ->
        "HTTP error: #{status_code}, #{body}"

      {:error, reason} ->
        "Error: #{reason}"
    end
  end
end
