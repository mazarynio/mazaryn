defmodule AIPost do

  def tokenize(text) do
    text
    |> String.downcase()
    |> String.split(~r/\s+/)
  end

  def create_vocabulary(posts_text) when is_list(posts_text) do
    posts_text
    |> Enum.flat_map(&tokenize/1)
    |> Enum.uniq()
  end

  # Encode the text into a simple numeric format
  def encode_text(text) do
    tokenize(text)
    |> Enum.map(&String.to_charlist(&1))
    |> Enum.map(&Enum.sum/1)
    |> Nx.tensor()
    |> Nx.reshape({1, :auto})
  end

  # Define a simple neural network model
  def create_sentiment_model do
    Axon.input({nil, 1})
    |> Axon.dense(16, activation: :relu)
    |> Axon.dense(3, activation: :softmax)
  end
end
