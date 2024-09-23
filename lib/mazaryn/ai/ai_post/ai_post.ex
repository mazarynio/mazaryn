defmodule AIPost do
  alias Nx.Defn
  alias Axon

  # Define a simple neural network model for sentiment analysis
  def create_sentiment_model do
    # Single feature input
    Axon.input({nil, 1})
    # Hidden layer with ReLU
    |> Axon.dense(32, activation: :relu)
    # Output layer with 3 classes
    |> Axon.dense(3, activation: :softmax)
  end

  # Train the model with dummy data
  def train_model do
    # Dummy training data
    # Scaled inputs
    x_train = Nx.tensor([[0.2], [0.8], [0.5]], type: {:f, 32})
    # Labels: 0 = Negative, 1 = Positive, 2 = Neutral
    y_train = Nx.tensor([0, 1, 2], type: {:s, 64})

    model = create_sentiment_model()

    model
    |> Axon.compile(
      optimizer: Axon.Optimizers.adam(learning_rate: 0.01),
      loss: :sparse_categorical_crossentropy
    )
    |> Axon.train(x_train, y_train, epochs: 10, batch_size: 1)
  end

  # A simplified method to convert a single sentence to a numeric value (e.g., based on sentiment words)
  def encode_text(text) do
    case text do
      "positive" -> Nx.tensor([[0.8]], type: {:f, 32})
      "negative" -> Nx.tensor([[0.2]], type: {:f, 32})
      "neutral" -> Nx.tensor([[0.5]], type: {:f, 32})
      # Default to neutral
      _ -> Nx.tensor([[0.5]], type: {:f, 32})
    end
  end

  # Predict sentiment for a single post
  def predict_sentiment(text, model) do
    encoded_text = encode_text(text)
    predictions = model |> Axon.predict(encoded_text) |> Nx.to_flat_list()

    sentiment = Enum.max_by(0..2, fn i -> Enum.at(predictions, i) end)

    case sentiment do
      0 -> "Negative"
      1 -> "Positive"
      2 -> "Neutral"
    end
  end
end
