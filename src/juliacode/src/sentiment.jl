using Flux
using CSV
using DataFrames
using Random

# Set a random seed for reproducibility
Random.seed!(1234)

# Load positive and negative words from CSV files
function load_sentiment_words(file_path)
    df = CSV.read(file_path, DataFrame)
    return Dict(String(row.word) => row.weight for row in eachrow(df))
end

# Preprocess the text and calculate sentiment score
function preprocess_and_score(text::String, pos_words::Dict{String, Int}, neg_words::Dict{String, Int})
    tokens = split(lowercase(text), r"\s+")  # Tokenize and convert to lowercase
    score = 0
    for token in tokens
        if haskey(pos_words, token)
            score += pos_words[token]
        elseif haskey(neg_words, token)
            score += neg_words[token]
        end
    end
    return score
end

# Define the Model
function create_model()
    model = Chain(
        Dense(1, 64, relu),  # Input layer with 1 feature (sentiment score), output 64 neurons
        Dense(64, 2),        # Hidden layer with 64 neurons, output 2 neurons (positive/negative)
        softmax              # Softmax activation for classification
    )
    return model
end

# Training the Model
function train_model(pos_words::Dict{String, Int}, neg_words::Dict{String, Int})
    model = create_model()

    # Load combined training data
    data_df = CSV.read("training_data.csv", DataFrame)

    # Extract texts and labels
    texts = data_df.text
    labels_raw = data_df.label

    # Convert labels to one-hot encoding
    labels = Flux.onehotbatch(labels_raw, 1:2)  # Assumes labels are 1 or 2

    # Convert text to sentiment scores
    x_train = [preprocess_and_score(text, pos_words, neg_words) for text in texts]
    x_train = reshape(Float32[x_train...], 1, length(x_train))  # Convert to 2D array with 1 row

    # Define loss function and optimizer
    loss(x, y) = Flux.crossentropy(model(x), y)
    opt = ADAM()

    # Training loop
    for epoch in 1:100  # 100 epochs for training
        Flux.train!(loss, Flux.params(model), [(x_train, labels)], opt)
    end

    return model
end

# Predict Sentiment
function predict_sentiment(text::String, model, pos_words, neg_words)
    sentiment_score = preprocess_and_score(text, pos_words, neg_words)
    input_vector = reshape(Float32[sentiment_score], 1, 1)  # Convert to 2D array with 1 row and 1 column

    # Make predictions
    pred = model(input_vector)
    
    # Convert the prediction to a score between -1 and 1
    score = (pred[2] - pred[1]) / 2  # Assumes pred[1] is negative sentiment, pred[2] is positive
    return clamp(score, -1, 1)
end

# Run and Test
function main()
    pos_words = load_sentiment_words("positive_words.csv")
    neg_words = load_sentiment_words("negative_words.csv")

    model = train_model(pos_words, neg_words)
    text = "I really love this product, it's amazing!"
    sentiment = predict_sentiment(text, model, pos_words, neg_words)
    println("Sentiment: ", sentiment)
end

# Execute the main function
main()
