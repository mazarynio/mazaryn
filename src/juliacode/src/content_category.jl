using CSV
using DataFrames
using TextAnalysis
using Flux
using Statistics
using Random
using SparseArrays

# Set a random seed for reproducibility
Random.seed!(1234)

# Load dataset from CSV file
function load_dataset(file_path)
    df = CSV.read(file_path, DataFrame)
    texts = df.text
    categories = df.category
    return texts, categories
end

# Preprocess text (tokenization, lowercase)
function preprocess_text(text::String)
    tokens = split(lowercase(text), r"\s+")
    return Document(tokens)
end

# Create a vocabulary from documents
function create_vocab(documents)
    return unique(vcat([d.tokens for d in documents]...))
end

# Convert text to TF-IDF vector
function text_to_tfidf(documents, vocab)
    tfidf_vectors = []
    for doc in documents
        tfidf_vector = zeros(Float32, length(vocab))
        for (i, word) in enumerate(vocab)
            tfidf_vector[i] = tf(doc, word) * idf(documents, word)
        end
        push!(tfidf_vectors, tfidf_vector)
    end
    return hcat(tfidf_vectors...)
end

# TF-IDF calculation
function tf(doc, word)
    count = sum(w == word for w in doc.tokens)
    return count / length(doc.tokens)
end

function idf(documents, word)
    doc_count = sum(word in d.tokens for d in documents)
    return log(length(documents) / (1 + doc_count))
end

# Define the Model
function create_model(input_size, num_categories)
    model = Chain(
        Dense(input_size, 128, relu),  # Input layer with input size equal to TF-IDF vector length
        Dense(128, 64, relu),          # Hidden layer with 64 neurons
        Dense(64, num_categories),     # Output layer with number of categories
        softmax                        # Softmax activation for multi-class classification
    )
    return model
end

# Train the Model
function train_model(X_train, y_train, input_size, num_categories)
    model = create_model(input_size, num_categories)

    # Define loss function and optimizer
    loss(x, y) = Flux.crossentropy(model(x), y)
    opt = ADAM()

    # Training loop
    for epoch in 1:50  # Adjust epochs as needed
        Flux.train!(loss, Flux.params(model), [(X_train, y_train)], opt)
        println("Epoch $epoch: Loss = $(loss(X_train, y_train))")
    end

    return model
end

# Predict Categories
function predict_category(text::String, model, vocab, category_labels)
    processed_text = preprocess_text(text)
    tfidf_vector = text_to_tfidf([processed_text], vocab)[:, 1]
    pred = model(tfidf_vector)

    # Get the predicted category
    pred_category = category_labels[argmax(pred)]
    return pred_category
end

function main()
    # Load dataset
    texts, categories = load_dataset("content_category.csv")
    
    # Preprocess texts
    processed_texts = [preprocess_text(text) for text in texts]

    # Create a vocabulary from all texts
    vocab = create_vocab(processed_texts)

    # Convert texts to TF-IDF vectors
    X = text_to_tfidf(processed_texts, vocab)

    # Convert categories to one-hot encoding
    category_labels = unique(categories)
    num_categories = length(category_labels)
    y = zeros(Float32, num_categories, length(categories))
    for (i, category) in enumerate(categories)
        idx = findfirst(isequal(category), category_labels)
        if idx !== nothing
            y[idx, i] = 1.0
        end
    end

    # Split data into training and testing sets
    train_size = Int(0.8 * size(X, 2))
    X_train = X[:, 1:train_size]
    y_train = y[:, 1:train_size]
    X_test = X[:, train_size+1:end]
    y_test = y[:, train_size+1:end]

    # Train the model
    model = train_model(X_train, y_train, size(X, 1), num_categories)

    # Example input
    text = "What are the best tips for studying effectively?"
    category = predict_category(text, model, vocab, category_labels)
    println("Predicted Category: ", category)
end

main()
