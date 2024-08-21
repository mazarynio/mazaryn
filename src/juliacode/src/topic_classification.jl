using Flux
using CSV
using DataFrames
using TextAnalysis
using Statistics
using Random

# Set a random seed for reproducibility
Random.seed!(1234)

# Load dataset from CSV file
function load_dataset(file_path)
    df = CSV.read(file_path, DataFrame)
    texts = df.text
    labels = [split(label, ", ") for label in df.label]
    return texts, labels
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

# Convert text to TF-IDF vector manually
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
function create_model(input_size, num_topics)
    model = Chain(
        Dense(input_size, 128, relu),  # Input layer with input size equal to TF-IDF vector length
        Dense(128, 64, relu),          # Hidden layer with 64 neurons
        Dense(64, num_topics),         # Output layer with number of topics
        sigmoid                        # Sigmoid activation for multi-label classification
    )
    return model
end

# Train the Model
function train_model(documents, labels, vocab, topic_labels)
    model = create_model(length(vocab), length(topic_labels))

    # Convert documents to TF-IDF vectors
    x_train = text_to_tfidf(documents, vocab)

    # Convert labels to one-hot encoding using actual topic labels
    y_train = zeros(Float32, length(topic_labels), length(labels))
    for (i, label_set) in enumerate(labels)
        for label in label_set
            idx = findfirst(isequal(label), topic_labels)
            if idx !== nothing
                y_train[idx, i] = 1.0
            end
        end
    end

    # Define loss function and optimizer
    loss(x, y) = Flux.logitcrossentropy(model(x), y)
    opt = ADAM()

    # Training loop
    for epoch in 1:50  # Adjust epochs as needed
        Flux.train!(loss, Flux.params(model), [(x_train, y_train)], opt)
        println("Epoch $epoch: Loss = $(loss(x_train, y_train))")
    end

    return model
end

# Predict Topics
function predict_topics(text::String, model, vocab, topic_labels)
    processed_text = preprocess_text(text)
    tfidf_vector = text_to_tfidf([processed_text], vocab)[:, 1]
    pred = model(tfidf_vector)
    
    # Print predictions for debugging
    println("Predicted probabilities: ", pred)

    # Adjust threshold if needed
    threshold = 0.5
    pred_topics = topic_labels[pred .> threshold]
    
    # Print topics that exceed the threshold
    println("Predicted topics based on threshold: ", pred_topics)
    
    return pred_topics
end

function main()
    # Load dataset
    texts, labels = load_dataset("topic_classification.csv")
    
    # Preprocess texts
    processed_texts = [preprocess_text(text) for text in texts]

    # Create a vocabulary from all texts
    vocab = create_vocab(processed_texts)

    # Get unique topic labels from the dataset
    topic_labels = unique(vcat(labels...))

    # Train the model
    model = train_model(processed_texts, labels, vocab, topic_labels)

    # Example input
    text = "Hello guys this is my post on mazaryn social network after reading some books and playin Guitar"
    topics = predict_topics(text, model, vocab, topic_labels)
    println("Predicted Topics: ", topics)
end

main()
