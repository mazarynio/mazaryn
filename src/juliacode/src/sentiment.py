import tensorflow as tf
import pandas as pd
import numpy as np

# Load positive and negative words from CSV files
def load_sentiment_words(file_path):
    df = pd.read_csv(file_path)
    return {row['word']: row['weight'] for _, row in df.iterrows()}

# Preprocess the text and calculate sentiment score
def preprocess_and_score(text, pos_words, neg_words):
    tokens = text.lower().split()
    score = 0
    for token in tokens:
        if token in pos_words:
            score += pos_words[token]
        elif token in neg_words:
            score += neg_words[token]
    return score

# Define the Model
def create_model():
    model = tf.keras.Sequential([
        tf.keras.layers.Dense(64, activation='relu', input_shape=(1,)),
        tf.keras.layers.Dense(2, activation='softmax')
    ])
    return model

# Training the Model
def train_model(pos_words, neg_words, file_path):
    model = create_model()
    
    # Load combined training data
    data_df = pd.read_csv(file_path)

    # Extract texts and labels
    texts = data_df['text']
    labels_raw = data_df['label'] - 1  # Assuming labels are 1 or 2, convert to 0 or 1 for TensorFlow

    # Convert text to sentiment scores
    x_train = np.array([preprocess_and_score(text, pos_words, neg_words) for text in texts]).reshape(-1, 1)
    y_train = tf.keras.utils.to_categorical(labels_raw, num_classes=2)

    # Compile the model
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

    # Training loop
    model.fit(x_train, y_train, epochs=100, verbose=1)

    return model

# Predict Sentiment
def predict_sentiment(text, model, pos_words, neg_words):
    sentiment_score = preprocess_and_score(text, pos_words, neg_words)
    input_vector = np.array([[sentiment_score]])

    # Make predictions
    pred = model.predict(input_vector)[0]
    
    # Convert the prediction to a score between -1 and 1
    score = (pred[1] - pred[0]) / 2
    return np.clip(score, -1, 1)

# Run and Test
def main():
    pos_words = load_sentiment_words("positive_words.csv")
    neg_words = load_sentiment_words("negative_words.csv")

    model = train_model(pos_words, neg_words, "training_data.csv")
    text = "I hate awful this product, it's sad!"
    sentiment = predict_sentiment(text, model, pos_words, neg_words)
    print("Sentiment: ", sentiment)

# Execute the main function
if __name__ == "__main__":
    main()
