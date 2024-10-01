import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
from sklearn.feature_extraction.text import TfidfVectorizer
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout, Embedding, LSTM
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
from tensorflow.keras.utils import to_categorical

# Load dataset
def load_dataset(file_path):
    df = pd.read_csv(file_path)
    texts = df['text']
    languages = df['language']
    return texts, languages

# Preprocess text and encode labels
def preprocess_data(texts, languages):
    # Tokenize and pad sequences
    tokenizer = Tokenizer()
    tokenizer.fit_on_texts(texts)
    sequences = tokenizer.texts_to_sequences(texts)
    X = pad_sequences(sequences)
    
    # Encode labels
    label_encoder = LabelEncoder()
    y = label_encoder.fit_transform(languages)
    y = to_categorical(y)  # One-hot encoding
    
    return X, y, tokenizer, label_encoder

# Define the model
def create_model(input_length, num_classes):
    model = Sequential([
        Embedding(input_dim=10000, output_dim=128, input_length=input_length),
        LSTM(128, return_sequences=True),
        Dropout(0.5),
        LSTM(64),
        Dense(num_classes, activation='softmax')
    ])
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    return model

# Predict language
def predict_language(text, model, tokenizer, label_encoder):
    sequence = tokenizer.texts_to_sequences([text])
    padded_sequence = pad_sequences(sequence, maxlen=model.input_shape[1])
    pred = model.predict(padded_sequence)
    language = label_encoder.inverse_transform(np.argmax(pred, axis=1))
    return language[0]

# Main function
def main():
    # Load and preprocess data
    texts, languages = load_dataset('language_detection.csv')
    X, y, tokenizer, label_encoder = preprocess_data(texts, languages)
    
    # Split data into training and test sets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
    
    # Create and train the model
    model = create_model(X.shape[1], y.shape[1])
    model.fit(X_train, y_train, epochs=10, batch_size=32, validation_split=0.1)
    
    # Evaluate the model
    loss, accuracy = model.evaluate(X_test, y_test)
    print(f"Test accuracy: {accuracy:.2f}")
    
    # Example usage
    text = "How are you?"
    predicted_language = predict_language(text, model, tokenizer, label_encoder)
    print(f"Predicted language: {predicted_language}")

if __name__ == "__main__":
    main()
