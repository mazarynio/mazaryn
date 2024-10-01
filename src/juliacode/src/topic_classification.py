import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.preprocessing import MultiLabelBinarizer
from sklearn.model_selection import train_test_split
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense

# Load dataset from CSV file
def load_dataset(file_path):
    df = pd.read_csv(file_path)
    texts = df['text']
    labels = df['label'].apply(lambda x: [label.strip() for label in x.split(",")])
    return texts, labels

# Define the Model
def create_model(input_size, num_topics):
    model = Sequential([
        Dense(128, activation='relu', input_shape=(input_size,)),
        Dense(64, activation='relu'),
        Dense(num_topics, activation='sigmoid')
    ])
    model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
    return model

# Train the Model
def train_model(X_train, y_train, input_size, num_topics):
    model = create_model(input_size, num_topics)
    model.fit(X_train, y_train, epochs=50, verbose=1)
    return model

# Predict Topics
def predict_topics(text, model, vectorizer, mlb):
    tfidf_vector = vectorizer.transform([text])
    pred = model.predict(tfidf_vector)
    
    # Print raw prediction probabilities for debugging
    print("Raw prediction probabilities: ", pred)
    
    # Adjust threshold if needed
    threshold = 0.1  # Lower threshold for testing
    pred_topics = mlb.classes_[np.where(pred > threshold)[1]]
    
    return pred_topics

def main():
    # Load dataset
    texts, labels = load_dataset("topic_classification.csv")

    # Create a TF-IDF vectorizer and transform the texts
    vectorizer = TfidfVectorizer()
    X = vectorizer.fit_transform(texts)
    
    # Convert labels to binary format
    mlb = MultiLabelBinarizer()
    y = mlb.fit_transform(labels)

    # Split data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=1234)

    # Train the model
    num_topics = len(mlb.classes_)
    model = train_model(X_train, y_train, X.shape[1], num_topics)

    # Example input
    text = "Hello guys this is my post on mazaryn social network after reading some books and playing Guitar"
    topics = predict_topics(text, model, vectorizer, mlb)
    print("Predicted Topics:", topics)

if __name__ == "__main__":
    main()
