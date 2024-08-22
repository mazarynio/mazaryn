import numpy as np
import pandas as pd
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.preprocessing import LabelBinarizer
from sklearn.model_selection import train_test_split
import tensorflow as tf
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense

# Load dataset from CSV file
def load_dataset(file_path):
    df = pd.read_csv(file_path)
    texts = df['text']
    categories = df['category']
    return texts, categories

# Define the Model
def create_model(input_size, num_categories):
    model = Sequential([
        Dense(128, activation='relu', input_shape=(input_size,)),
        Dense(64, activation='relu'),
        Dense(num_categories, activation='softmax')  # Softmax for multi-class classification
    ])
    model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])
    return model

# Train the Model
def train_model(X_train, y_train, input_size, num_categories):
    model = create_model(input_size, num_categories)
    model.fit(X_train, y_train, epochs=10, verbose=1)
    return model

# Predict Categories
def predict_category(text, model, vectorizer, lb):
    tfidf_vector = vectorizer.transform([text])
    pred = model.predict(tfidf_vector)
    pred_category = lb.classes_[np.argmax(pred)]
    return pred_category

def main():
    # Load dataset
    texts, categories = load_dataset("content_category.csv")

    # Create a TF-IDF vectorizer and transform the texts
    vectorizer = TfidfVectorizer()
    X = vectorizer.fit_transform(texts)
    
    # Convert categories to one-hot encoding
    lb = LabelBinarizer()
    y = lb.fit_transform(categories)

    # Split data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=1234)

    # Train the model
    num_categories = len(lb.classes_)
    model = train_model(X_train, y_train, X.shape[1], num_categories)

    # Example input
    text = "What are the best tips for studying effectively?"
    category = predict_category(text, model, vectorizer, lb)
    print("Predicted Category:", category)

if __name__ == "__main__":
    main()
