import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Embedding, LSTM, Dense, Dropout
from tensorflow.keras.callbacks import ModelCheckpoint
import pickle
import os

def extract_train_csv():
    print("Extracting train.csv from train.csv.zip...")
    zip_path = 'train.csv.zip'  
    extract_dir = os.path.dirname(__file__)  

    with zipfile.ZipFile(zip_path, 'r') as zip_ref:
        zip_ref.extract('train.csv', extract_dir)
    
    print("Extraction complete! train.csv is ready.")

def load_dataset():
    print("Loading dataset...")
    train_data = pd.read_csv('train.csv')  
    print("Dataset loaded successfully!")
    return train_data

def preprocess_data(train_data):
    print("Preprocessing data...")
    
    tokenizer = Tokenizer(num_words=10000)
    tokenizer.fit_on_texts(train_data['comment_text'])
    
    sequences = tokenizer.texts_to_sequences(train_data['comment_text'])
    
    data = pad_sequences(sequences, maxlen=200)
    
    labels = train_data[['toxic', 'severe_toxic', 'obscene', 'threat', 'insult', 'identity_hate']].values
    
    X_train, X_val, y_train, y_val = train_test_split(data, labels, test_size=0.2, random_state=42)
    
    print("Data preprocessing complete!")
    return X_train, X_val, y_train, y_val, tokenizer

def build_model():
    print("Building the model...")
    model = Sequential([
        Embedding(input_dim=10000, output_dim=128, input_length=200),  
        LSTM(128, dropout=0.2, recurrent_dropout=0.2),                
        Dense(64, activation='relu'),                                
        Dropout(0.5),                                                 
        Dense(6, activation='sigmoid')                              
    ])
    
    model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])
    
    print("Model built successfully!")
    return model

def train_model(model, X_train, y_train, X_val, y_val):
    print("Training the model...")
    
    checkpoint = ModelCheckpoint(
        os.path.join(os.path.dirname(__file__), 'best_toxicity_model.h5'), 
        monitor='val_loss',
        save_best_only=True,
        mode='min',
        verbose=1
    )
    
    history = model.fit(
        X_train, y_train,
        epochs=5,
        batch_size=32,
        validation_data=(X_val, y_val),
        callbacks=[checkpoint]
    )
    
    print("Model training complete!")
    return history

def evaluate_model(model, X_val, y_val):
    print("Evaluating the model...")
    loss, accuracy = model.evaluate(X_val, y_val)
    print(f"Validation Loss: {loss}")
    print(f"Validation Accuracy: {accuracy}")

def save_tokenizer(tokenizer):
    print("Saving the tokenizer...")
    tokenizer_path = os.path.join(os.path.dirname(__file__), 'tokenizer.pkl')  
    with open(tokenizer_path, 'wb') as f:
        pickle.dump(tokenizer, f)
    print("Tokenizer saved successfully!")

def main():
    train_data = load_dataset()
    
    X_train, X_val, y_train, y_val, tokenizer = preprocess_data(train_data)
    
    model = build_model()
    
    train_model(model, X_train, y_train, X_val, y_val)
    
    evaluate_model(model, X_val, y_val)
    
    save_tokenizer(tokenizer)

if __name__ == "__main__":
    main()