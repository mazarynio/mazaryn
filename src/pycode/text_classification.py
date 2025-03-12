import pandas as pd
import tensorflow as tf
from transformers import DebertaV2Tokenizer, TFDebertaV2ForSequenceClassification
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MultiLabelBinarizer
import numpy as np
from sklearn.metrics import f1_score, precision_score, recall_score
import os
import pickle  

def load_dataset(file_path="keywords.parquet"):
    df = pd.read_parquet(file_path)
    return df

def preprocess_data(df):
    category_keywords = df.groupby('category')['keyword'].apply(list).to_dict()
    
    texts = []
    labels = []
    
    for category, keywords in category_keywords.items():
        for keyword in keywords:
            texts.append(keyword)
            labels.append([category])
    
    mlb = MultiLabelBinarizer()
    labels = mlb.fit_transform(labels)
    
    return texts, labels, mlb

def tokenize_data(texts, tokenizer, max_length=128):
    return tokenizer(texts, padding=True, truncation=True, max_length=max_length, return_tensors="tf")

def prepare_dataset(tokenized_data, labels, batch_size=8):  
    dataset = tf.data.Dataset.from_tensor_slices((
        {
            "input_ids": tokenized_data["input_ids"],
            "attention_mask": tokenized_data["attention_mask"]
        },
        labels
    ))
    dataset = dataset.batch(batch_size)
    return dataset

def split_dataset(texts, labels, test_size=0.2):
    train_texts, test_texts, train_labels, test_labels = train_test_split(texts, labels, test_size=test_size, random_state=42)
    return train_texts, test_texts, train_labels, test_labels

def fine_tune_model(train_dataset, test_dataset, num_labels, epochs=3, batch_size=8):
    model = TFDebertaV2ForSequenceClassification.from_pretrained('microsoft/deberta-base', num_labels=num_labels)
    
    optimizer = tf.keras.optimizers.Adam(learning_rate=5e-5)
    loss = tf.keras.losses.BinaryCrossentropy(from_logits=True)
    model.compile(optimizer=optimizer, loss=loss, metrics=['accuracy'])
    
    history = model.fit(train_dataset, validation_data=test_dataset, epochs=epochs)
    
    return model, history

def evaluate_model(model, test_dataset, mlb):
    y_true = []
    y_pred = []
    
    for batch in test_dataset:
        inputs, labels = batch
        predictions = model(inputs)
        logits = predictions.logits
        probabilities = tf.sigmoid(logits).numpy()
        predicted_labels = (probabilities > 0.3).astype(int) 
        
        y_true.extend(labels.numpy())
        y_pred.extend(predicted_labels)
    
    y_true = np.array(y_true)
    y_pred = np.array(y_pred)
    
    f1 = f1_score(y_true, y_pred, average="micro")
    precision = precision_score(y_true, y_pred, average="micro")
    recall = recall_score(y_true, y_pred, average="micro")
    
    print(f"F1 Score: {f1}")
    print(f"Precision: {precision}")
    print(f"Recall: {recall}")

def classify_text(text, model, tokenizer, mlb, threshold=0.3, max_length=128):
    tokenized_text = tokenizer(text, padding=True, truncation=True, max_length=max_length, return_tensors="tf")
    
    predictions = model(tokenized_text)
    logits = predictions.logits
    
    probabilities = tf.sigmoid(logits).numpy()
    
    predicted_labels = (probabilities > threshold).astype(int)
    
    predicted_categories = mlb.inverse_transform(predicted_labels)
    
    return predicted_categories, probabilities

def main():
    df = load_dataset()
    
    texts, labels, mlb = preprocess_data(df)
    
    with open("mlb.pkl", "wb") as f:
        pickle.dump(mlb, f)
    print("Saved MultiLabelBinarizer to 'mlb.pkl'")
    
    train_texts, test_texts, train_labels, test_labels = split_dataset(texts, labels)
    
    try:
        tokenizer = DebertaV2Tokenizer.from_pretrained('microsoft/deberta-base')
    except Exception as e:
        print(f"Error loading DeBERTa-v2 tokenizer: {e}")
        print("Falling back to DeBERTa-v1 tokenizer...")
        from transformers import DebertaTokenizer
        tokenizer = DebertaTokenizer.from_pretrained('microsoft/deberta-base')
    
    train_tokenized = tokenize_data(train_texts, tokenizer)
    test_tokenized = tokenize_data(test_texts, tokenizer)
    
    train_dataset = prepare_dataset(train_tokenized, train_labels)
    test_dataset = prepare_dataset(test_tokenized, test_labels)
    
    num_labels = len(mlb.classes_)
    model, history = fine_tune_model(train_dataset, test_dataset, num_labels)
    
    evaluate_model(model, test_dataset, mlb)
    
    model.save_pretrained("text_classification_model")
    tokenizer.save_pretrained("text_classification_model")
    print("Saved model and tokenizer to 'text_classification_model'")
    
    example_texts = [
        "I love to play football",
        "I love to play basketball and trade EURUSD on forex market",
        "I love to do JavaScript programming"
    ]
    
    for text in example_texts:
        predicted_categories, probabilities = classify_text(text, model, tokenizer, mlb, threshold=0.3)
        print(f"Text: {text}")
        print(f"Predicted Categories: {predicted_categories}")
        print(f"Probabilities: {probabilities}")
        print()

if __name__ == "__main__":
    main()