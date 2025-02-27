import pandas as pd
import re
from sklearn.preprocessing import LabelEncoder
from sklearn.model_selection import train_test_split
from tensorflow.keras.preprocessing.text import Tokenizer
from tensorflow.keras.preprocessing.sequence import pad_sequences
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Embedding, LSTM, Dense, Dropout
import pickle

parquet_file = "keywords.parquet"
df = pd.read_parquet(parquet_file)

print("Dataset Head:")
print(df.head())
print("\nDataset Info:")
print(df.info())

def clean_text(text):
    text = re.sub(r"[^a-zA-Z\s]", "", text)
    text = text.lower()
    return text

df["keyword"] = df["keyword"].apply(clean_text)

label_encoder = LabelEncoder()
df["label"] = label_encoder.fit_transform(df["category"])

print("\nEncoded Labels:")
print(df[["category", "label"]].drop_duplicates())

train_df, val_df = train_test_split(df, test_size=0.2, random_state=42)

print(f"\nTraining samples: {len(train_df)}")
print(f"Validation samples: {len(val_df)}")

tokenizer = Tokenizer(num_words=10000)
tokenizer.fit_on_texts(train_df["keyword"])

train_sequences = tokenizer.texts_to_sequences(train_df["keyword"])
val_sequences = tokenizer.texts_to_sequences(val_df["keyword"])

max_length = 20  
train_padded = pad_sequences(train_sequences, maxlen=max_length, padding="post", truncating="post")
val_padded = pad_sequences(val_sequences, maxlen=max_length, padding="post", truncating="post")

model = Sequential([
    Embedding(input_dim=10000, output_dim=128, input_length=max_length),
    LSTM(64, return_sequences=False),
    Dropout(0.5),
    Dense(64, activation="relu"),
    Dense(len(label_encoder.classes_), activation="softmax")  
])

model.compile(optimizer="adam", loss="sparse_categorical_crossentropy", metrics=["accuracy"])

print("\nModel Summary:")
model.summary()

print("\nTraining the model...")
history = model.fit(
    train_padded,
    train_df["label"],
    validation_data=(val_padded, val_df["label"]),
    epochs=10,
    batch_size=32
)

val_loss, val_accuracy = model.evaluate(val_padded, val_df["label"])
print(f"\nValidation Loss: {val_loss}")
print(f"Validation Accuracy: {val_accuracy}")

model.save("text_classification_model.h5")
print("\nModel saved as 'text_classification_model.h5'.")

with open("tokenizer.pkl", "wb") as f:
    pickle.dump(tokenizer, f)

with open("label_encoder.pkl", "wb") as f:
    pickle.dump(label_encoder, f)

print("Tokenizer and label encoder saved as 'tokenizer.pkl' and 'label_encoder.pkl'.")