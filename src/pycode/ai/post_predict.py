from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import tensorflow as tf
from transformers import DebertaTokenizer, TFDebertaV2ForSequenceClassification
from sklearn.preprocessing import MultiLabelBinarizer
import numpy as np
import pickle
import os
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

current_dir = os.path.dirname(os.path.abspath(__file__))
model_path = os.path.join(current_dir, "..", "text_classification_model")
mlb_path = os.path.join(current_dir, "..", "mlb.pkl")

if not os.path.exists(model_path):
    raise FileNotFoundError(f"Model directory '{model_path}' not found. Please ensure it exists.")

model = TFDebertaV2ForSequenceClassification.from_pretrained(model_path)

try:
    tokenizer = DebertaTokenizer.from_pretrained(model_path)
except Exception as e:
    logger.warning(f"Failed to load DebertaTokenizer: {e}")
    from transformers import DebertaV2Tokenizer
    tokenizer = DebertaV2Tokenizer.from_pretrained(model_path)

if not os.path.exists(mlb_path):
    raise FileNotFoundError(f"MultiLabelBinarizer file '{mlb_path}' not found. Please ensure it exists.")

with open(mlb_path, "rb") as f:
    mlb = pickle.load(f)

router = APIRouter()

class SentenceRequest(BaseModel):
    sentence: str

@router.post("/text_predict")
def predict(request: SentenceRequest):
    try:
        tokenized_text = tokenizer(request.sentence, padding=True, truncation=True, max_length=128, return_tensors="tf")
        
        predictions = model(tokenized_text)
        logits = predictions.logits
        
        probabilities = tf.sigmoid(logits).numpy()
        
        threshold = 0.3
        predicted_labels = (probabilities > threshold).astype(int)
        
        predicted_categories = mlb.inverse_transform(predicted_labels)
        
        response = {
            "sentence": request.sentence,
            "categories": predicted_categories[0],  
            "probabilities": probabilities.tolist()[0]  
        }
        
        return response
    except Exception as e:
        logger.error(f"Error during prediction: {e}")
        raise HTTPException(status_code=500, detail=str(e))