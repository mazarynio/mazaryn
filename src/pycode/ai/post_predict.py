from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import torch
from transformers import DistilBertForSequenceClassification, DistilBertTokenizer
from sklearn.preprocessing import LabelEncoder
import pickle
import os
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

model = DistilBertForSequenceClassification.from_pretrained("./fine_tuned_distilbert")
tokenizer = DistilBertTokenizer.from_pretrained("./fine_tuned_distilbert")

# Load the label encoder
label_encoder_path = "label_encoder.pkl"

if not os.path.exists(label_encoder_path):
    raise FileNotFoundError(f"Label encoder file '{label_encoder_path}' not found. Please ensure it exists.")

with open(label_encoder_path, "rb") as f:
    label_encoder = pickle.load(f)

router = APIRouter()

# Define request body
class SentenceRequest(BaseModel):
    sentence: str

# Define prediction endpoint
@router.post("/text_predict")
def predict(request: SentenceRequest):
    try:
        # Tokenize the input sentence
        inputs = tokenizer(request.sentence, return_tensors="pt", padding=True, truncation=True, max_length=128)
        
        # Make prediction
        with torch.no_grad():
            outputs = model(**inputs)
            logits = outputs.logits
            probs = torch.softmax(logits, dim=-1)  
        
        # Get all predicted categories and their probabilities
        all_probs = probs[0].numpy()
        all_categories = label_encoder.classes_
        
        # Log confidence scores for debugging
        logger.info(f"Confidence scores for sentence: {request.sentence}")
        for cat, prob in zip(all_categories, all_probs):
            logger.info(f"{cat}: {prob:.4f}")
        
        # Filter categories based on a confidence threshold
        confidence_threshold = 0.2  
        relevant_categories = [
            {"category": cat, "probability": float(prob)}
            for cat, prob in zip(all_categories, all_probs)
            if prob >= confidence_threshold
        ]
        
        # If no categories meet the threshold, return the top category
        if not relevant_categories:
            top_index = torch.argmax(probs, dim=-1).item()
            top_category = label_encoder.classes_[top_index]
            top_prob = float(probs[0][top_index].item())
            relevant_categories = [{"category": top_category, "probability": top_prob}]
        
        # Sort categories by probability (descending order)
        relevant_categories.sort(key=lambda x: x["probability"], reverse=True)
        
        return {
            "sentence": request.sentence,
            "categories": relevant_categories
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))