from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from tensorflow.keras.models import load_model
from tensorflow.keras.preprocessing.sequence import pad_sequences
import pickle
import os

router = APIRouter()

MODEL_PATH = os.path.join(os.path.dirname(__file__), 'best_toxicity_model.h5')
TOKENIZER_PATH = os.path.join(os.path.dirname(__file__), 'tokenizer.pkl')

print(f"Current Working Directory: {os.getcwd()}")
print(f"Model Path: {MODEL_PATH}")
print(f"Tokenizer Path: {TOKENIZER_PATH}")

if not os.path.exists(MODEL_PATH):
    raise FileNotFoundError(f"Model file not found: {MODEL_PATH}")
if not os.path.exists(TOKENIZER_PATH):
    raise FileNotFoundError(f"Tokenizer file not found: {TOKENIZER_PATH}")

try:
    model = load_model(MODEL_PATH)
    with open(TOKENIZER_PATH, 'rb') as f:
        tokenizer = pickle.load(f)
    print("Model and tokenizer loaded successfully!")
except Exception as e:
    raise RuntimeError(f"Failed to load model or tokenizer: {e}")

class ToxicityRequest(BaseModel):
    sentence: str

class ToxicityResponse(BaseModel):
    toxic: float
    severe_toxic: float
    obscene: float
    threat: float
    insult: float
    identity_hate: float

@router.post("/toxicity", response_model=ToxicityResponse)
def predict_toxicity(request: ToxicityRequest):
    try:
        sequence = tokenizer.texts_to_sequences([request.sentence])
        padded_sequence = pad_sequences(sequence, maxlen=200)
        
        prediction = model.predict(padded_sequence)
        
        result = {
            "toxic": float(prediction[0][0]),
            "severe_toxic": float(prediction[0][1]),
            "obscene": float(prediction[0][2]),
            "threat": float(prediction[0][3]),
            "insult": float(prediction[0][4]),
            "identity_hate": float(prediction[0][5]),
        }
        
        return result
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Prediction failed: {e}")