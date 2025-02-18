from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import os
import warnings
from transformers import TFAutoModelForSeq2SeqLM, AutoTokenizer
import logging

# Suppress all warnings
warnings.filterwarnings("ignore")

# Suppress TensorFlow and CUDA logging
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  
os.environ["CUDA_VISIBLE_DEVICES"] = ""  

# Suppress Hugging Face logging
logging.getLogger("transformers").setLevel(logging.ERROR)

# Define request model
class TranslationRequest(BaseModel):
    text: str
    src_lang: str = "en"
    dest_lang: str = "it" 

app = FastAPI()

# Dictionary to cache loaded models and tokenizers
model_cache = {}
tokenizer_cache = {}

def get_model_and_tokenizer(src_lang, dest_lang):
    # Generate a unique key for the model and tokenizer
    cache_key = f"{src_lang}-{dest_lang}"

    # Check if the model and tokenizer are already loaded
    if cache_key in model_cache:
        return model_cache[cache_key], tokenizer_cache[cache_key]

    # Load the appropriate MarianMT model
    model_name = f"Helsinki-NLP/opus-mt-{src_lang}-{dest_lang}"
    try:
        model = TFAutoModelForSeq2SeqLM.from_pretrained(model_name)
        tokenizer = AutoTokenizer.from_pretrained(model_name)
    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Unsupported language pair: {src_lang}-{dest_lang}")

    # Cache the model and tokenizer
    model_cache[cache_key] = model
    tokenizer_cache[cache_key] = tokenizer

    return model, tokenizer

@app.post("/translate")
def translate_text(request: TranslationRequest):
    try:
        # Get the model and tokenizer for the specified language pair
        model, tokenizer = get_model_and_tokenizer(request.src_lang, request.dest_lang)

        # Translate the text
        input_ids = tokenizer.encode(request.text, return_tensors="tf")
        translated_ids = model.generate(input_ids)
        translated_text = tokenizer.decode(translated_ids[0], skip_special_tokens=True)

        # Return only the translated text
        return translated_text
    except HTTPException as e:
        raise e
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)