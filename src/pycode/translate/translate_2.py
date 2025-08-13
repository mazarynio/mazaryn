from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import os
import warnings
from transformers import TFAutoModelForSeq2SeqLM, AutoTokenizer
import logging

warnings.filterwarnings("ignore")
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"  
logging.getLogger("transformers").setLevel(logging.ERROR)

app = FastAPI()

class TranslationRequest(BaseModel):
    text: str
    src_lang: str = "en"
    dest_lang: str = "it"

model_cache = {}
tokenizer_cache = {}

def get_model_and_tokenizer(src_lang: str, dest_lang: str):
    cache_key = f"{src_lang}-{dest_lang}"
    
    if cache_key in model_cache:
        return model_cache[cache_key], tokenizer_cache[cache_key]
    
    model_name = f"Helsinki-NLP/opus-mt-{src_lang}-{dest_lang}"
    try:
        model = TFAutoModelForSeq2SeqLM.from_pretrained(model_name)
        tokenizer = AutoTokenizer.from_pretrained(model_name)
    except Exception as e:
        raise HTTPException(status_code=400, detail=f"Unsupported language pair: {src_lang}-{dest_lang} or model loading failed: {str(e)}")
    
    model_cache[cache_key] = model
    tokenizer_cache[cache_key] = tokenizer
    return model, tokenizer

@app.post("/translate")
async def translate_text(request: TranslationRequest):
    try:
        supported_languages = ["en", "it", "fr", "es", "de", "zh", "ru", "ar", "hi", "fa"] 
        if request.src_lang not in supported_languages or request.dest_lang not in supported_languages:
            raise HTTPException(status_code=400, detail=f"Unsupported language pair: {request.src_lang}-{request.dest_lang}")

        model, tokenizer = get_model_and_tokenizer(request.src_lang, request.dest_lang)
        
        inputs = tokenizer(request.text, return_tensors="tf", padding=True, truncation=True)
        translated_ids = model.generate(inputs["input_ids"], attention_mask=inputs["attention_mask"])
        translated_text = tokenizer.decode(translated_ids[0], skip_special_tokens=True)
        
        return {
            "translated_text": translated_text,
            "source_lang": request.src_lang,
            "dest_lang": request.dest_lang
        }
    except HTTPException as e:
        raise e
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Translation error: {str(e)}")

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8003)