from fastapi import FastAPI, HTTPException
from pydantic import BaseModel
import os
import warnings
import time
from transformers import AutoModelForSeq2SeqLM, AutoTokenizer
import torch

warnings.filterwarnings("ignore")

app = FastAPI(title="Simple Translation API", version="2.0.0")

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

class TranslationRequest(BaseModel):
    text: str
    src_lang: str = "en"
    dest_lang: str = "es"

class TranslationResponse(BaseModel):
    translated_text: str
    processing_time: float
    model_used: str

model_cache = {}
tokenizer_cache = {}

MARIAN_KNOWN = {
    ("en", "es"): "Helsinki-NLP/opus-mt-en-es",
    ("en", "fr"): "Helsinki-NLP/opus-mt-en-fr",
    ("en", "de"): "Helsinki-NLP/opus-mt-en-de",
    ("en", "it"): "Helsinki-NLP/opus-mt-en-it",
    ("es", "en"): "Helsinki-NLP/opus-mt-es-en",
    ("fr", "en"): "Helsinki-NLP/opus-mt-fr-en",
    ("de", "en"): "Helsinki-NLP/opus-mt-de-en",
    ("it", "en"): "Helsinki-NLP/opus-mt-it-en",
}

M2M_MODEL_NAME = os.getenv("MULTILINGUAL_MODEL", "facebook/m2m100_418M")

ALIASES = {
    "zh-cn": "zh", "zh-sg": "zh", "zh-hans": "zh",
    "zh-tw": "zh", "zh-hk": "zh", "zh-hant": "zh",
    "pt-br": "pt", "pt-pt": "pt",
    "he": "iw",  
}

def normalize_lang(code: str) -> str:
    c = code.strip().lower().replace("_", "-")
    if c in ALIASES:
        return ALIASES[c]
    if "-" in c:
        c = c.split("-")[0]
    return c

def load_model(model_name: str):
    if model_name in model_cache:
        return model_cache[model_name], tokenizer_cache[model_name]
    tok = AutoTokenizer.from_pretrained(model_name)
    mdl = AutoModelForSeq2SeqLM.from_pretrained(model_name)
    mdl.to(device)
    model_cache[model_name] = mdl
    tokenizer_cache[model_name] = tok
    return mdl, tok

def try_marian(src: str, dest: str):
    key = (src, dest)
    if key in MARIAN_KNOWN:
        return MARIAN_KNOWN[key]
    guess = f"Helsinki-NLP/opus-mt-{src}-{dest}"
    try:
        _mdl, _tok = load_model(guess)
        return guess
    except Exception:
        return None

def m2m_supported_languages():
    try:
        _mdl, tok = load_model(M2M_MODEL_NAME)
        if hasattr(tok, "lang_code_to_id"):
            return sorted(list(tok.lang_code_to_id.keys()))
        if hasattr(tok, "languages"):
            return sorted(list(tok.languages))
    except Exception:
        pass
    return []

def translate_with_marian(text: str, src: str, dest: str, model_name: str) -> str:
    mdl, tok = load_model(model_name)
    enc = tok(text, return_tensors="pt", padding=True, truncation=True, max_length=512).to(device)
    with torch.no_grad():
        out = mdl.generate(**enc, max_length=512, num_beams=4, early_stopping=True, do_sample=False)
    return tok.decode(out[0], skip_special_tokens=True)

def translate_with_m2m(text: str, src: str, dest: str) -> str:
    mdl, tok = load_model(M2M_MODEL_NAME)

    src_tag = src
    dest_tag = dest

    if hasattr(tok, "lang_code_to_id"):
        if src_tag not in tok.lang_code_to_id:
            raise HTTPException(status_code=400, detail=f"Source language '{src_tag}' not supported by {M2M_MODEL_NAME}")
        if dest_tag not in tok.lang_code_to_id:
            raise HTTPException(status_code=400, detail=f"Target language '{dest_tag}' not supported by {M2M_MODEL_NAME}")
        tok.src_lang = src_tag
        forced_bos = tok.lang_code_to_id[dest_tag]
    else:
        try:
            tok.src_lang = src_tag
            forced_bos = tok.get_lang_id(dest_tag)
        except Exception:
            raise HTTPException(status_code=400, detail=f"Languages '{src_tag}->{dest_tag}' not supported by {M2M_MODEL_NAME}")

    enc = tok(text, return_tensors="pt", padding=True, truncation=True, max_length=512).to(device)

    with torch.no_grad():
        out = mdl.generate(
            **enc,
            max_length=512,
            num_beams=4,
            early_stopping=True,
            do_sample=False,
            forced_bos_token_id=forced_bos,
        )
    return tok.decode(out[0], skip_special_tokens=True)

def pick_and_translate(text: str, src_lang: str, dest_lang: str):
    src = normalize_lang(src_lang)
    dest = normalize_lang(dest_lang)
    if src == dest:
        return text, "identity (no-op)"

    marian_model = try_marian(src, dest)
    if marian_model:
        return translate_with_marian(text, src, dest, marian_model), marian_model

    translated = translate_with_m2m(text, src, dest)
    return translated, M2M_MODEL_NAME

@app.post("/translate", response_model=TranslationResponse)
async def translate_text(request: TranslationRequest):
    start_time = time.time()
    try:
        translated_text, model_used = pick_and_translate(request.text, request.src_lang, request.dest_lang)
        return TranslationResponse(
            translated_text=translated_text,
            processing_time=time.time() - start_time,
            model_used=model_used
        )
    except HTTPException:
        raise
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Translation failed: {str(e)}")

@app.get("/health")
async def health_check():
    return {
        "status": "healthy",
        "models_loaded": list(model_cache.keys()),
        "device": str(device),
        "torch_version": torch.__version__
    }

@app.get("/supported-languages")
async def get_supported_languages():
    m2m_langs = m2m_supported_languages()
    pairs = sorted(list(MARIAN_KNOWN.keys()))
    return {
        "marian_fast_pairs": [{"src": s, "dest": d, "name": f"{s}→{d}"} for (s, d) in pairs],
        "multilingual_model": M2M_MODEL_NAME,
        "multilingual_languages": m2m_langs,
        "note": "If a Marian pair isn’t available, the multilingual model is used."
    }

@app.delete("/cache")
async def clear_cache():
    model_cache.clear()
    tokenizer_cache.clear()
    return {"message": "Cache cleared"}

@app.get("/test")
async def quick_test():
    try:
        result = await translate_text(TranslationRequest(text="Hello world", src_lang="en", dest_lang="es"))
        return {"test_status": "success", "translation": result.translated_text, "processing_time": result.processing_time}
    except Exception as e:
        return {"test_status": "failed", "error": str(e)}

if __name__ == "__main__":
    import uvicorn
    print("Starting Translation API v2...")
    uvicorn.run(app, host="0.0.0.0", port=8089)
