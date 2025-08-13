from fastapi import FastAPI, HTTPException, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel, Field
from typing import List, Optional, Dict, Any
import os
import warnings
import asyncio
import time
import hashlib
from transformers import (
    AutoModelForSeq2SeqLM, 
    AutoTokenizer, 
    pipeline,
    M2M100ForConditionalGeneration,
    M2M100Tokenizer
)
import torch
import logging
from functools import lru_cache
import json

warnings.filterwarnings("ignore")
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
logging.getLogger("transformers").setLevel(logging.ERROR)

class TranslationRequest(BaseModel):
    text: str = Field(..., description="Text to translate", max_length=5000)
    src_lang: str = Field("auto", description="Source language code (auto for detection)")
    dest_lang: str = Field("en", description="Target language code")
    model_type: str = Field("nllb", description="Model type: nllb, m2m100, or marian")
    quality: str = Field("high", description="Translation quality: high, balanced, or fast")

class BatchTranslationRequest(BaseModel):
    texts: List[str] = Field(..., description="List of texts to translate")
    src_lang: str = Field("auto", description="Source language code")
    dest_lang: str = Field("en", description="Target language code")
    model_type: str = Field("nllb", description="Model type")
    quality: str = Field("high", description="Translation quality")

class TranslationResponse(BaseModel):
    translated_text: str
    detected_language: Optional[str] = None
    confidence_score: Optional[float] = None
    model_used: str
    processing_time: float

class BatchTranslationResponse(BaseModel):
    translations: List[TranslationResponse]
    total_processing_time: float

app = FastAPI(
    title="Advanced Translation API",
    description="State-of-the-art multilingual translation service",
    version="2.0.0"
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

MODEL_CONFIGS = {
    "nllb": {
        "high": "facebook/nllb-200-3.3B",
        "balanced": "facebook/nllb-200-1.3B", 
        "fast": "facebook/nllb-200-distilled-600M"
    },
    "m2m100": {
        "high": "facebook/m2m100_1.2B",
        "balanced": "facebook/m2m100_418M",
        "fast": "facebook/m2m100_418M"
    },
    "marian": {
        "high": "Helsinki-NLP/opus-mt-{src}-{dest}",
        "balanced": "Helsinki-NLP/opus-mt-{src}-{dest}",
        "fast": "Helsinki-NLP/opus-mt-{src}-{dest}"
    }
}

NLLB_LANG_CODES = {
    "en": "eng_Latn", "es": "spa_Latn", "fr": "fra_Latn", "de": "deu_Latn",
    "it": "ita_Latn", "pt": "por_Latn", "ru": "rus_Cyrl", "zh": "zho_Hans",
    "ja": "jpn_Jpan", "ko": "kor_Hang", "ar": "arb_Arab", "hi": "hin_Deva",
    "tr": "tur_Latn", "pl": "pol_Latn", "nl": "nld_Latn", "sv": "swe_Latn",
    "da": "dan_Latn", "no": "nor_Latn", "fi": "fin_Latn", "cs": "ces_Latn",
    "sk": "slk_Latn", "hu": "hun_Latn", "ro": "ron_Latn", "bg": "bul_Cyrl",
    "hr": "hrv_Latn", "sr": "srp_Cyrl", "sl": "slv_Latn", "et": "est_Latn",
    "lv": "lav_Latn", "lt": "lit_Latn", "uk": "ukr_Cyrl", "be": "bel_Cyrl",
    "mk": "mkd_Cyrl", "sq": "sqi_Latn", "eu": "eus_Latn", "ca": "cat_Latn",
    "gl": "glg_Latn", "cy": "cym_Latn", "ga": "gle_Latn", "mt": "mlt_Latn",
    "is": "isl_Latn", "fo": "fao_Latn", "he": "heb_Hebr", "fa": "pes_Arab",
    "ur": "urd_Arab", "bn": "ben_Beng", "ta": "tam_Taml", "te": "tel_Telu",
    "ml": "mal_Mlym", "kn": "kan_Knda", "gu": "guj_Gujr", "pa": "pan_Guru",
    "th": "tha_Thai", "vi": "vie_Latn", "id": "ind_Latn", "ms": "zsm_Latn",
    "tl": "tgl_Latn", "sw": "swh_Latn", "am": "amh_Ethi", "yo": "yor_Latn",
    "ig": "ibo_Latn", "ha": "hau_Latn", "zu": "zul_Latn", "xh": "xho_Latn",
    "af": "afr_Latn", "ne": "npi_Deva", "si": "sin_Sinh", "my": "mya_Mymr",
    "km": "khm_Khmr", "lo": "lao_Laoo", "ka": "kat_Geor", "hy": "hye_Armn",
    "az": "azj_Latn", "kk": "kaz_Cyrl", "ky": "kir_Cyrl", "uz": "uzn_Latn",
    "tg": "tgk_Cyrl", "mn": "khk_Cyrl", "bo": "bod_Tibt", "dz": "dzo_Tibt"
}

M2M100_LANG_CODES = {
    "en": "en", "es": "es", "fr": "fr", "de": "de", "it": "it", "pt": "pt",
    "ru": "ru", "zh": "zh", "ja": "ja", "ko": "ko", "ar": "ar", "hi": "hi",
    "tr": "tr", "pl": "pl", "nl": "nl", "cs": "cs", "ro": "ro", "uk": "uk",
    "vi": "vi", "th": "th", "id": "id", "ms": "ms", "he": "he", "fa": "fa",
    "bn": "bn", "ta": "ta", "te": "te", "ml": "ml", "kn": "kn", "gu": "gu",
    "pa": "pa", "ur": "ur", "ne": "ne", "si": "si", "my": "my", "km": "km",
    "lo": "lo", "ka": "ka", "hy": "hy", "az": "az", "kk": "kk", "ky": "ky",
    "uz": "uz", "tg": "tg", "mn": "mn", "af": "af", "sw": "sw", "am": "am",
    "yo": "yo", "ig": "ig", "ha": "ha", "zu": "zu", "xh": "xh"
}

model_cache: Dict[str, Any] = {}
device = torch.device("cuda" if torch.cuda.is_available() else "cpu")

@lru_cache(maxsize=1)
def get_language_detector():
    return pipeline("text-classification", model="facebook/fasttext-language-identification")

class TranslationService:
    def __init__(self):
        self.cache = {}
        
    def _get_cache_key(self, text: str, src_lang: str, dest_lang: str, model_type: str, quality: str) -> str:
        """Generate cache key for translation."""
        content = f"{text}|{src_lang}|{dest_lang}|{model_type}|{quality}"
        return hashlib.md5(content.encode()).hexdigest()
    
    def _detect_language(self, text: str) -> str:
        """Detect the language of input text."""
        try:
            detector = get_language_detector()
            result = detector(text[:500])  
            detected_lang = result[0]['label'].split('_')[0]  
            return detected_lang if detected_lang in NLLB_LANG_CODES else "en"
        except:
            return "en"  
    
    def _get_model_key(self, model_type: str, quality: str, src_lang: str = None, dest_lang: str = None) -> str:
        """Generate model key for caching."""
        if model_type == "marian" and src_lang and dest_lang:
            return f"marian-{src_lang}-{dest_lang}"
        return f"{model_type}-{quality}"
    
    def _load_model(self, model_type: str, quality: str, src_lang: str = None, dest_lang: str = None):
        """Load and cache translation model."""
        model_key = self._get_model_key(model_type, quality, src_lang, dest_lang)
        
        if model_key in model_cache:
            return model_cache[model_key]
        
        try:
            if model_type == "nllb":
                model_name = MODEL_CONFIGS["nllb"][quality]
                model = AutoModelForSeq2SeqLM.from_pretrained(
                    model_name,
                    torch_dtype=torch.float16 if torch.cuda.is_available() else torch.float32,
                    device_map="auto" if torch.cuda.is_available() else None
                )
                tokenizer = AutoTokenizer.from_pretrained(model_name)
                
            elif model_type == "m2m100":
                model_name = MODEL_CONFIGS["m2m100"][quality]
                model = M2M100ForConditionalGeneration.from_pretrained(
                    model_name,
                    torch_dtype=torch.float16 if torch.cuda.is_available() else torch.float32,
                    device_map="auto" if torch.cuda.is_available() else None
                )
                tokenizer = M2M100Tokenizer.from_pretrained(model_name)
                
            elif model_type == "marian" and src_lang and dest_lang:
                model_name = f"Helsinki-NLP/opus-mt-{src_lang}-{dest_lang}"
                model = AutoModelForSeq2SeqLM.from_pretrained(model_name)
                tokenizer = AutoTokenizer.from_pretrained(model_name)
                if not torch.cuda.is_available():
                    model = model.to(device)
            else:
                raise ValueError(f"Invalid model configuration: {model_type}")
            
            model_cache[model_key] = {"model": model, "tokenizer": tokenizer, "name": model_name}
            return model_cache[model_key]
            
        except Exception as e:
            raise HTTPException(
                status_code=400, 
                detail=f"Failed to load model {model_type}-{quality}: {str(e)}"
            )
    
    def _translate_with_nllb(self, text: str, src_lang: str, dest_lang: str, model_info: dict) -> tuple:
        """Translate using NLLB model."""
        model, tokenizer = model_info["model"], model_info["tokenizer"]
        
        src_code = NLLB_LANG_CODES.get(src_lang, "eng_Latn")
        dest_code = NLLB_LANG_CODES.get(dest_lang, "eng_Latn")
        
        tokenizer.src_lang = src_code
        inputs = tokenizer(text, return_tensors="pt", padding=True, truncation=True, max_length=512)
        
        if torch.cuda.is_available():
            inputs = {k: v.to(model.device) for k, v in inputs.items()}
        
        with torch.no_grad():
            generated_tokens = model.generate(
                **inputs,
                forced_bos_token_id=tokenizer.lang_code_to_id[dest_code],
                max_length=512,
                num_beams=5,
                do_sample=True,
                temperature=0.7,
                top_p=0.9,
                no_repeat_ngram_size=3
            )
        
        translated_text = tokenizer.batch_decode(generated_tokens, skip_special_tokens=True)[0]
        return translated_text, 0.95  
    
    def _translate_with_m2m100(self, text: str, src_lang: str, dest_lang: str, model_info: dict) -> tuple:
        """Translate using M2M100 model."""
        model, tokenizer = model_info["model"], model_info["tokenizer"]
        
        src_code = M2M100_LANG_CODES.get(src_lang, "en")
        dest_code = M2M100_LANG_CODES.get(dest_lang, "en")
        
        tokenizer.src_lang = src_code
        inputs = tokenizer(text, return_tensors="pt", padding=True, truncation=True, max_length=512)
        
        if torch.cuda.is_available():
            inputs = {k: v.to(model.device) for k, v in inputs.items()}
        
        with torch.no_grad():
            generated_tokens = model.generate(
                **inputs,
                forced_bos_token_id=tokenizer.get_lang_id(dest_code),
                max_length=512,
                num_beams=4,
                do_sample=True,
                temperature=0.6,
                no_repeat_ngram_size=2
            )
        
        translated_text = tokenizer.batch_decode(generated_tokens, skip_special_tokens=True)[0]
        return translated_text, 0.90  
    
    def _translate_with_marian(self, text: str, src_lang: str, dest_lang: str, model_info: dict) -> tuple:
        """Translate using MarianMT model."""
        model, tokenizer = model_info["model"], model_info["tokenizer"]
        
        inputs = tokenizer(text, return_tensors="pt", padding=True, truncation=True, max_length=512)
        
        if not torch.cuda.is_available():
            inputs = {k: v.to(device) for k, v in inputs.items()}
        
        with torch.no_grad():
            generated_tokens = model.generate(
                **inputs,
                max_length=512,
                num_beams=4,
                early_stopping=True,
                no_repeat_ngram_size=2
            )
        
        translated_text = tokenizer.decode(generated_tokens[0], skip_special_tokens=True)
        return translated_text, 0.85  
    
    async def translate(self, request: TranslationRequest) -> TranslationResponse:
        """Main translation method."""
        start_time = time.time()
        
        cache_key = self._get_cache_key(
            request.text, request.src_lang, request.dest_lang, 
            request.model_type, request.quality
        )
        
        if cache_key in self.cache:
            cached_result = self.cache[cache_key]
            cached_result.processing_time = time.time() - start_time
            return cached_result
        
        detected_lang = None
        src_lang = request.src_lang
        if src_lang == "auto":
            src_lang = self._detect_language(request.text)
            detected_lang = src_lang
        
        if src_lang == request.dest_lang:
            result = TranslationResponse(
                translated_text=request.text,
                detected_language=detected_lang,
                confidence_score=1.0,
                model_used="none",
                processing_time=time.time() - start_time
            )
            self.cache[cache_key] = result
            return result
        
        model_info = self._load_model(request.model_type, request.quality, src_lang, request.dest_lang)
        
        try:
            if request.model_type == "nllb":
                translated_text, confidence = self._translate_with_nllb(
                    request.text, src_lang, request.dest_lang, model_info
                )
            elif request.model_type == "m2m100":
                translated_text, confidence = self._translate_with_m2m100(
                    request.text, src_lang, request.dest_lang, model_info
                )
            elif request.model_type == "marian":
                translated_text, confidence = self._translate_with_marian(
                    request.text, src_lang, request.dest_lang, model_info
                )
            else:
                raise ValueError(f"Unsupported model type: {request.model_type}")
            
            result = TranslationResponse(
                translated_text=translated_text,
                detected_language=detected_lang,
                confidence_score=confidence,
                model_used=model_info["name"],
                processing_time=time.time() - start_time
            )
            
            self.cache[cache_key] = result
            return result
            
        except Exception as e:
            raise HTTPException(status_code=500, detail=f"Translation failed: {str(e)}")

translation_service = TranslationService()

@app.post("/translate", response_model=TranslationResponse)
async def translate_text(request: TranslationRequest):
    """Translate a single text."""
    return await translation_service.translate(request)

@app.post("/translate/batch", response_model=BatchTranslationResponse)
async def translate_batch(request: BatchTranslationRequest):
    """Translate multiple texts in batch."""
    start_time = time.time()
    
    translation_requests = [
        TranslationRequest(
            text=text,
            src_lang=request.src_lang,
            dest_lang=request.dest_lang,
            model_type=request.model_type,
            quality=request.quality
        )
        for text in request.texts
    ]
    
    translations = await asyncio.gather(
        *[translation_service.translate(req) for req in translation_requests]
    )
    
    return BatchTranslationResponse(
        translations=translations,
        total_processing_time=time.time() - start_time
    )

@app.get("/languages")
async def get_supported_languages():
    """Get list of supported languages for each model."""
    return {
        "nllb": list(NLLB_LANG_CODES.keys()),
        "m2m100": list(M2M100_LANG_CODES.keys()),
        "marian": ["Depends on language pair availability"]
    }

@app.get("/models")
async def get_available_models():
    """Get information about available models."""
    return {
        "models": MODEL_CONFIGS,
        "recommendations": {
            "high_quality": "Use NLLB-3.3B for best quality",
            "balanced": "Use NLLB-1.3B or M2M100-418M for good speed/quality balance",
            "fast": "Use NLLB-600M for fastest translation",
            "specific_pairs": "Use MarianMT for specific language pairs with good quality"
        }
    }

@app.get("/health")
async def health_check():
    """Health check endpoint."""
    return {
        "status": "healthy",
        "models_loaded": len(model_cache),
        "device": str(device),
        "cuda_available": torch.cuda.is_available()
    }

@app.delete("/cache")
async def clear_cache():
    """Clear model cache (admin endpoint)."""
    global model_cache
    model_cache.clear()
    translation_service.cache.clear()
    torch.cuda.empty_cache() if torch.cuda.is_available() else None
    return {"message": "Cache cleared successfully"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        app, 
        host="0.0.0.0", 
        port=8082,
        workers=1,  
        log_level="info"
    )