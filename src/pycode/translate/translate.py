from fastapi import FastAPI, HTTPException
from deep_translator import GoogleTranslator
from pydantic import BaseModel

app = FastAPI()

# Define request model
class TranslationRequest(BaseModel):
    text: str
    src_lang: str = "en"
    dest_lang: str = "fa"

@app.post("/translate")
def translate_text(request: TranslationRequest):
    try:
        translated = GoogleTranslator(source=request.src_lang, target=request.dest_lang).translate(request.text)
        return {
            "translated_text": translated,
            "source_lang": request.src_lang,
            "dest_lang": request.dest_lang,
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))


if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)