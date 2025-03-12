from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import pandas as pd

router = APIRouter()

class ChatDataRequest(BaseModel):
    id: str
    ai_chat_id: str
    user_id: str
    recipient_id: str
    body: str
    media: str
    bot: str
    date_created: str
    date_updated: str
    filename: str

@router.post("/convert_chat_to_parquet")
def convert_chat_to_parquet(request: ChatDataRequest):
    try:
        data = {
            "id": [request.id],
            "ai_chat_id": [request.ai_chat_id],
            "user_id": [request.user_id],
            "recipient_id": [request.recipient_id],
            "body": [request.body],
            "media": [request.media],
            "bot": [request.bot],
            "date_created": [request.date_created],
            "date_updated": [request.date_updated]
        }

        df = pd.DataFrame(data)
        df.to_parquet(request.filename)

        return {"message": f"Chat saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))