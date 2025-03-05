from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import json
import os

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

@router.post("/convert_chat_to_json")
def convert_chat_to_json(request: ChatDataRequest):
    try:
        chat_data = {
            "id": request.id,
            "ai_chat_id": request.ai_chat_id,
            "user_id": request.user_id,
            "recipient_id": request.recipient_id,
            "body": request.body,
            "media": request.media,
            "bot": request.bot,
            "date_created": request.date_created,
            "date_updated": request.date_updated
        }

        with open(request.filename, "w") as f:
            json.dump(chat_data, f, indent=4)

        return {"message": f"Chat saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/append_chat_to_json")
def append_chat_to_json(request: ChatDataRequest):
    try:
        new_chat = {
            "id": request.id,
            "ai_chat_id": request.ai_chat_id,
            "user_id": request.user_id,
            "recipient_id": request.recipient_id,
            "body": request.body,
            "media": request.media,
            "bot": request.bot,
            "date_created": request.date_created,
            "date_updated": request.date_updated
        }

        if os.path.exists(request.filename):
            with open(request.filename, "r") as f:
                data = json.load(f)
            
            if isinstance(data, list):
                data.append(new_chat)
            else:
                data = [data, new_chat]
        else:
            data = [new_chat]

        with open(request.filename, "w") as f:
            json.dump(data, f, indent=4)

        return {"message": f"Chat appended to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))