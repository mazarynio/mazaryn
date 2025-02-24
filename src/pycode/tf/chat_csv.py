from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import csv
import io
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

@router.post("/convert_chat_to_csv")
def convert_chat_to_csv(request: ChatDataRequest):
    try:
        output = io.StringIO()
        writer = csv.writer(output, quoting=csv.QUOTE_ALL)

        writer.writerow([
            "id", "ai_chat_id", "user_id", "recipient_id", "body", "media", 
            "bot", "date_created", "date_updated"
        ])

        writer.writerow([
            request.id, request.ai_chat_id, request.user_id, request.recipient_id, 
            request.body, request.media, request.bot, request.date_created, 
            request.date_updated
        ])

        with open(request.filename, "w") as f:
            f.write(output.getvalue())

        return {"message": f"Chat saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/append_chat_to_csv")
def append_chat_to_csv(request: ChatDataRequest):
    try:
        file_exists = os.path.exists(request.filename)

        with open(request.filename, mode="a", newline="", encoding="utf-8") as f:
            writer = csv.writer(f, quoting=csv.QUOTE_ALL)

            if not file_exists:
                writer.writerow([
                    "id", "ai_chat_id", "user_id", "recipient_id", "body", "media", 
                    "bot", "date_created", "date_updated"
                ])

            
            writer.writerow([
                request.id, request.ai_chat_id, request.user_id, request.recipient_id, 
                request.body, request.media, request.bot, request.date_created, 
                request.date_updated
            ])

        return {"message": f"Chat appended to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))