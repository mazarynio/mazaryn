from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import pandas as pd

router = APIRouter()

class PostDataRequest(BaseModel):
    id: str
    ai_post_id: str
    content: str
    emoji: str
    comments: str
    likes: str
    media: str
    hashtag: str
    mention: str
    link_url: str
    author: str
    date_created: str
    filename: str

@router.post("/convert_post_to_parquet")
def convert_post_to_parquet(request: PostDataRequest):
    try:
        data = {
            "id": [request.id],
            "ai_post_id": [request.ai_post_id],
            "content": [request.content],
            "emoji": [request.emoji],
            "comments": [request.comments],
            "likes": [request.likes],
            "media": [request.media],
            "hashtag": [request.hashtag],
            "mention": [request.mention],
            "link_url": [request.link_url],
            "author": [request.author],
            "date_created": [request.date_created]
        }

        df = pd.DataFrame(data)

        df.to_parquet(request.filename)

        return {"message": f"Post saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))