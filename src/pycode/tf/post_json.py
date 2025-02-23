from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import json
import os 

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

@router.post("/convert_post_to_json")
def convert_post_to_json(request: PostDataRequest):
    try:
        
        post_data = {
            "id": request.id,
            "ai_post_id": request.ai_post_id,
            "content": request.content,
            "emoji": request.emoji,
            "comments": request.comments,
            "likes": request.likes,
            "media": request.media,
            "hashtag": request.hashtag,
            "mention": request.mention,
            "link_url": request.link_url,
            "author": request.author,
            "date_created": request.date_created
        }

        with open(request.filename, "w") as f:
            json.dump(post_data, f, indent=4)  

        return {"message": f"Post saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/append_post_to_json")
def append_post_to_json(request: PostDataRequest):
    try:
        new_post = {
            "id": request.id,
            "ai_post_id": request.ai_post_id,
            "content": request.content,
            "emoji": request.emoji,
            "comments": request.comments,
            "likes": request.likes,
            "media": request.media,
            "hashtag": request.hashtag,
            "mention": request.mention,
            "link_url": request.link_url,
            "author": request.author,
            "date_created": request.date_created
        }

        if os.path.exists(request.filename):
            with open(request.filename, "r") as f:
                data = json.load(f)
            
            if isinstance(data, list):
                data.append(new_post)
            else:
                data = [data, new_post]
        else:
            data = [new_post]

        with open(request.filename, "w") as f:
            json.dump(data, f, indent=4)

        return {"message": f"Post appended to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))