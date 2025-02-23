from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import csv
import io

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

@router.post("/convert_post_to_csv")
def convert_post_to_csv(request: PostDataRequest):
    try:
        output = io.StringIO()
        writer = csv.writer(output, quoting=csv.QUOTE_ALL)  

    
        writer.writerow([
            "id", "ai_post_id", "content", "emoji", "comments", "likes", "media", "hashtag", 
            "mention", "link_url", "author", "date_created"
        ])

    
        writer.writerow([
            request.id, request.ai_post_id, request.content, request.emoji, request.comments, 
            request.likes, request.media, request.hashtag, request.mention, request.link_url, 
            request.author, request.date_created
        ])

    
        with open(request.filename, "w") as f:
            f.write(output.getvalue())

        return {"message": f"Post saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))