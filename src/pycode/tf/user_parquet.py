from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import pandas as pd
import io

router = APIRouter()

class UserDataRequest(BaseModel):
    id: str
    ai_user_id: str
    business_id: str
    ads_id: str
    quantum_id: str
    username: str
    password: str
    email: str
    address: str
    knode: str
    media: str
    post: str
    blog_post: str
    notif: str
    following: str
    follower: str
    blocked: str
    saved_posts: str
    other_info: str
    private: str
    date_created: str
    date_updated: str
    avatar_url: str
    banner_url: str
    token_id: str
    chat: str
    verified: str
    report: str
    level: str
    last_activity: str
    suspend: str
    filename: str  

@router.post("/convert_user_to_parquet")
def convert_user_to_parquet(request: UserDataRequest):
    try:
        # Create a DataFrame from the request data
        data = {
            "id": [request.id],
            "ai_user_id": [request.ai_user_id],
            "business_id": [request.business_id],
            "ads_id": [request.ads_id],
            "quantum_id": [request.quantum_id],
            "username": [request.username],
            "password": [request.password],
            "email": [request.email],
            "address": [request.address],
            "knode": [request.knode],
            "media": [request.media],
            "post": [request.post],
            "blog_post": [request.blog_post],
            "notif": [request.notif],
            "following": [request.following],
            "follower": [request.follower],
            "blocked": [request.blocked],
            "saved_posts": [request.saved_posts],
            "other_info": [request.other_info],
            "private": [request.private],
            "date_created": [request.date_created],
            "date_updated": [request.date_updated],
            "avatar_url": [request.avatar_url],
            "banner_url": [request.banner_url],
            "token_id": [request.token_id],
            "chat": [request.chat],
            "verified": [request.verified],
            "report": [request.report],
            "level": [request.level],
            "last_activity": [request.last_activity],
            "suspend": [request.suspend]
        }

        df = pd.DataFrame(data)

        # Save the DataFrame to a Parquet file
        df.to_parquet(request.filename)

        return {"message": f"User saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))