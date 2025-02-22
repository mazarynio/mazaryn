from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import csv
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

@router.post("/convert_user_to_csv")
def convert_user_to_csv(request: UserDataRequest):
    try:
        output = io.StringIO()
        writer = csv.writer(output, quoting=csv.QUOTE_ALL)  

        writer.writerow([
            "id", "ai_user_id", "business_id", "ads_id", "quantum_id", "username", "password", "email", "address", 
            "knode", "media", "post", "blog_post", "notif", "following", "follower", "blocked", "saved_posts", 
            "other_info", "private", "date_created", "date_updated", "avatar_url", "banner_url", "token_id", 
            "chat", "verified", "report", "level", "last_activity", "suspend"
        ])

    
        writer.writerow([
            request.id, request.ai_user_id, request.business_id, request.ads_id, request.quantum_id, request.username, 
            request.password, request.email, request.address, request.knode, request.media, request.post, 
            request.blog_post, request.notif, request.following, request.follower, request.blocked, request.saved_posts, 
            request.other_info, request.private, request.date_created, request.date_updated, request.avatar_url, 
            request.banner_url, request.token_id, request.chat, request.verified, request.report, request.level, 
            request.last_activity, request.suspend
        ])

    
        with open(request.filename, "w") as f:
            f.write(output.getvalue())

        return {"message": f"User saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))