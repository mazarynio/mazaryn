from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import tensorflow as tf
import logging
import os

# Suppress TensorFlow logging
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
logging.getLogger("tensorflow").setLevel(logging.ERROR)

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

@router.post("/convert_user_to_tfrecord")
def convert_user_to_tfrecord(request: UserDataRequest):
    try:
        example = tf.train.Example()

    
        example.features.feature["id"].bytes_list.value.append(request.id.encode("utf-8"))
        example.features.feature["ai_user_id"].bytes_list.value.append(request.ai_user_id.encode("utf-8"))
        example.features.feature["business_id"].bytes_list.value.append(request.business_id.encode("utf-8"))
        example.features.feature["ads_id"].bytes_list.value.append(request.ads_id.encode("utf-8"))
        example.features.feature["quantum_id"].bytes_list.value.append(request.quantum_id.encode("utf-8"))
        example.features.feature["username"].bytes_list.value.append(request.username.encode("utf-8"))
        example.features.feature["password"].bytes_list.value.append(request.password.encode("utf-8"))
        example.features.feature["email"].bytes_list.value.append(request.email.encode("utf-8"))
        example.features.feature["address"].bytes_list.value.append(request.address.encode("utf-8"))
        example.features.feature["knode"].bytes_list.value.append(request.knode.encode("utf-8"))
        example.features.feature["media"].bytes_list.value.append(request.media.encode("utf-8"))
        example.features.feature["post"].bytes_list.value.append(request.post.encode("utf-8"))
        example.features.feature["blog_post"].bytes_list.value.append(request.blog_post.encode("utf-8"))
        example.features.feature["notif"].bytes_list.value.append(request.notif.encode("utf-8"))
        example.features.feature["following"].bytes_list.value.append(request.following.encode("utf-8"))
        example.features.feature["follower"].bytes_list.value.append(request.follower.encode("utf-8"))
        example.features.feature["blocked"].bytes_list.value.append(request.blocked.encode("utf-8"))
        example.features.feature["saved_posts"].bytes_list.value.append(request.saved_posts.encode("utf-8"))
        example.features.feature["other_info"].bytes_list.value.append(request.other_info.encode("utf-8"))
        example.features.feature["private"].bytes_list.value.append(request.private.encode("utf-8"))
        example.features.feature["date_created"].bytes_list.value.append(request.date_created.encode("utf-8"))
        example.features.feature["date_updated"].bytes_list.value.append(request.date_updated.encode("utf-8"))
        example.features.feature["avatar_url"].bytes_list.value.append(request.avatar_url.encode("utf-8"))
        example.features.feature["banner_url"].bytes_list.value.append(request.banner_url.encode("utf-8"))
        example.features.feature["token_id"].bytes_list.value.append(request.token_id.encode("utf-8"))
        example.features.feature["chat"].bytes_list.value.append(request.chat.encode("utf-8"))
        example.features.feature["verified"].bytes_list.value.append(request.verified.encode("utf-8"))
        example.features.feature["report"].bytes_list.value.append(request.report.encode("utf-8"))
        example.features.feature["level"].bytes_list.value.append(request.level.encode("utf-8"))
        example.features.feature["last_activity"].bytes_list.value.append(request.last_activity.encode("utf-8"))
        example.features.feature["suspend"].bytes_list.value.append(request.suspend.encode("utf-8"))

        serialized_data = example.SerializeToString()

        with tf.io.TFRecordWriter(request.filename) as writer:
            writer.write(serialized_data)

        return {"message": f"User saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))