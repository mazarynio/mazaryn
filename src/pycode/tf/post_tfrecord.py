from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import tensorflow as tf
import logging
import os

# Suppress TensorFlow logging
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
logging.getLogger("tensorflow").setLevel(logging.ERROR)

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


@router.post("/convert_post_to_tfrecord")
def convert_to_tfrecord(request: PostDataRequest):
    try:
        example = tf.train.Example()

        example.features.feature["id"].bytes_list.value.append(request.id.encode("utf-8"))
        example.features.feature["ai_post_id"].bytes_list.value.append(request.ai_post_id.encode("utf-8"))
        example.features.feature["content"].bytes_list.value.append(request.content.encode("utf-8"))
        example.features.feature["emoji"].bytes_list.value.append(request.emoji.encode("utf-8"))
        example.features.feature["comments"].bytes_list.value.append(request.comments.encode("utf-8"))
        example.features.feature["likes"].bytes_list.value.append(request.likes.encode("utf-8"))
        example.features.feature["media"].bytes_list.value.append(request.media.encode("utf-8"))
        example.features.feature["hashtag"].bytes_list.value.append(request.hashtag.encode("utf-8"))
        example.features.feature["mention"].bytes_list.value.append(request.mention.encode("utf-8"))
        example.features.feature["link_url"].bytes_list.value.append(request.link_url.encode("utf-8"))
        example.features.feature["author"].bytes_list.value.append(request.author.encode("utf-8"))
        example.features.feature["date_created"].bytes_list.value.append(request.date_created.encode("utf-8"))

        serialized_data = example.SerializeToString()

        with tf.io.TFRecordWriter(request.filename) as writer:
            writer.write(serialized_data)

        return {"message": f"Post saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))