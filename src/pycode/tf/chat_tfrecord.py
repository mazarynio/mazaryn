from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
import tensorflow as tf
import logging
import os

# Suppress TensorFlow logging
os.environ["TF_CPP_MIN_LOG_LEVEL"] = "3"
logging.getLogger("tensorflow").setLevel(logging.ERROR)

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

@router.post("/convert_chat_to_tfrecord")
def convert_chat_to_tfrecord(request: ChatDataRequest):
    try:
        example = tf.train.Example()

        example.features.feature["id"].bytes_list.value.append(request.id.encode("utf-8"))
        example.features.feature["ai_chat_id"].bytes_list.value.append(request.ai_chat_id.encode("utf-8"))
        example.features.feature["user_id"].bytes_list.value.append(request.user_id.encode("utf-8"))
        example.features.feature["recipient_id"].bytes_list.value.append(request.recipient_id.encode("utf-8"))
        example.features.feature["body"].bytes_list.value.append(request.body.encode("utf-8"))
        example.features.feature["media"].bytes_list.value.append(request.media.encode("utf-8"))
        example.features.feature["bot"].bytes_list.value.append(request.bot.encode("utf-8"))
        example.features.feature["date_created"].bytes_list.value.append(request.date_created.encode("utf-8"))
        example.features.feature["date_updated"].bytes_list.value.append(request.date_updated.encode("utf-8"))

        serialized_data = example.SerializeToString()

        with tf.io.TFRecordWriter(request.filename) as writer:
            writer.write(serialized_data)

        return {"message": f"Chat saved to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post("/append_chat_to_tfrecord")
def append_chat_to_tfrecord(request: ChatDataRequest):
    try:
        example = tf.train.Example()

        example.features.feature["id"].bytes_list.value.append(request.id.encode("utf-8"))
        example.features.feature["ai_chat_id"].bytes_list.value.append(request.ai_chat_id.encode("utf-8"))
        example.features.feature["user_id"].bytes_list.value.append(request.user_id.encode("utf-8"))
        example.features.feature["recipient_id"].bytes_list.value.append(request.recipient_id.encode("utf-8"))
        example.features.feature["body"].bytes_list.value.append(request.body.encode("utf-8"))
        example.features.feature["media"].bytes_list.value.append(request.media.encode("utf-8"))
        example.features.feature["bot"].bytes_list.value.append(request.bot.encode("utf-8"))
        example.features.feature["date_created"].bytes_list.value.append(request.date_created.encode("utf-8"))
        example.features.feature["date_updated"].bytes_list.value.append(request.date_updated.encode("utf-8"))

        serialized_data = example.SerializeToString()

        existing_records = []
        if os.path.exists(request.filename):
            for record in tf.data.TFRecordDataset(request.filename):
                existing_records.append(record.numpy())

        existing_records.append(serialized_data)

        with tf.io.TFRecordWriter(request.filename) as writer:
            for record in existing_records:
                writer.write(record)

        return {"message": f"Chat appended to {request.filename}"}
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))