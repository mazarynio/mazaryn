from fastapi import APIRouter, HTTPException, Query
from typing import Optional
import tensorflow as tf
import datetime
from collections import defaultdict

router = APIRouter()

def parse_tfrecord(example_proto):
    feature_description = {
        'likes': tf.io.FixedLenFeature([], tf.string),
        'date_created': tf.io.FixedLenFeature([], tf.string),
        'author': tf.io.FixedLenFeature([], tf.string), 
        'id': tf.io.FixedLenFeature([], tf.string),
        'ai_post_id': tf.io.FixedLenFeature([], tf.string),
    }
    return tf.io.parse_single_example(example_proto, feature_description)

def count_likes(dataset):
    like_counter = defaultdict(int)
    for record in dataset:
        likes = record['likes'].numpy().decode('utf-8') 
        if likes:  
            like_counter[likes] += 1
    return like_counter

def get_top_likes(like_counter, top_n=10):
    sorted_likes = sorted(like_counter.items(), key=lambda x: x[1], reverse=True)
    return sorted_likes[:top_n]

def filter_by_postID(dataset, postID):
    filtered_records = []
    for record in dataset:
        record_postID = record['id'].numpy().decode('utf-8')  
        if record_postID == postID:
            filtered_records.append(record)
    return filtered_records

def filter_by_ai_post_id(dataset, ai_post_id):
    filtered_records = []
    for record in dataset:
        record_ai_post_id = record['ai_post_id'].numpy().decode('utf-8') 
        if record_ai_post_id == ai_post_id:
            filtered_records.append(record)
    return filtered_records

def filter_by_author(dataset, author):
    filtered_records = []
    for record in dataset:
        record_author = record['author'].numpy().decode('utf-8')  
        if record_author == author:
            filtered_records.append(record)
    return filtered_records

def filter_by_month(dataset, target_month, target_year):
    filtered_records = []
    for record in dataset:
        date_created = record['date_created'].numpy().decode('utf-8')  
        record_date = datetime.datetime.strptime(date_created, "%Y-%m-%d %H:%M:%S")
        if record_date.month == target_month and record_date.year == target_year:
            filtered_records.append(record)
    return filtered_records

def filter_by_day(dataset, target_date):
    filtered_records = []
    for record in dataset:
        date_created = record['date_created'].numpy().decode('utf-8')  
        record_date = datetime.datetime.strptime(date_created, "%Y-%m-%d %H:%M:%S").date()
        if record_date == target_date:
            filtered_records.append(record)
    return filtered_records

def load_dataset():
    filenames = ['post.tfrecord']
    raw_dataset = tf.data.TFRecordDataset(filenames)
    parsed_dataset = raw_dataset.map(parse_tfrecord)
    return parsed_dataset

@router.get("/top_likes")
def get_top_likes_api(
    period: str = Query(..., description="Time period: 'all_time', 'monthly', or 'daily'"),
    target_month: Optional[int] = Query(None, description="Target month (1-12)"),
    target_year: Optional[int] = Query(None, description="Target year (e.g., 2025)"),
    target_day: Optional[str] = Query(None, description="Target day in YYYY-MM-DD format")
):
    try:
        dataset = load_dataset()

        if period == "all_time":
            likes = count_likes(dataset)
            top_likes = get_top_likes(likes)
            return {"top_likes": top_likes}

        elif period == "monthly":
            if not target_month or not target_year:
                raise HTTPException(status_code=400, detail="target_month and target_year are required for monthly period")
            filtered_dataset = filter_by_month(dataset, target_month, target_year)
            likes = count_likes(filtered_dataset)
            top_likes = get_top_likes(likes)
            return {"top_likes": top_likes}

        elif period == "daily":
            if not target_day:
                raise HTTPException(status_code=400, detail="target_day is required for daily period")
            target_date = datetime.datetime.strptime(target_day, "%Y-%m-%d").date()
            filtered_dataset = filter_by_day(dataset, target_date)
            likes = count_likes(filtered_dataset)
            top_likes = get_top_likes(likes)
            return {"top_likes": top_likes}

        else:
            raise HTTPException(status_code=400, detail="Invalid period. Use 'all_time', 'monthly', or 'daily'")

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/likes_by_postID")
def get_likes_by_postID_api(
    postID: str = Query(..., description="Post ID")
):
    try:
        dataset = load_dataset()

        filtered_dataset = filter_by_postID(dataset, postID)

        likes = count_likes(filtered_dataset)
        top_likes = get_top_likes(likes)
        return {"top_likes": top_likes}

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get("/likes_by_ai_post_id")
def get_likes_by_ai_post_id_api(
    ai_post_id: str = Query(..., description="AI Post ID")
):
    try:
        dataset = load_dataset()

        # Filter by ai_post_id
        filtered_dataset = filter_by_ai_post_id(dataset, ai_post_id)

        likes = count_likes(filtered_dataset)
        top_likes = get_top_likes(likes)
        return {"top_likes": top_likes}

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))