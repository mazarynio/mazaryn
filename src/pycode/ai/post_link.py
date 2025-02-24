from fastapi import APIRouter, HTTPException, Query
from typing import Optional
import tensorflow as tf
import datetime
from collections import defaultdict

router = APIRouter()

def parse_tfrecord(example_proto):
    feature_description = {
        'link_url': tf.io.FixedLenFeature([], tf.string),
        'date_created': tf.io.FixedLenFeature([], tf.string),
    }
    return tf.io.parse_single_example(example_proto, feature_description)

def count_links(dataset):
    link_counter = defaultdict(int)
    for record in dataset:
        link_url = record['link_url'].numpy().decode('utf-8')  
        if link_url:  
            link_counter[link_url] += 1
    return link_counter

def get_top_links(link_counter, top_n=10):
    sorted_links = sorted(link_counter.items(), key=lambda x: x[1], reverse=True)
    return sorted_links[:top_n]

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

@router.get("/top_links")
def get_top_links_api(
    period: str = Query(..., description="Time period: 'all_time', 'monthly', or 'daily'"),
    target_month: Optional[int] = Query(None, description="Target month (1-12)"),
    target_year: Optional[int] = Query(None, description="Target year (e.g., 2025)"),
    target_day: Optional[str] = Query(None, description="Target day in YYYY-MM-DD format")
):
    try:
        dataset = load_dataset()

        if period == "all_time":
            links = count_links(dataset)
            top_links = get_top_links(links)
            return {"top_links": top_links}

        elif period == "monthly":
            if not target_month or not target_year:
                raise HTTPException(status_code=400, detail="target_month and target_year are required for monthly period")
            filtered_dataset = filter_by_month(dataset, target_month, target_year)
            links = count_links(filtered_dataset)
            top_links = get_top_links(links)
            return {"top_links": top_links}

        elif period == "daily":
            if not target_day:
                raise HTTPException(status_code=400, detail="target_day is required for daily period")
            target_date = datetime.datetime.strptime(target_day, "%Y-%m-%d").date()
            filtered_dataset = filter_by_day(dataset, target_date)
            links = count_links(filtered_dataset)
            top_links = get_top_links(links)
            return {"top_links": top_links}

        else:
            raise HTTPException(status_code=400, detail="Invalid period. Use 'all_time', 'monthly', or 'daily'")

    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))