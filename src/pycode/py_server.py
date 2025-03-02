from fastapi import FastAPI
from tf.post_tfrecord import router as post_router
from tf.user_tfrecord import router as user_router
from tf.chat_tfrecord import router as chat_router
from tf.chat_csv import router as chat_csv_router
from tf.chat_parquet import router as chat_parquet_router
from tf.chat_json import router as chat_json_router
from tf.business_tfrecord import router as business_router
from tf.user_csv import router as user_csv_router
from tf.post_csv import router as post_csv_router
from tf.user_json import router as user_json_router
from tf.post_json import router as post_json_router
from tf.user_parquet import router as user_parquet_router 
from tf.post_parquet import router as post_parquet_router 
from ai.post_hashtag import router as post_hashtag_router 
from ai.post_link import router as post_link_router 
from ai.post_mention import router as post_mention_router
from ai.post_emoji import router as post_emoji_router
from ai.post_like import router as post_like_router 
from ai.post_predict import router as post_predict_router

app = FastAPI()

app.include_router(post_router, prefix="/api")
app.include_router(user_router, prefix="/api")
app.include_router(chat_router, prefix="/api")
app.include_router(chat_csv_router, prefix="/api")
app.include_router(chat_parquet_router, prefix="/api")
app.include_router(chat_json_router, prefix="/api")
app.include_router(business_router, prefix="/api")
app.include_router(user_csv_router, prefix="/api")
app.include_router(post_csv_router, prefix="/api")
app.include_router(user_json_router, prefix="/api")
app.include_router(post_json_router, prefix="/api")
app.include_router(user_parquet_router, prefix="/api")
app.include_router(post_parquet_router, prefix="/api") 
app.include_router(post_hashtag_router, prefix="/api")
app.include_router(post_link_router, prefix="/api")
app.include_router(post_mention_router, prefix="/api")
app.include_router(post_emoji_router, prefix="/api") 
app.include_router(post_like_router, prefix="/api")
app.include_router(post_predict_router, prefix="/api")

# Run the server
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)