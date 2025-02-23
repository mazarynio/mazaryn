from fastapi import FastAPI
from tf.post_tfrecord import router as post_router
from tf.user_tfrecord import router as user_router
from tf.chat_tfrecord import router as chat_router
from tf.business_tfrecord import router as business_router
from tf.user_csv import router as user_csv_router
from tf.user_json import router as user_json_router
from tf.post_json import router as post_json_router
from tf.user_parquet import router as user_parquet_router 
from tf.post_parquet import router as post_parquet_router  

app = FastAPI()

app.include_router(post_router, prefix="/api")
app.include_router(user_router, prefix="/api")
app.include_router(chat_router, prefix="/api")
app.include_router(business_router, prefix="/api")
app.include_router(user_csv_router, prefix="/api")
app.include_router(user_json_router, prefix="/api")
app.include_router(post_json_router, prefix="/api")
app.include_router(user_parquet_router, prefix="/api")
app.include_router(post_parquet_router, prefix="/api")  

# Run the server
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)