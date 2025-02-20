from fastapi import FastAPI
from tf.post_tfrecord import router as post_router
from tf.user_tfrecord import router as user_router

app = FastAPI()

# Include the routers
app.include_router(post_router, prefix="/api")
app.include_router(user_router, prefix="/api")

# Run the server
if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000)