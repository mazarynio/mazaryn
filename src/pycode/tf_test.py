import tensorflow as tf

# Read the TFRecord file
for record in tf.data.TFRecordDataset("post.tfrecord"):
    example = tf.train.Example()
    example.ParseFromString(record.numpy())
    print(example)