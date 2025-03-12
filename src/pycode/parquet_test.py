import pandas as pd

def read_parquet_file(filename):
    try:
        df = pd.read_parquet(filename)
        
        print("Data read from Parquet file:")
        print(df)
        
        print("\nSchema of the Parquet file:")
        print(df.dtypes)
        
        print(f"\nNumber of rows: {len(df)}")
        print(f"Number of columns: {len(df.columns)}")
        
    except Exception as e:
        print(f"Error reading Parquet file: {e}")

if __name__ == "__main__":
    parquet_file = "post.parquet"
    
    read_parquet_file(parquet_file)