import pandas as pd
import os

def get_fashion_keywords():
    """Return a DataFrame of fashion-related keywords."""
    keywords = [
        "fashion", "style", "clothing", "accessories", "footwear", "hairstyles", "makeup", "skincare", "runway", "fashion week",
        "designer brands", "street style", "vintage fashion", "high fashion", "fast fashion", "ethical fashion", "sustainable fashion", "luxury brands", "casual wear", "formal wear",
        "activewear", "sportswear", "swimwear", "outerwear", "seasonal fashion", "trendy", "classic style", "boho chic", "minimalist style", "urban fashion",
        "fashion accessories", "handbags", "jewelry", "sunglasses", "watches", "fashion influencers", "style bloggers", "fashion photography", "editorial shoots", "lookbooks",
        "fashion magazines", "Vogue", "Elle", "Harper's Bazaar", "GQ", "fashion shows", "catwalk", "couture", "ready-to-wear", "bespoke tailoring", "streetwear", 
        "athleisure", "sustainable fabrics", "organic cotton", "recycled fabrics", "fashion sustainability", "fashion revolution", "fast fashion impact", "upcycled fashion", "eco-friendly brands",
        "fashion design", "couture collections", "fashion houses", "fashion designers", "runway shows", "fashion stylists", "custom clothing", "tailored suits", "seamstress", "fashion trends",
        "fashion forecasting", "seasonal trends", "color trends", "fabric trends", "fashion bloggers", "fashion vloggers", "fashion tutorials", "personal style", "street fashion",
        "high street fashion", "independent designers", "local fashion", "fashion collaborations", "limited edition collections", "fashion advertising", "fashion campaigns", "celebrity fashion",
        "fashion icons", "red carpet fashion", "fashion week trends", "fashion culture", "luxury shopping", "designer boutiques", "fashion retail", "online fashion stores", "fashion e-commerce",
        "custom jewelry", "fashion makeup", "hair accessories", "fashion photography", "beauty trends", "seasonal wardrobes", "statement pieces", "fashionista", "fashion lovers",
        "fashion influencers", "vintage shopping", "thrift shopping", "secondhand fashion", "vintage clothing", "upcycled clothing", "personal shopper", "fashion consultancy", "fashion advice"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "fashion"})
    df = df.drop_duplicates(subset=["keyword"]) 
    return df

def save_fashion_keywords(output_file="fashion_keywords.parquet"):
    """Save fashion keywords to a parquet file. Append if the file already exists."""
    new_df = get_fashion_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique fashion keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique fashion keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_fashion_keywords()
