import pandas as pd
import os

def get_sports_keywords():
    """Return a DataFrame of sports-related keywords."""
    keywords = [
        "football", "basketball", "soccer", "tennis", "walking", "baseball", "golf", "swimming", "volleyball", "hiking",
        "hockey", "rugby", "cricket", "track", "field", "martial arts", "wrestling", "boxing", "cycling",
        "badminton", "table tennis", "skiing", "snowboarding", "surfing", "skateboarding", "fencing", "archery", "rowing", "sailing",
        "climbing", "karate", "judo", "taekwondo", "weightlifting", "powerlifting", "bodybuilding", "crossfit", "pilates", "yoga",
        "gymnastics", "cheerleading", "equestrian", "polo", "lacrosse", "handball", "curling", "biathlon", "triathlon", "pentathlon",
        "motorsport", "karting", "drifting", "endurance racing", "motocross", "freediving", "scuba diving", "kayaking", "canoeing", "snorkeling",
        "paragliding", "skydiving", "bungee jumping", "parkour", "ultimate frisbee", "disc golf", "dodgeball", "kickball", "softball", "paddleboarding",
        "kitesurfing", "windsurfing", "sandboarding", "mountaineering", "orienteering"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "sports"})
    df = df.drop_duplicates(subset=["keyword"])
    return df

def save_sports_keywords(output_file="sports_keywords.parquet"):
    """Save sports keywords to a parquet file. Append if the file already exists."""
    new_df = get_sports_keywords()
    
    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique sports keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique sports keywords to {output_file}")
    
    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_sports_keywords()
