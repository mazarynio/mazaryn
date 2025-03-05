import pandas as pd
import os

def get_travel_keywords():
    """Return a DataFrame of travel-related keywords."""
    keywords = [
        "travel", "tourism", "vacation", "adventure", "road trip", "backpacking", "sightseeing", "cruise", "flight", "airlines",
        "hotel", "resort", "hostel", "camping", "glamping", "travel photography", "travel blogging", "cultural tourism", "ecotourism", "luxury travel",
        "budget travel", "solo travel", "family trip", "group tours", "city break", "beach holiday", "mountain climbing", "national parks", "historical sites", "world heritage",
        "travel insurance", "visa application", "travel guide", "itinerary", "tourist attractions", "local cuisine", "travel tips", "packing list", "travel apps", "language barriers",
        "adventure sports", "hiking trails", "scuba diving", "safari", "road maps", "eco-friendly travel", "digital nomad", "remote work travel", "staycations", "glamping sites",
        "wellness tourism", "sustainable tourism", "food tourism", "adventure tourism", "nature tourism", "volunteer travel", "medical tourism", "luxury cruises", "destination wedding",
        "family-friendly destinations", "honeymoon destinations", "off-the-beaten-path travel", "local experiences", "travel concierge", "personalized travel", "slow travel",
        "jet lag recovery", "travel gear", "travel safety", "cultural exchange", "workcations", "short trips", "weekend getaway", "travel bloggers", "travel influencers", "luxury hotels",
        "boutique hotels", "hotel reviews", "holiday homes", "vacation rentals", "digital detox", "traveler’s health", "bucket list destinations", "volcano tourism", "snowboarding",
        "ski resorts", "wine tourism", "adventure parks", "tour packages", "private tours", "luxury safari", "glamping experiences", "group travel tips", "destination travel",
        "airline loyalty programs", "travel hacks", "local festivals", "destination guides", "travel podcasts", "cultural immersion", "authentic experiences"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "travel"})
    df = df.drop_duplicates(subset=["keyword"])  
    return df

def save_travel_keywords(output_file="travel_keywords.parquet"):
    """Save travel keywords to a parquet file. Append if the file already exists."""
    new_df = get_travel_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique travel keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique travel keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_travel_keywords()
