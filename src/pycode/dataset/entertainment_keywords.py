import pandas as pd
import os

def get_entertainment_keywords():
    """Return a DataFrame of entertainment-related keywords."""
    keywords = [
        "movies", "television", "music", "concerts", "theater", "comedy", "stand-up", "magic shows", "festivals", "circus",
        "video games", "board games", "puzzles", "escape rooms", "theme parks", "amusement parks", "virtual reality games", "live streaming", "podcasts", "audiobooks",
        "cinema", "film festivals", "indie films", "blockbusters", "documentaries", "animation", "anime", "manga", "cosplay", "fan conventions",
        "celebrity gossip", "pop culture", "reality TV", "talk shows", "late night shows", "music festivals", "DJ sets", "karaoke", "dance performances", "musicals",
        "art exhibitions", "photography", "vlogging", "influencers", "social media trends", "streaming platforms", "Netflix", "HBO", "Disney+", "Amazon Prime",
        "YouTube", "Twitch", "TikTok", "Instagram Reels", "short films", "sports events", "eSports", "gaming tournaments", "streaming services", "online gaming",
        "karaoke bars", "clubs", "nightlife", "pop music", "rock music", "classical music", "hip-hop", "electronic dance music", "jazz music", "blues music", "symphonies",
        "comedy clubs", "improv comedy", "open mic", "musical theater", "singers", "songwriters", "rappers", "DJ performances", "musical performances", "rock concerts",
        "pop concerts", "classical concerts", "theater productions", "Broadway", "West End", "playwrights", "film directors", "film producers", "actors", "actresses",
        "voice acting", "animation studios", "art house films", "foreign films", "cult films", "true crime documentaries", "biographical documentaries", "live theater",
        "musical concerts", "indie music", "singer-songwriters", "cover bands", "film critics", "movie reviews", "celebrity interviews", "Hollywood news", "Hollywood stars",
        "actors' biographies", "red carpet events", "fashion shows", "celebrity fashion", "celebrity endorsements", "behind-the-scenes", "pop idols", "boy bands", "girl bands",
        "celebrity podcasts", "fan clubs", "celebrity news", "celebrity fashion trends", "online concerts", "crowd funding", "crowd sourcing", "independent artists",
        "viral challenges", "TikTok challenges", "viral videos", "streaming series", "movie marathons", "celebrity gaming", "celebrity music", "viral music videos", "music videos",
        "audio streaming", "music platforms", "SoundCloud", "Spotify", "Apple Music", "Pandora", "radio stations", "digital content creators", "live events", "theater reviews"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "entertainment"})
    df = df.drop_duplicates(subset=["keyword"])  
    return df

def save_entertainment_keywords(output_file="entertainment_keywords.parquet"):
    """Save entertainment keywords to a parquet file. Append if the file already exists."""
    new_df = get_entertainment_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique entertainment keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique entertainment keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_entertainment_keywords()
