import pandas as pd
from dataset import (
    art_keywords,
    entertainment_keywords,
    fashion_keywords,
    finance_keywords,
    psychology_keywords,
    science_keywords,
    sport_keywords,
    tech_keywords,
    travel_keywords
)

def save_all_keywords(output_file="keywords.parquet"):
    # Load keywords for each category
    art_df = art_keywords.get_art_keywords()
    print(f"Loaded {len(art_df)} art keywords.")

    entertainment_df = entertainment_keywords.get_entertainment_keywords()
    print(f"Loaded {len(entertainment_df)} entertainment keywords.")

    fashion_df = fashion_keywords.get_fashion_keywords()
    print(f"Loaded {len(fashion_df)} fashion keywords.")

    finance_df = finance_keywords.get_finance_keywords()
    print(f"Loaded {len(finance_df)} finance keywords.")

    psychology_df = psychology_keywords.get_psychology_keywords()
    print(f"Loaded {len(psychology_df)} psychology keywords.")

    science_df = science_keywords.get_science_keywords()
    print(f"Loaded {len(science_df)} science keywords.")

    sport_df = sport_keywords.get_sports_keywords()
    print(f"Loaded {len(sport_df)} sport keywords.")

    tech_df = tech_keywords.get_tech_keywords()
    print(f"Loaded {len(tech_df)} tech keywords.")

    travel_df = travel_keywords.get_travel_keywords()
    print(f"Loaded {len(travel_df)} travel keywords.")

    # Combine all DataFrames and remove duplicates
    combined_df = pd.concat([
        art_df,
        entertainment_df,
        fashion_df,
        finance_df,
        psychology_df,
        science_df,
        sport_df,
        tech_df,
        travel_df
    ]).drop_duplicates(subset=["keyword"])

    print(f"Total unique keywords: {len(combined_df)}")

    # Save to Parquet file
    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    print(f"Saved all keywords to {output_file}")

if __name__ == "__main__":
    save_all_keywords()