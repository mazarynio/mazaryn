import pandas as pd
import os

def get_art_keywords():
    """Return a DataFrame of art-related keywords."""
    keywords = [
        "painting", "drawing", "sculpture", "photography", "calligraphy", "graphic design", "illustration", "digital art", "collage", "printmaking",
        "ceramics", "mosaic", "graffiti", "tattoo art", "installation art", "conceptual art", "performance art", "video art", "street art", "textile art",
        "animation", "carving", "engraving", "stained glass", "paper art", "folk art", "pop art", "abstract art", "surrealism", "realism",
        "impressionism", "expressionism", "minimalism", "modern art", "contemporary art", "baroque art", "renaissance art", "romanticism", "cubism", "futurism",
        "dadaism", "photorealism", "art deco", "art nouveau", "postmodern art", "landscape art", "portraiture", "still life", "concept art", "character design",
        "watercolor", "oil painting", "acrylic painting", "pastel", "chalk", "pencil sketch", "mixed media", "urban art", "public art", "fine art", "figurative art",
        "abstract expressionism", "neo-expressionism", "hyperrealism", "vintage art", "baroque sculpture", "classical sculpture", "wood sculpture", "stone sculpture",
        "bronze sculpture", "ceramic sculpture", "photo collage", "paper collage", "digital painting", "3D art", "motion graphics", "augmented reality art",
        "installation art", "sound art", "interactive art", "light art", "street photography", "art photography", "fashion photography", "portrait photography",
        "architectural photography", "fine art photography", "experimental art", "outsider art", "indigenous art", "tribal art", "folk art", "native art", "art curation",
        "gallery", "art exhibition", "museum", "art collection", "art auction", "art market", "art criticism", "art history", "art theory", "art conservation",
        "digital illustration", "vector art", "graffiti style", "character illustration", "3D modeling", "rendering", "motion design", "storyboarding", "conceptual illustration",
        "game art", "comic book art", "graphic novel", "storytelling", "design thinking", "visual storytelling", "art direction", "branding design", "logo design",
        "typography", "font design", "web design", "user interface design", "user experience design", "advertisement design", "poster design", "packaging design",
        "environmental design", "space design", "product design", "industrial design", "exhibition design", "fashion design", "textile design", "furniture design", "interior design"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "art"})
    df = df.drop_duplicates(subset=["keyword"])  
    return df

def save_art_keywords(output_file="art_keywords.parquet"):
    """Save art keywords to a parquet file. Append if the file already exists."""
    new_df = get_art_keywords()
    
    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique art keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique art keywords to {output_file}")
    
    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_art_keywords()
