import pandas as pd
import os

def get_science_keywords():
    """Return a DataFrame of science-related keywords."""
    keywords = [
        "physics", "chemistry", "biology", "astronomy", "geology", "meteorology", "oceanography", "ecology", "genetics", "neuroscience",
        "biochemistry", "microbiology", "zoology", "botany", "paleontology", "astrophysics", "quantum mechanics", "relativity", "thermodynamics", "electromagnetism",
        "organic chemistry", "inorganic chemistry", "physical chemistry", "analytical chemistry", "materials science", "environmental science", "climatology", "geophysics", "seismology", "volcanology",
        "space exploration", "cosmology", "exoplanets", "astrobiology", "particle physics", "nanotechnology", "biotechnology", "robotics", "artificial intelligence", "cognitive science",
        "psychology", "sociology", "anthropology", "archaeology", "linguistics", "mathematics", "statistics", "probability", "calculus", "algebra",
        "geometry", "topology", "number theory", "combinatorics", "cryptography", "computer science", "software engineering", "data structures", "algorithms", "machine learning", 
        "neural networks", "deep learning", "natural language processing", "reinforcement learning", "computer vision", "augmented reality", "virtual reality", "blockchain", "internet of things",
        "quantum computing", "bioinformatics", "synthetic biology", "gene editing", "CRISPR", "environmental chemistry", "climate change", "renewable energy", "clean energy", "fusion energy", 
        "space travel", "black holes", "dark matter", "dark energy", "gravitational waves", "nanomedicine", "personalized medicine", "gene therapy", "3D printing", "smart materials", 
        "artificial photosynthesis", "carbon capture", "fusion power", "bioengineering", "sustainable development", "geoinformatics", "geoengineering", "astroengineering", "metabolic engineering", 
        "synthetic chemistry", "superconductivity", "neuroscience and AI", "quantum cryptography", "synthetic intelligence", "space mining", "biohacking", "biomimicry", "environmental toxicology"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "science"})
    df = df.drop_duplicates(subset=["keyword"])  
    return df

def save_science_keywords(output_file="science_keywords.parquet"):
    """Save science keywords to a parquet file. Append if the file already exists."""
    new_df = get_science_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique science keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique science keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_science_keywords()
