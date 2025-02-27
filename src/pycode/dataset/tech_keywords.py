import pandas as pd
import os

def get_tech_keywords():
    """Return a DataFrame of technology-related keywords."""
    keywords = [
        "artificial intelligence", "machine learning", "deep learning", "neural networks", "data science", "big data", "cloud computing", "cybersecurity", "blockchain",
        "internet of things", "5G", "augmented reality", "virtual reality", "mixed reality", "quantum computing", "edge computing", "microservices", "API development", "web development",
        "mobile development", "full stack development", "frontend", "backend", "devops", "CI/CD", "containerization", "Docker", "Kubernetes", "serverless computing",
        "database management", "SQL", "NoSQL", "graph databases", "distributed systems", "networking", "cloud storage", "SaaS", "PaaS", "IaaS",
        "cryptography", "computer vision", "NLP", "robotics", "automation", "software engineering", "embedded systems", "VLSI", "FPGA", "IoT devices",
        "digital twins", "AR development", "VR development", "game development", "UI/UX design", "human-computer interaction", "ethical hacking", "penetration testing", "firewalls",
        "encryption", "decryption", "machine vision", "speech recognition", "gesture recognition", "augmented intelligence", "AI ethics", "autonomous systems", "edge AI", 
        "self-driving cars", "blockchain development", "cryptocurrency", "metaverse", "smart contracts", "NFTs", "digital transformation", "5G networks", "robotic process automation", 
        "cloud-native development", "chatbots", "digital assistants", "natural language generation", "AI in healthcare", "AI in finance", "biometrics", "facial recognition", "IoT security",
        "biotechnology", "sustainable tech", "green computing", "cloud security", "RPA (robotic process automation)", "smart homes", "wearables", "cyber-physical systems", "3D printing",
        "autonomous drones", "smart cities", "data lakes", "data wrangling", "AI models", "machine ethics", "5G infrastructure", "computer vision models", "AI-based healthcare solutions",
        "AI in retail", "cloud migration", "digital marketing automation", "serverless architecture", "data privacy", "edge AI devices", "AI for good", "chatbot development"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "technology"})
    df = df.drop_duplicates(subset=["keyword"]) 
    return df

def save_tech_keywords(output_file="tech_keywords.parquet"):
    """Save technology keywords to a parquet file. Append if the file already exists."""
    new_df = get_tech_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique tech keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique tech keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_tech_keywords()
