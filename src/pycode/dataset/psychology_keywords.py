import pandas as pd
import os

def get_psychology_keywords():
    """Return a DataFrame of psychology-related keywords."""
    keywords = [
        "psychology", "mental health", "cognitive science", "behavioral therapy", "psychoanalysis", "emotional intelligence", "self-awareness", "personality types",
        "neuroscience", "counseling", "psychotherapy", "clinical psychology", "developmental psychology", "social psychology", "positive psychology",
        "mindfulness", "meditation", "stress management", "anxiety", "depression", "mental disorders", "phobias", "trauma", "PTSD", "OCD",
        "psychological research", "case studies", "behavioral science", "cognitive behavioral therapy", "attachment theory", "motivation", "learning theories",
        "conditioning", "reinforcement", "habit formation", "memory", "perception", "decision making", "problem solving", "creativity",
        "intelligence", "emotional regulation", "self-esteem", "assertiveness", "communication skills", "emotional health", "self-compassion",
        "psychopathology", "behavior modification", "family therapy", "couples therapy", "group therapy", "art therapy", "play therapy", "mind-body connection",
        "neuroplasticity", "growth mindset", "behavioral economics", "cognitive biases", "self-care", "resilience", "psychological well-being", "existential psychology",
        "behavioral activation", "anger management", "psychological flexibility", "self-regulation", "hypnotherapy", "clinical assessments", "neurodevelopmental disorders",
        "attention deficit disorder", "autism spectrum disorder", "bipolar disorder", "schizophrenia", "borderline personality disorder", "eating disorders", 
        "addiction psychology", "workplace psychology", "school psychology", "forensic psychology", "health psychology", "psychosomatic disorders", "human behavior",
        "social cognition", "cognitive neuroscience", "interpersonal relationships", "mental health advocacy", "psychological testing", "psychometric assessments",
        "therapy techniques", "solution-focused therapy", "dialectical behavior therapy", "motivational interviewing", "narrative therapy", "integrative therapy", "counseling techniques"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "psychology"})
    df = df.drop_duplicates(subset=["keyword"])  
    return df

def save_psychology_keywords(output_file="psychology_keywords.parquet"):
    """Save psychology keywords to a parquet file. Append if the file already exists."""
    new_df = get_psychology_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique psychology keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique psychology keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_psychology_keywords()
