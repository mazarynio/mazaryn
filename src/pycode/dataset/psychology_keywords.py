import pandas as pd
import os

def get_psychology_keywords():
    """Return a DataFrame of psychology-related keywords with a comprehensive list."""
    keywords = [
        # Core Psychology Terms
        "psychology", "mental health", "cognitive science", "behavioral therapy", "psychoanalysis", "emotional intelligence", "self-awareness", "personality types",
        "neuroscience", "counseling", "psychotherapy", "clinical psychology", "developmental psychology", "social psychology", "positive psychology",
        
        # Therapeutic Approaches
        "mindfulness", "meditation", "stress management", "cognitive behavioral therapy", "psychodynamic therapy", "humanistic therapy", "existential therapy",
        "gestalt therapy", "acceptance and commitment therapy", "eye movement desensitization and reprocessing", "EMDR", "interpersonal therapy",
        "family therapy", "couples therapy", "group therapy", "art therapy", "play therapy", "music therapy", "dance therapy", "drama therapy",
        "sand tray therapy", "animal-assisted therapy", "equine therapy", "wilderness therapy", "bibliotherapy", "ecotherapy", "solution-focused brief therapy",
        "narrative therapy", "dialectical behavior therapy", "motivational interviewing", "integrative therapy", "holistic therapy", "coaching psychology",
        "hypnotherapy", "neuro-linguistic programming", "NLP", "somatic experiencing", "internal family systems", "IFS", "psychodrama", 
        "emotion-focused therapy", "EFT", "biofeedback", "neurofeedback", "schema therapy", "transactional analysis", "reality therapy",
        "rational emotive behavior therapy", "REBT", "logotherapy", "metacognitive therapy", "mentalizing-based therapy", "mindfulness-based cognitive therapy",
        "MBCT", "mindfulness-based stress reduction", "MBSR", "compassion-focused therapy", "sensorimotor psychotherapy", "psychosynthesis",
        
        # Mental Health Conditions
        "anxiety", "depression", "mental disorders", "phobias", "trauma", "PTSD", "OCD", "obsessive compulsive disorder", "generalized anxiety disorder", "GAD",
        "social anxiety disorder", "SAD", "panic disorder", "agoraphobia", "specific phobia", "separation anxiety", "major depressive disorder", "MDD", 
        "persistent depressive disorder", "dysthymia", "seasonal affective disorder", "bipolar disorder", "cyclothymia", "schizophrenia", 
        "schizoaffective disorder", "delusional disorder", "psychosis", "schizotypal personality disorder", "schizoid personality disorder", 
        "borderline personality disorder", "BPD", "narcissistic personality disorder", "NPD", "histrionic personality disorder", "antisocial personality disorder", 
        "avoidant personality disorder", "dependent personality disorder", "obsessive-compulsive personality disorder", "paranoid personality disorder",
        "eating disorders", "anorexia nervosa", "bulimia nervosa", "binge eating disorder", "avoidant restrictive food intake disorder", "ARFID", 
        "pica", "rumination disorder", "orthorexia", "body dysmorphic disorder", "BDD", "somatic symptom disorder", "illness anxiety disorder", 
        "conversion disorder", "factitious disorder", "munchausen syndrome", "dissociative disorders", "dissociative identity disorder", "DID", 
        "depersonalization-derealization disorder", "dissociative amnesia", "dissociative fugue", "adjustment disorders", "acute stress disorder",
        "complex PTSD", "CPTSD", "toxic stress", "complicated grief", "prolonged grief disorder", "disenfranchised grief", "substance use disorders", 
        "alcohol use disorder", "opioid use disorder", "stimulant use disorder", "cannabis use disorder", "hallucinogen use disorder", 
        "behavioral addictions", "gambling disorder", "internet gaming disorder", "compulsive sexual behavior disorder", "kleptomania", "pyromania",
        "trichotillomania", "excoriation disorder", "impulse control disorders", "intermittent explosive disorder", "conduct disorder", 
        "oppositional defiant disorder", "ODD", "attention-deficit/hyperactivity disorder", "ADHD", "autism spectrum disorder", "ASD", 
        "Asperger's syndrome", "intellectual developmental disorder", "learning disorders", "dyslexia", "dyscalculia", "dysgraphia", 
        "language disorder", "speech sound disorder", "childhood-onset fluency disorder", "stuttering", "selective mutism", "reactive attachment disorder", 
        "disinhibited social engagement disorder", "rumination", "insomnia", "hypersomnia", "narcolepsy", "sleep apnea", "parasomnias", "night terrors",
        "nightmare disorder", "restless legs syndrome", "RLS", "premenstrual dysphoric disorder", "PMDD", "gender dysphoria",
        
        # Psychological Concepts
        "attachment theory", "motivation", "learning theories", "conditioning", "reinforcement", "habit formation", "memory", "perception", "decision making", 
        "problem solving", "creativity", "intelligence", "emotional regulation", "self-esteem", "assertiveness", "communication skills", "emotional health", 
        "self-compassion", "psychopathology", "behavior modification", "mind-body connection", "neuroplasticity", "growth mindset", "behavioral economics", 
        "cognitive biases", "self-care", "resilience", "psychological well-being", "existential psychology", "behavioral activation", "anger management", 
        "psychological flexibility", "self-regulation", "clinical assessments", "neurodevelopmental disorders", "psychosomatic disorders", "human behavior",
        "social cognition", "cognitive neuroscience", "interpersonal relationships", "mental health advocacy", "psychological testing", "psychometric assessments",
        "therapy techniques", "counseling techniques", "mindset", "consciousness", "unconscious", "subconscious", "preconscious", "classical conditioning", 
        "operant conditioning", "observational learning", "social learning theory", "constructivism", "cognitive development", "object permanence", 
        "theory of mind", "moral development", "identity formation", "ego development", "self-concept", "self-efficacy", "locus of control", 
        "learned helplessness", "learned optimism", "attitude formation", "attitude change", "cognitive dissonance", "attribution theory", 
        "fundamental attribution error", "self-serving bias", "confirmation bias", "availability heuristic", "representativeness heuristic", 
        "anchoring bias", "framing effect", "sunk cost fallacy", "groupthink", "social facilitation", "social loafing", "deindividuation", 
        "bystander effect", "conformity", "compliance", "obedience", "persuasion", "propaganda", "stereotype", "prejudice", "discrimination",
        "implicit bias", "explicit bias", "microaggressions", "collectivism", "individualism", "authoritarianism", "altruism", "prosocial behavior", 
        "antisocial behavior", "aggression", "catharsis", "frustration-aggression hypothesis", "empathy", "sympathy", "compassion", 
        "emotional contagion", "emotional intelligence", "alexithymia", "affect", "mood", "temperament", "personality", "big five personality traits", 
        "openness", "conscientiousness", "extraversion", "agreeableness", "neuroticism", "Myers-Briggs Type Indicator", "MBTI",
        
        # Memory & Cognition
        "working memory", "short-term memory", "long-term memory", "episodic memory", "semantic memory", "procedural memory", "declarative memory", 
        "implicit memory", "explicit memory", "autobiographical memory", "flashbulb memory", "prospective memory", "metamemory", "encoding", "storage", 
        "retrieval", "mnemonics", "method of loci", "spaced repetition", "elaborative rehearsal", "maintenance rehearsal", "interference theory", 
        "proactive interference", "retroactive interference", "serial position effect", "primacy effect", "recency effect", "forgetting curve", 
        "tip-of-the-tongue phenomenon", "false memory", "cryptomnesia", "source confusion", "misinformation effect", "eyewitness memory", 
        "memory consolidation", "memory reconsolidation", "state-dependent memory", "context-dependent memory", "mood-dependent memory",
        "attention", "selective attention", "divided attention", "sustained attention", "attentional blink", "change blindness", "inattentional blindness", 
        "executive function", "inhibitory control", "cognitive flexibility", "working memory", "planning", "problem-solving", "decision-making", 
        "reasoning", "logical reasoning", "deductive reasoning", "inductive reasoning", "abductive reasoning", "critical thinking", 
        "counterfactual thinking", "divergent thinking", "convergent thinking", "lateral thinking", "fluid intelligence", "crystallized intelligence", 
        "multiple intelligences", "emotional intelligence", "social intelligence", "practical intelligence", "IQ", "intelligence quotient", 
        
        # Neuroscience & Biological Psychology
        "neurotransmitters", "dopamine", "serotonin", "norepinephrine", "GABA", "glutamate", "acetylcholine", "endorphins", "oxytocin", "vasopressin", 
        "cortisol", "adrenaline", "melatonin", "neurons", "synapses", "axons", "dendrites", "action potential", "neurogenesis", "synaptic pruning", 
        "brain structures", "cerebral cortex", "prefrontal cortex", "frontal lobe", "parietal lobe", "temporal lobe", "occipital lobe", "hippocampus", 
        "amygdala", "thalamus", "hypothalamus", "basal ganglia", "cerebellum", "brain stem", "corpus callosum", "limbic system", "reticular activating system", 
        "default mode network", "vagus nerve", "autonomic nervous system", "sympathetic nervous system", "parasympathetic nervous system", 
        "fight-or-flight response", "rest-and-digest", "polyvagal theory", "endocrine system", "HPA axis", "nervous system", "central nervous system", 
        "peripheral nervous system", "somatosensory system", "proprioception", "interoception", "neuroimaging", "fMRI", "EEG", "MEG", "PET scan", 
        "brain-computer interface", "neurofeedback", "neuropsychology", "cognitive neuroscience", "affective neuroscience", "social neuroscience", 
        "evolutionary psychology", "behavioral genetics", "epigenetics", "heritability", "nature versus nurture", "biopsychosocial model", 
        "psychoneuroimmunology", "neuroplasticity", "critical periods", "sensitization", "habituation", "long-term potentiation",
        
        # Research & Methodology
        "psychological research", "case studies", "behavioral science", "quantitative research", "qualitative research", "mixed methods", 
        "cross-sectional study", "longitudinal study", "experimental design", "quasi-experimental design", "correlational study", "naturalistic observation", 
        "participant observation", "survey research", "interview research", "focus groups", "single-subject design", "between-subjects design", 
        "within-subjects design", "repeated measures", "independent variable", "dependent variable", "confounding variable", "operational definition", 
        "random sampling", "convenience sampling", "snowball sampling", "stratified sampling", "representative sample", "sample size", "statistical power", 
        "effect size", "statistical significance", "p-value", "null hypothesis", "alternative hypothesis", "type I error", "type II error", 
        "reliability", "validity", "internal validity", "external validity", "construct validity", "ecological validity", "inter-rater reliability", 
        "test-retest reliability", "split-half reliability", "Cronbach's alpha", "factor analysis", "meta-analysis", "systematic review", 
        "replication crisis", "publication bias", "researcher bias", "demand characteristics", "social desirability bias", "Hawthorne effect", 
        "placebo effect", "double-blind study", "informed consent", "debriefing", "ethics in psychology research", "institutional review board", "IRB",
        
        # Developmental Psychology
        "prenatal development", "infant development", "childhood development", "adolescent development", "early adulthood", "middle adulthood", 
        "late adulthood", "aging", "developmental milestones", "developmental stages", "developmental domains", "cognitive development", 
        "social development", "emotional development", "physical development", "motor development", "language development", "moral development", 
        "psychosocial development", "attachment", "secure attachment", "insecure attachment", "anxious attachment", "avoidant attachment", 
        "disorganized attachment", "internal working models", "separation anxiety", "stranger anxiety", "social referencing", "temperament", 
        "goodness of fit", "parenting styles", "authoritative parenting", "authoritarian parenting", "permissive parenting", "uninvolved parenting", 
        "helicopter parenting", "free-range parenting", "attachment parenting", "gentle parenting", "positive discipline", "scaffolding", 
        "zone of proximal development", "socialization", "gender development", "gender identity", "gender roles", "pubertal development", 
        "identity versus role confusion", "emerging adulthood", "intimacy versus isolation", "generativity versus stagnation", "integrity versus despair", 
        "midlife crisis", "empty nest syndrome", "retirement adjustment", "successful aging", "cognitive aging", "wisdom", "reminiscence", 
        "life review", "gerontology", "ageism", "developmental psychopathology", "developmental disabilities", "developmental delays", 
        "gifted and talented", "precocious development", "resilience in development", "risk factors", "protective factors", 
        
        # Clinical Practice
        "clinical assessment", "intake interview", "mental status examination", "clinical observation", "psychological testing", "psychodiagnostic assessment", 
        "projective tests", "personality inventories", "intelligence testing", "neuropsychological assessment", "behavioral assessment", 
        "functional analysis", "diagnosis", "differential diagnosis", "comorbidity", "case conceptualization", "treatment planning", 
        "evidence-based practice", "empirically supported treatments", "practice-based evidence", "common factors", "therapeutic alliance", 
        "therapeutic relationship", "therapist variables", "client variables", "cultural competence", "multicultural counseling", "client-centered therapy", 
        "therapist congruence", "unconditional positive regard", "empathic understanding", "therapeutic presence", "therapeutic boundaries", 
        "therapeutic frame", "therapeutic contract", "informed consent", "confidentiality", "privilege", "mandated reporting", "duty to warn", 
        "crisis intervention", "suicide risk assessment", "safety planning", "harm reduction", "relapse prevention", "recovery model", 
        "trauma-informed care", "culturally responsive therapy", "intersectionality", "feminist therapy", "LGBTQ+ affirmative therapy", 
        "neurodiversity-affirming approaches", "teletherapy", "online counseling", "mobile health interventions", "stepped care", 
        "integrated behavioral health", "collaborative care", "interdisciplinary teams", "continuity of care", "treatment outcome", 
        "symptom reduction", "functional improvement", "quality of life", "well-being", "client satisfaction", "therapeutic progress monitoring",
        
        # Specialized Areas
        "workplace psychology", "industrial-organizational psychology", "IO psychology", "organizational behavior", "organizational development", 
        "leadership psychology", "team dynamics", "workplace motivation", "job satisfaction", "employee engagement", "workplace stress", 
        "burnout", "workplace wellness", "occupational health psychology", "job analysis", "personnel selection", "performance appraisal", 
        "training and development", "executive coaching", "career development", "vocational psychology", "career counseling", "retirement planning", 
        "school psychology", "educational psychology", "learning disabilities", "special education", "gifted education", "academic achievement", 
        "academic motivation", "test anxiety", "classroom management", "teacher-student relationship", "bullying prevention", "school climate", 
        "school counseling", "college counseling", "student development", "forensic psychology", "criminal psychology", "legal psychology", 
        "eyewitness testimony", "false confessions", "jury selection", "courtroom psychology", "criminal profiling", "risk assessment", 
        "violence risk", "psychopathy", "antisocial personality", "juvenile delinquency", "correctional psychology", "offender rehabilitation", 
        "restorative justice", "victim psychology", "victimology", "health psychology", "behavioral medicine", "psychosomatic medicine", 
        "psychoneuroimmunology", "placebo effect", "nocebo effect", "illness behavior", "adherence", "compliance", "patient-provider communication", 
        "end-of-life care", "grief counseling", "bereavement", "death anxiety", "palliative psychology", "sport psychology", "performance psychology", 
        "peak performance", "flow state", "mental toughness", "visualization", "sports confidence", "pre-performance routines", "rehabilitation psychology", 
        "community psychology", "prevention science", "social justice", "advocacy", "empowerment", "community mental health", "disaster psychology", 
        "crisis psychology", "media psychology", "consumer psychology", "advertising psychology", "human factors psychology", "ergonomics", 
        "human-computer interaction", "user experience", "traffic psychology", "environmental psychology", "conservation psychology", 
        "ecopsychology", "peace psychology", "political psychology", "military psychology", "combat stress", "veteran psychology", 
        "cross-cultural psychology", "cultural psychology", "indigenous psychology", "ethnopsychology", "religious psychology", "spiritual psychology", 
        "psychology of religion", "transpersonal psychology", "consciousness studies", "parapsychology", "cyberpsychology", "internet psychology", 
        "social media psychology", "video game psychology", "online behavior",
        
        # Positive Psychology & Well-being
        "positive psychology", "well-being", "subjective well-being", "psychological well-being", "happiness", "life satisfaction", "positive emotions", 
        "negative emotions", "emotion regulation", "hedonia", "eudaimonia", "flow", "engagement", "meaning", "purpose", "accomplishment", 
        "positive relationships", "character strengths", "virtues", "wisdom", "courage", "humanity", "justice", "temperance", "transcendence", 
        "gratitude", "forgiveness", "hope", "optimism", "resilience", "grit", "perseverance", "self-determination theory", "autonomy", 
        "competence", "relatedness", "intrinsic motivation", "extrinsic motivation", "posttraumatic growth", "benefit finding", "meaning-making", 
        "broaden-and-build theory", "savoring", "mindfulness", "meditation", "loving-kindness meditation", "compassion meditation", 
        "self-compassion", "self-care", "work-life balance", "psychological capital", "flourishing", "languishing", "second wave positive psychology", 
        "positive psychotherapy", "well-being therapy", "quality of life", "life coaching", "positive education", "positive organizations", 
        "appreciative inquiry", "strengths-based approaches"
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