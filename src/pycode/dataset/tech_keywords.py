import pandas as pd
import os

def get_tech_keywords():
    """Return a DataFrame of technology-related keywords with extensive coverage."""
    keywords = [
        # Core AI & ML
        "artificial intelligence", "machine learning", "deep learning", "neural networks", "data science", "big data", 
        "predictive analytics", "statistical learning", "supervised learning", "unsupervised learning", "semi-supervised learning", 
        "reinforcement learning", "transfer learning", "federated learning", "active learning", "ensemble learning", 
        "generative AI", "discriminative models", "feature engineering", "dimensionality reduction", "hyperparameter tuning", 
        "model optimization", "model deployment", "explainable AI", "interpretable ML", "AI governance", "AI regulations",
        "AI compliance", "AI auditing", "model fairness", "model bias", "model drift", "concept drift", "data drift",
        "model monitoring", "model versioning", "MLOps", "AI pipelines", "AI workflow", "AI benchmarks", "AI performance metrics",
        
        # Neural Network Types
        "convolutional neural networks", "recurrent neural networks", "LSTM networks", "GRU networks", "transformer models", 
        "attention mechanisms", "self-attention", "graph neural networks", "generative adversarial networks", "variational autoencoders", 
        "autoregressive models", "diffusion models", "normalization techniques", "activation functions", "backpropagation", 
        "gradient descent", "stochastic gradient descent", "adam optimizer", "batch normalization", "layer normalization",
        "residual networks", "dense networks", "siamese networks", "encoder-decoder architectures", "U-Net", "BERT", "GPT", 
        "LLaMA", "Gemini", "Claude", "language models", "vision models", "multimodal models", "foundation models",
        "quantized models", "pruned models", "distilled models", "knowledge distillation", "neural architecture search",
        
        # NLP
        "natural language processing", "natural language understanding", "natural language generation", "sentiment analysis", 
        "text classification", "named entity recognition", "part-of-speech tagging", "dependency parsing", "constituency parsing", 
        "tokenization", "lemmatization", "stemming", "word embeddings", "contextual embeddings", "semantic analysis", 
        "syntactic analysis", "discourse analysis", "pragmatic analysis", "question answering", "machine translation", 
        "text summarization", "text generation", "chatbots", "conversational AI", "dialogue systems", "intent recognition", 
        "entity extraction", "relation extraction", "coreference resolution", "topic modeling", "document clustering",
        "language identification", "text preprocessing", "text normalization", "speech recognition", "speech synthesis",
        "text-to-speech", "speech-to-text", "voice recognition", "voice biometrics", "language modeling",
        
        # Computer Vision
        "computer vision", "image processing", "object detection", "image classification", "semantic segmentation", 
        "instance segmentation", "panoptic segmentation", "image generation", "style transfer", "super resolution", 
        "image restoration", "image inpainting", "image colorization", "image captioning", "visual question answering", 
        "pose estimation", "facial recognition", "emotion recognition", "gesture recognition", "action recognition", 
        "video understanding", "optical flow", "depth estimation", "stereo vision", "3D reconstruction", "SLAM", 
        "visual odometry", "visual SLAM", "visual tracking", "object tracking", "visual search", "content-based image retrieval",
        "image retrieval", "image registration", "image alignment", "image stitching", "panorama creation", "computational photography",
        "augmented reality", "virtual reality", "mixed reality", "extended reality", 
        
        # Big Data & Data Engineering
        "big data", "data engineering", "data pipelines", "ETL processes", "data integration", "data migration", 
        "data warehousing", "data marts", "data lakes", "data mesh", "data fabric", "data virtualization", "data catalogs", 
        "data governance", "data quality", "data lineage", "data observability", "metadata management", "master data management", 
        "reference data management", "data modeling", "dimensional modeling", "snowflake schema", "star schema", "data normalization",
        "denormalization", "data partitioning", "data sharding", "distributed computing", "parallel processing", "batch processing", 
        "stream processing", "real-time analytics", "data streaming", "event streaming", "change data capture", "lambda architecture", 
        "kappa architecture", "data orchestration", "workflow orchestration", "data transformation", "data cleaning",
        
        # Cloud Computing & Infrastructure
        "cloud computing", "public cloud", "private cloud", "hybrid cloud", "multi-cloud", "cloud migration", "cloud native", 
        "cloud security", "cloud governance", "cloud cost optimization", "cloud monitoring", "cloud observability", 
        "infrastructure as code", "configuration management", "provisioning", "orchestration", "container orchestration", 
        "serverless computing", "Function as a Service", "microservices", "service mesh", "API gateway", "edge computing", 
        "fog computing", "bare metal", "virtualization", "hypervisors", "virtual machines", "containers", "Kubernetes", 
        "Docker", "Podman", "Helm", "Terraform", "Ansible", "Puppet", "Chef", "Packer", "CI/CD pipelines", "continuous integration", 
        "continuous delivery", "continuous deployment", "GitOps", "DevOps", "DevSecOps", "AIOps", "NoOps", "infrastructure monitoring",
        
        # Databases & Storage
        "database management", "relational databases", "SQL", "NoSQL", "document databases", "key-value stores", 
        "column-family stores", "graph databases", "time-series databases", "vector databases", "NewSQL", "in-memory databases", 
        "distributed databases", "database sharding", "database replication", "database partitioning", "data replication", 
        "data consistency", "ACID properties", "BASE properties", "eventual consistency", "strong consistency", "CAP theorem", 
        "PACELC theorem", "database indexing", "database optimization", "query optimization", "stored procedures", "triggers", 
        "views", "materialized views", "database migrations", "change management", "database versioning", "database backup", 
        "database recovery", "high availability", "disaster recovery", "storage solutions", "block storage", "file storage", 
        "object storage", "distributed storage", "storage area networks", "network attached storage", "content delivery networks",
        
        # Cybersecurity
        "cybersecurity", "information security", "network security", "application security", "cloud security", "IoT security", 
        "mobile security", "endpoint security", "identity and access management", "authentication", "authorization", "accounting", 
        "multi-factor authentication", "single sign-on", "privileged access management", "zero trust architecture", "perimeter security", 
        "defense in depth", "security by design", "threat modeling", "risk assessment", "vulnerability management", "penetration testing", 
        "red teaming", "blue teaming", "purple teaming", "security operations center", "security information and event management", 
        "security orchestration, automation and response", "intrusion detection", "intrusion prevention", "firewalls", 
        "web application firewalls", "API security", "cryptography", "encryption", "decryption", "digital signatures", "PKI", 
        "hashing", "security compliance", "security standards", "security frameworks", "security policies", "incident response", 
        "threat intelligence", "threat hunting", "malware analysis", "reverse engineering", "forensics", "data privacy", 
        "privacy engineering", "privacy by design", "data protection", "data loss prevention",
        
        # Blockchain & Web3
        "blockchain", "distributed ledger technology", "smart contracts", "cryptocurrencies", "digital assets", "tokenization", 
        "non-fungible tokens", "decentralized applications", "decentralized finance", "decentralized autonomous organizations", 
        "consensus mechanisms", "proof of work", "proof of stake", "delegated proof of stake", "byzantine fault tolerance", 
        "public blockchain", "private blockchain", "consortium blockchain", "permissioned blockchain", "permissionless blockchain", 
        "blockchain scalability", "sharding", "sidechains", "state channels", "layer 2 solutions", "blockchain interoperability", 
        "cross-chain communication", "blockchain oracles", "web3", "metaverse", "decentralized identity", "self-sovereign identity", 
        "zero-knowledge proofs", "secure multi-party computation", "tokenomics", "blockchain governance", "crypto wallets", 
        "hardware wallets", "cold storage", "hot storage", "blockchain analytics", "blockchain forensics", "mining", "staking", 
        "yield farming", "liquidity provision", "algorithmic stablecoins", "central bank digital currencies",
        
        # IoT & Edge Computing
        "internet of things", "IoT devices", "IoT platforms", "IoT protocols", "IoT gateways", "IoT sensors", "IoT actuators", 
        "sensor networks", "mesh networks", "edge computing", "edge devices", "edge analytics", "edge AI", "fog computing", 
        "embedded systems", "microcontrollers", "single-board computers", "cyber-physical systems", "digital twins", "industrial IoT", 
        "smart cities", "smart buildings", "smart homes", "smart grids", "smart agriculture", "smart healthcare", "smart retail", 
        "smart manufacturing", "smart logistics", "connected vehicles", "autonomous vehicles", "vehicle-to-everything", 
        "low-power wide-area networks", "Bluetooth Low Energy", "Zigbee", "Z-Wave", "LoRaWAN", "NB-IoT", "MQTT", "CoAP", 
        "IoT security", "IoT privacy", "IoT data management", "IoT analytics", "predictive maintenance", "condition monitoring", 
        "remote monitoring", "remote diagnostics", "telemetry", "wearable technology", "implantable technology", "biomedical devices",
        
        # Quantum Computing & Advanced Technologies
        "quantum computing", "quantum algorithms", "quantum cryptography", "quantum communication", "quantum networking", 
        "quantum internet", "quantum machine learning", "quantum annealing", "quantum supremacy", "quantum advantage", 
        "quantum error correction", "quantum gates", "quantum circuits", "qubits", "superposition", "entanglement", "quantum sensing", 
        "quantum metrology", "quantum simulation", "post-quantum cryptography", "quantum-resistant algorithms", "quantum key distribution", 
        "quantum random number generation", "photonics", "integrated photonics", "silicon photonics", "neuromorphic computing", 
        "neuromorphic hardware", "spiking neural networks", "brain-computer interfaces", "brain-machine interfaces", 
        "neuroelectronic interfaces", "molecular computing", "DNA computing", "cellular computing", "biological computing", 
        "bioinformatics", "computational biology", "systems biology", "synthetic biology", "computational genomics", "proteomics", 
        "metabolomics", "transcriptomics", "6G technology", "terahertz communication", "holographic communication", 
        "molecular communication", "neural interfaces", "ambient computing", "ubiquitous computing", "haptic technology",
        
        # Software Development & Engineering
        "software engineering", "software architecture", "software design patterns", "monolithic architecture", 
        "microservices architecture", "serverless architecture", "event-driven architecture", "domain-driven design", 
        "test-driven development", "behavior-driven development", "agile methodology", "scrum methodology", "kanban methodology", 
        "waterfall methodology", "extreme programming", "pair programming", "mob programming", "code review", "continuous integration", 
        "continuous delivery", "continuous deployment", "version control", "git", "gitflow", "trunk-based development", 
        "feature flagging", "A/B testing", "canary releases", "blue-green deployment", "rolling deployment", "API development", 
        "API design", "API documentation", "API testing", "API versioning", "API management", "API gateway", "API security", 
        "REST APIs", "GraphQL", "gRPC", "WebSockets", "webhooks", "event streaming", "message queues", "message brokers", 
        "service discovery", "service registry", "service mesh", "sidecar pattern", "circuit breaker pattern", "fallback pattern", 
        "bulkhead pattern", "retry pattern", "rate limiting", "throttling", "load balancing", "caching strategies", "content delivery",
        
        # Frontend & UI/UX
        "frontend development", "web development", "responsive design", "adaptive design", "progressive web apps", "single-page applications", 
        "multi-page applications", "HTML", "CSS", "JavaScript", "TypeScript", "web components", "shadow DOM", "virtual DOM", 
        "frontend frameworks", "React", "Angular", "Vue.js", "Svelte", "Next.js", "Nuxt.js", "state management", "Redux", "Vuex", 
        "MobX", "CSS frameworks", "Bootstrap", "Tailwind CSS", "Material Design", "web performance", "web accessibility", 
        "internationalization", "localization", "static site generation", "server-side rendering", "client-side rendering", 
        "JAMstack", "UI design", "UX design", "user interface", "user experience", "interaction design", "visual design", 
        "information architecture", "user research", "usability testing", "A/B testing", "heuristic evaluation", "wireframing", 
        "prototyping", "design systems", "design tokens", "atomic design", "mobile-first design", "responsive images", 
        "progressive enhancement", "graceful degradation", "web animations", "micro-interactions", "web fonts", "icon systems", 
        "design thinking", "user-centered design", "human-computer interaction",
        
        # Backend & Performance
        "backend development", "server-side programming", "middleware", "web servers", "application servers", "reverse proxies", 
        "load balancers", "content delivery networks", "backend frameworks", "Node.js", "Express.js", "Django", "Flask", 
        "Spring Boot", "Ruby on Rails", "Laravel", "ASP.NET Core", "FastAPI", "server-side rendering", "static site generation", 
        "content management systems", "headless CMS", "API-first CMS", "ORM", "ODM", "query builders", "database migration", 
        "connection pooling", "caching layers", "in-memory caching", "distributed caching", "cache invalidation", "cache eviction", 
        "cache warming", "message queues", "message brokers", "RabbitMQ", "Kafka", "Redis", "NATS", "ZeroMQ", "batch processing", 
        "stream processing", "task scheduling", "cron jobs", "webhooks", "websockets", "server-sent events", "long polling", 
        "short polling", "server monitoring", "application monitoring", "logging", "tracing", "metrics collection", "distributed tracing", 
        "observability", "performance optimization", "bottleneck identification", "performance profiling", "code profiling", 
        "memory profiling", "CPU profiling", "network profiling", "database profiling", "query optimization", "indexing strategies", 
        "denormalization strategies", "vertical scaling", "horizontal scaling", "auto-scaling", "load testing", "stress testing", 
        "endurance testing", "spike testing", "chaos engineering", "resilience engineering", "fault tolerance", "high availability", 
        "data replication", "data sharding", "data partitioning", "database clustering", "read replicas", "write replicas", 
        "leader-follower replication", "multi-leader replication", "leaderless replication",
        
        # Mobile & Cross-platform
        "mobile development", "iOS development", "Android development", "Swift", "Objective-C", "Kotlin", "Java for Android", 
        "native mobile development", "cross-platform development", "hybrid mobile development", "progressive web apps", 
        "React Native", "Flutter", "Xamarin", "Ionic", "Capacitor", "Cordova", "NativeScript", "mobile UI frameworks", 
        "mobile design patterns", "responsive mobile design", "adaptive mobile design", "mobile navigation patterns", 
        "gesture-based interfaces", "touch interfaces", "mobile performance optimization", "mobile offline capabilities", 
        "mobile security", "mobile authentication", "biometric authentication", "mobile device management", "mobile app permissions", 
        "mobile app privacy", "app store optimization", "app store guidelines", "app monetization", "in-app purchases", 
        "subscription models", "freemium models", "ad-based models", "mobile analytics", "mobile user engagement", "push notifications", 
        "deep linking", "app indexing", "mobile SEO", "mobile accessibility", "localization and internationalization", 
        "app versioning", "over-the-air updates", "hot code push", "app crashes handling", "crash reporting", "mobile app testing", 
        "mobile device testing", "mobile emulators", "mobile simulators", "real device testing", "mobile test automation", 
        "mobile CI/CD", "mobile app monitoring", "mobile app performance monitoring", "mobile app analytics",
        
        # Game Development & Graphics
        "game development", "game engines", "game design", "game mechanics", "game physics", "game AI", "procedural generation", 
        "level design", "3D modeling", "3D animation", "rigging", "skinning", "motion capture", "game asset creation", 
        "game audio", "sound design", "music composition", "voice acting", "game UI/UX", "game HUD design", "game rendering", 
        "graphics programming", "shader programming", "real-time rendering", "ray tracing", "global illumination", "pathtracing", 
        "voxel rendering", "texturing", "material creation", "PBR materials", "sprite animation", "particle systems", 
        "volumetric effects", "post-processing effects", "game optimization", "performance profiling", "LOD systems", 
        "occlusion culling", "frustum culling", "spatial partitioning", "game networking", "multiplayer architecture", 
        "client-server architecture", "peer-to-peer architecture", "lag compensation", "netcode", "input prediction", 
        "entity interpolation", "deterministic lockstep", "game state synchronization", "game session management", 
        "matchmaking systems", "leaderboards", "achievements", "in-game economies", "virtual currencies", "game analytics", 
        "player retention", "player acquisition", "game monetization", "free-to-play", "premium", "subscription", 
        "battle pass", "loot boxes", "game localization", "culturalization", "game accessibility", "game QA", "playtesting", 
        "game balancing", "game progression systems", "game narrative design", "interactive storytelling", "branching narratives",
        
        # Human-Computer Interaction & Cognitive Computing
        "human-computer interaction", "user interfaces", "natural user interfaces", "voice user interfaces", "conversational interfaces", 
        "gestural interfaces", "haptic interfaces", "brain-computer interfaces", "augmented reality interfaces", "virtual reality interfaces", 
        "mixed reality interfaces", "ambient interfaces", "tangible interfaces", "wearable interfaces", "affective computing", 
        "emotion recognition", "sentiment analysis", "attention tracking", "gaze tracking", "facial expression analysis", 
        "gesture recognition", "posture analysis", "behavioral analysis", "cognitive load measurement", "user engagement measurement", 
        "physiological computing", "biometric interfaces", "assistive technologies", "accessibility technologies", "universal design", 
        "inclusive design", "cognitive ergonomics", "physical ergonomics", "user research methodologies", "usability testing", 
        "user experience evaluation", "human factors", "human-centered design", "participatory design", "co-design", 
        "empathic design", "contextual inquiry", "ethnographic research", "persona development", "scenario development", 
        "journey mapping", "experience mapping", "service design", "interaction design patterns", "cognitive psychology", 
        "perceptual psychology", "social psychology", "behavioral economics", "decision science", "persuasive technology", 
        "behavior change design", "gamification", "serious games", "edutainment", "simulation-based learning",
        
        # Other Emerging Technologies & Applications
        "autonomous systems", "self-driving vehicles", "autonomous drones", "autonomous robots", "swarm robotics", "robot operating system", 
        "computational creativity", "generative design", "procedural generation", "AI in design", "AI in architecture", "AI in engineering", 
        "AI in manufacturing", "AI in agriculture", "AI in energy", "AI in environment", "AI in sustainability", "AI in healthcare", 
        "AI in medicine", "AI in drug discovery", "AI in medical imaging", "AI in diagnostics", "AI in personalized medicine", 
        "AI in genomics", "AI in proteomics", "AI in clinical trials", "AI in patient care", "AI in healthcare operations", 
        "AI in finance", "algorithmic trading", "high-frequency trading", "robo-advisors", "fraud detection", "risk management", 
        "credit scoring", "insurance underwriting", "claims processing", "regulatory compliance", "financial forecasting", 
        "market analysis", "sentiment analysis in finance", "ESG investing", "sustainable finance", "digital twins", 
        "simulation modeling", "agent-based modeling", "system dynamics", "discrete event simulation", "monte carlo simulation", 
        "physics-based simulation", "computational fluid dynamics", "finite element analysis", "augmented analytics", 
        "automated machine learning", "automated data science", "citizen data science", "data storytelling", "data journalism", 
        "visual analytics", "immersive analytics", "spatial computing", "geospatial analytics", "location intelligence", 
        "geofencing", "indoor positioning", "spatial databases", "spatial algorithms", "3D printing", "additive manufacturing", 
        "bioprinting", "4D printing", "distributed manufacturing", "on-demand manufacturing", "mass customization", 
        "sustainable technology", "green computing", "energy-efficient computing", "carbon-aware computing", "circular economy", 
        "renewable energy", "smart grid", "energy storage", "carbon capture", "climate tech", "clean tech", "precision agriculture", 
        "vertical farming", "hydroponics", "aeroponics", "agricultural robotics", "crop monitoring", "livestock monitoring", 
        "soil analysis", "water management", "food technology", "alternative proteins", "cultured meat", "edible insects", 
        "food preservation", "food safety", "food traceability", "supply chain visibility", "supply chain optimization", 
        "logistics optimization", "last-mile delivery", "autonomous delivery", "drone delivery", "warehouse automation", 
        "inventory management", "demand forecasting", "predictive maintenance", "digital supply networks", "circular supply chains",
        "space technology", "satellite systems", "earth observation", "remote sensing", "space communication", "space robotics", 
        "space mining", "space manufacturing", "space tourism", "interplanetary internet", "nanotechnology", "nanomaterials", 
        "nanoelectronics", "nanophotonics", "nanomedicine", "nanorobotics", "molecular nanotechnology", "advanced materials", 
        "metamaterials", "smart materials", "self-healing materials", "programmable materials"
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