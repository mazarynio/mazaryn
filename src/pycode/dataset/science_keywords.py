import pandas as pd
import os

def get_science_keywords():
    """Return a DataFrame of science-related keywords with significantly expanded coverage."""
    keywords = [
        # Original keywords
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
        "synthetic chemistry", "superconductivity", "neuroscience and AI", "quantum cryptography", "synthetic intelligence", "space mining", "biohacking", "biomimicry", "environmental toxicology",
        
        # Physics - expanded
        "atomic physics", "nuclear physics", "optics", "acoustics", "fluid dynamics", "statistical mechanics", "condensed matter physics", "plasma physics", "string theory", "quantum field theory",
        "quantum gravity", "quantum entanglement", "quantum computing", "quantum tunneling", "quantum superposition", "quantum teleportation", "quantum information", "quantum sensors",
        "high energy physics", "standard model", "higgs boson", "supersymmetry", "superstring theory", "M-theory", "brane cosmology", "loop quantum gravity", "nuclear fusion", "nuclear fission",
        "superconductivity", "superfluidity", "bose-einstein condensate", "fermi-dirac statistics", "bose-einstein statistics", "maxwell-boltzmann statistics",
        "classical mechanics", "newtonian mechanics", "lagrangian mechanics", "hamiltonian mechanics", "wave-particle duality", "uncertainty principle", "schrodinger equation", "dirac equation",
        "special relativity", "general relativity", "lorentz transformation", "time dilation", "length contraction", "relativistic momentum", "relativistic energy", "spacetime curvature",
        "gravitational lensing", "event horizon", "hawking radiation", "cosmic inflation", "cosmic microwave background", "redshift", "blueshift", "doppler effect",
        "photoelectric effect", "double-slit experiment", "ballistics", "magnetohydrodynamics", "chaos theory", "nonlinear dynamics", "perturbation theory", "renormalization",
        
        # Chemistry - expanded
        "electrochemistry", "photochemistry", "thermochemistry", "radiochemistry", "mechanochemistry", "sonochemistry", "crystallography", "spectroscopy", "chromatography",
        "polymer chemistry", "carbohydrate chemistry", "lipid chemistry", "protein chemistry", "catalysis", "chirality", "stereochemistry", "isomerism", "aromaticity",
        "coordination chemistry", "organometallic chemistry", "bioinorganic chemistry", "green chemistry", "medicinal chemistry", "pharmaceutical chemistry", "computational chemistry",
        "quantum chemistry", "chemical kinetics", "chemical equilibrium", "chemical thermodynamics", "chemical synthesis", "retrosynthesis", "total synthesis", "asymmetric synthesis",
        "click chemistry", "combinatorial chemistry", "supramolecular chemistry", "surface chemistry", "colloid chemistry", "rheology", "metallurgy", "ceramic chemistry", "glass chemistry",
        "nuclear chemistry", "isotope chemistry", "periodic table", "chemical bonding", "covalent bond", "ionic bond", "hydrogen bond", "metallic bond", "van der waals forces",
        "chemical reaction", "redox reactions", "acid-base reactions", "precipitation reactions", "combustion reactions", "hydrolysis", "oxidation", "reduction", "pH", "buffer solutions",
        "functional groups", "alkanes", "alkenes", "alkynes", "aromatics", "alcohols", "ethers", "aldehydes", "ketones", "carboxylic acids", "esters", "amines", "amides",

        # Biology - expanded
        "molecular biology", "cell biology", "anatomy", "physiology", "histology", "embryology", "evolutionary biology", "taxonomy", "phylogenetics", "cladistics",
        "population biology", "conservation biology", "marine biology", "freshwater biology", "terrestrial biology", "extremophile biology", "astrobiology", "exobiology",
        "systems biology", "synthetic biology", "computational biology", "structural biology", "behavioral biology", "ethology", "sociobiology", "chronobiology", "cryobiology",
        "developmental biology", "regenerative biology", "aging biology", "immunology", "virology", "bacteriology", "mycology", "parasitology", "entomology", "ornithology",
        "mammalogy", "herpetology", "ichthyology", "primatology", "cytology", "histology", "pathology", "pharmacology", "toxicology", "epidemiology", "biostatistics",
        "genomics", "proteomics", "metabolomics", "transcriptomics", "epigenetics", "gene expression", "gene regulation", "gene therapy", "gene silencing", "RNA interference",
        "genetic engineering", "genetic modification", "transgenic organisms", "cloning", "stem cells", "cellular differentiation", "apoptosis", "necrosis", "autophagy",
        "mitosis", "meiosis", "fertilization", "gastrulation", "organogenesis", "morphogenesis", "homeostasis", "symbiosis", "mutualism", "commensalism", "parasitism",
        "predation", "competition", "keystone species", "invasive species", "endangered species", "biodiversity", "biogeography", "ecosystem", "food web", "trophic levels",
        "primary production", "secondary production", "decomposition", "nutrient cycling", "carbon cycle", "nitrogen cycle", "water cycle", "phosphorus cycle", "oxygen cycle",
        
        # Earth Sciences - expanded
        "hydrogeology", "petrology", "mineralogy", "stratigraphy", "sedimentology", "paleoclimatology", "paleoecology", "paleobotany", "paleozoology", "taphonomy",
        "tectonics", "plate tectonics", "continental drift", "orogeny", "isostasy", "geomorphology", "erosion", "weathering", "deposition", "diagenesis", "lithification",
        "metamorphism", "magmatism", "vulcanology", "geothermal energy", "geodynamics", "geodesy", "geomagnetism", "paleomagnetism", "geochronology", "speleology",
        "glaciology", "hydrology", "limnology", "atmospheric sciences", "atmospheric chemistry", "atmospheric physics", "aeronomy", "climate science", "paleoclimatology",
        "cloud physics", "precipitation", "atmospheric circulation", "weather forecasting", "numerical weather prediction", "climate modeling", "heat waves", "cold waves",
        "tropical cyclones", "hurricanes", "typhoons", "tornadoes", "thunderstorms", "lightning", "flooding", "drought", "desertification", "soil science", "pedology",
        "edaphology", "marine geology", "marine geophysics", "bathymetry", "ocean circulation", "thermohaline circulation", "ocean acidification", "sea level rise",
        "coastal erosion", "coastal deposition", "coral bleaching", "marine ecology", "phytoplankton", "zooplankton", "marine food webs", "fisheries science",
        
        # Astronomy & Space Sciences - expanded
        "stellar astronomy", "galactic astronomy", "extragalactic astronomy", "radio astronomy", "infrared astronomy", "optical astronomy", "ultraviolet astronomy",
        "x-ray astronomy", "gamma-ray astronomy", "gravitational wave astronomy", "neutrino astronomy", "stellar evolution", "stellar nucleosynthesis", "supernovae",
        "neutron stars", "pulsars", "magnetars", "white dwarfs", "red giants", "blue giants", "brown dwarfs", "main sequence stars", "variable stars", "binary stars",
        "star clusters", "globular clusters", "open clusters", "stellar populations", "interstellar medium", "molecular clouds", "nebulae", "planetary nebulae",
        "protoplanetary disks", "solar system", "inner planets", "outer planets", "gas giants", "ice giants", "dwarf planets", "asteroids", "comets", "kuiper belt",
        "oort cloud", "planetary rings", "planetary moons", "planetary atmospheres", "planetary interiors", "planetary magnetospheres", "planetary tectonics",
        "comparative planetology", "exoplanets", "hot jupiters", "super-earths", "mini-neptunes", "habitable zone", "exomoons", "planetary migration", "planet formation",
        "galaxy formation", "galaxy evolution", "galaxy morphology", "spiral galaxies", "elliptical galaxies", "irregular galaxies", "active galactic nuclei",
        "quasars", "blazars", "radio galaxies", "starburst galaxies", "galaxy clusters", "galaxy superclusters", "galaxy filaments", "galaxy voids", "local group",
        "milky way", "andromeda galaxy", "magellanic clouds", "galactic center", "galactic bulge", "galactic disk", "galactic halo", "galactic rotation",
        "cosmology", "big bang theory", "cosmic inflation", "recombination era", "dark ages", "reionization", "structure formation", "baryon acoustic oscillations",
        "cosmic strings", "cosmic microwave background", "cosmic neutrino background", "cosmic infrared background", "lambda-CDM model", "multiverse", "ekpyrotic universe",
        
        # Mathematics - expanded
        "set theory", "group theory", "ring theory", "field theory", "module theory", "category theory", "order theory", "graph theory", "knot theory", "measure theory",
        "differential equations", "partial differential equations", "integral equations", "functional analysis", "operator theory", "spectral theory", "variational calculus",
        "numerical analysis", "computational mathematics", "discrete mathematics", "finite mathematics", "infinite mathematics", "mathematical logic", "model theory",
        "proof theory", "recursion theory", "set theory", "algebraic geometry", "differential geometry", "riemannian geometry", "symplectic geometry", "non-euclidean geometry",
        "projective geometry", "fractal geometry", "convex geometry", "algebraic topology", "differential topology", "geometric topology", "homotopy theory", "homology theory",
        "manifold theory", "dynamical systems", "ergodic theory", "chaos theory", "game theory", "decision theory", "optimization theory", "control theory", "information theory",
        "coding theory", "cryptography", "number theory", "analytic number theory", "algebraic number theory", "combinatorial number theory", "arithmetic", "prime numbers",
        "integer sequences", "modular arithmetic", "divisibility", "congruences", "diophantine equations", "continued fractions", "combinatorics", "enumerative combinatorics",
        "algebraic combinatorics", "extremal combinatorics", "probabilistic combinatorics", "geometric combinatorics", "design theory", "coding theory", "ramsey theory",
        
        # Computer Science - expanded
        "theory of computation", "automata theory", "formal languages", "computational complexity", "algorithm design", "algorithmic efficiency", "algorithm analysis",
        "parallel algorithms", "distributed algorithms", "randomized algorithms", "approximation algorithms", "online algorithms", "streaming algorithms", "data structures",
        "arrays", "linked lists", "stacks", "queues", "trees", "graphs", "hash tables", "heaps", "tries", "bloom filters", "skip lists", "programming languages",
        "compiler design", "interpreter design", "language processing", "parsing", "lexical analysis", "syntax analysis", "semantic analysis", "code generation",
        "code optimization", "memory management", "garbage collection", "runtime systems", "operating systems", "process management", "memory management", "file systems",
        "input/output systems", "virtualization", "containerization", "distributed systems", "parallel computing", "concurrent computing", "cloud computing", "grid computing",
        "edge computing", "fog computing", "quantum computing", "database systems", "relational databases", "NoSQL databases", "graph databases", "time-series databases",
        "database design", "query optimization", "transaction processing", "data mining", "data warehousing", "big data", "data science", "data analysis", "data visualization",
        "information retrieval", "search engines", "recommendation systems", "artificial intelligence", "knowledge representation", "expert systems", "fuzzy logic",
        "evolutionary computation", "genetic algorithms", "swarm intelligence", "particle swarm optimization", "ant colony optimization", "machine learning",
        "supervised learning", "unsupervised learning", "semi-supervised learning", "reinforcement learning", "transfer learning", "self-supervised learning",
        "meta-learning", "federated learning", "active learning", "online learning", "deep learning", "neural networks", "convolutional neural networks",
        "recurrent neural networks", "transformers", "generative adversarial networks", "autoencoders", "deep reinforcement learning", "computer vision",
        "image processing", "object detection", "image segmentation", "facial recognition", "pose estimation", "optical character recognition", "natural language processing",
        "syntactic analysis", "semantic analysis", "sentiment analysis", "named entity recognition", "machine translation", "text summarization", "question answering",
        "dialogue systems", "speech recognition", "speech synthesis", "robotics", "robot kinematics", "robot dynamics", "robot control", "robot learning",
        "robot vision", "robot manipulation", "robot navigation", "human-robot interaction", "swarm robotics", "cyber-physical systems", "internet of things",
        "embedded systems", "real-time systems", "mobile computing", "ubiquitous computing", "pervasive computing", "wearable computing", "human-computer interaction",
        "user interface design", "user experience design", "augmented reality", "virtual reality", "mixed reality", "computer graphics", "rendering", "ray tracing",
        "photorealistic rendering", "non-photorealistic rendering", "animation", "motion capture", "procedural generation", "computer networks", "network protocols",
        "network architecture", "network security", "cryptography", "cryptographic protocols", "blockchain", "distributed ledger", "smart contracts", "software engineering",
        "software architecture", "software design patterns", "software testing", "software verification", "formal verification", "model checking", "software project management",
        "agile development", "devops", "continuous integration", "continuous deployment", "version control", "ethics in computing", "algorithmic bias", "privacy",
        "data protection", "cybersecurity", "network security", "application security", "information security", "cryptographic security", "operational security",
        
        # Interdisciplinary Fields - expanded
        "biophysics", "biochemistry", "physical chemistry", "chemical physics", "geophysics", "geochemistry", "biogeochemistry", "astrochemistry", "astrobiology",
        "psychophysics", "neurophysics", "medical physics", "health physics", "environmental physics", "econophysics", "sociophysics", "quantum biology",
        "systems biology", "computational biology", "mathematical biology", "theoretical biology", "evolutionary psychology", "cognitive neuroscience",
        "behavioral neuroscience", "affective neuroscience", "social neuroscience", "neuroeconomics", "neuroinformatics", "computational neuroscience",
        "biomedical engineering", "neural engineering", "genetic engineering", "protein engineering", "tissue engineering", "biomechanics", "bioelectronics",
        "bioinstrumentation", "biomaterials", "regenerative medicine", "nanomedicine", "theranostics", "pharmacogenomics", "nutrigenomics", "metabolic engineering",
        "synthetic genomics", "metagenomics", "functional genomics", "comparative genomics", "systems genomics", "ecological genomics", "conservation genomics",
        "environmental genomics", "agricultural genomics", "structural genomics", "pharmacogenetics", "ecoinformatics", "biodiversity informatics", "neuroinformatics",
        "chemoinformatics", "materials informatics", "paleoinformatics", "geoinformatics", "astroinformatics", "health informatics", "medical informatics",
        "clinical informatics", "environmental informatics", "energy informatics", "urban informatics", "social informatics", "computational social science",
        "digital humanities", "computational linguistics", "psycholinguistics", "neurolinguistics", "sociolinguistics", "evolutionary linguistics", "historical linguistics",
        "cognitive linguistics", "computational archaeology", "geoarchaeology", "bioarchaeology", "archaeometry", "paleoanthropology", "evolutionary anthropology",
        "cognitive anthropology", "medical anthropology", "environmental anthropology", "digital anthropology", "ethnobiology", "ethnobotany", "ethnopharmacology",
        "ethnomathematics", "ethnoastronomy", "ethnophysics", "environmental ethics", "bioethics", "neuroethics", "roboethics", "information ethics", "data ethics",
        
        # Emerging Scientific Fields - expanded
        "femtochemistry", "attosecond physics", "quantum biology", "quantum neuroscience", "quantum cognition", "negative temperature physics", "topological matter",
        "twistronics", "neuromorphic computing", "reservoir computing", "quantum machine learning", "molecular machines", "spintronics", "valleytronics", "excitonics",
        "photonics", "plasmonics", "metamaterials", "metasurfaces", "2D materials", "carbon nanotubes", "graphene", "fullerenes", "quantum dots", "nanocrystals",
        "nanowires", "aerogels", "hydrogels", "cryogels", "stimuli-responsive materials", "self-healing materials", "shape memory materials", "piezoelectric materials",
        "thermoelectric materials", "multiferroic materials", "topological insulators", "quasicrystals", "high-entropy alloys", "metallic glasses", "biomimetic materials",
        "tissue-engineered organs", "organoids", "brain organoids", "lab-on-a-chip", "organ-on-a-chip", "microfluidics", "nanofluidics", "biosensors", "optogenetics",
        "magnetogenetics", "sonogenetics", "chemogenetics", "genome-wide association studies", "precision medicine", "personalized medicine", "regenerative medicine",
        "immunotherapy", "CAR-T therapy", "gene therapy", "RNAi therapy", "antisense therapy", "miRNA therapy", "CRISPR-Cas9", "base editing", "prime editing",
        "epigenome editing", "synthetic genomics", "xenobiology", "protocell research", "synthetic cells", "minimal cells", "artificial life", "digital organisms",
        "computational evolution", "directed evolution", "evolutionary computation", "artificial evolution", "evo-devo", "evolutionary developmental biology",
        "evolutionary robotics", "evolutionary medicine", "paleogenomics", "ancient DNA", "de-extinction", "resurrection biology", "conservation genomics",
        "environmental DNA", "metagenomics", "microbiome science", "metabolomics", "foodomics", "nutrigenomics", "exposomics", "phenomics", "connectomics",
        "brain mapping", "whole brain emulation", "brain-computer interfaces", "consciousness science", "sleep science", "chronobiology", "circadian neuroscience",
        "neuroeconomics", "cultural neuroscience", "social neuroscience", "affective neuroscience", "contemplative neuroscience", "computational psychiatry",
        "precision psychiatry", "psychedelics research", "neuroplasticity", "neurogenesis", "cognitive enhancement", "neuroethics", "neurotechnology",
        "closed-loop neurotechnology", "neurofeedback", "transcranial stimulation", "deep brain stimulation", "neural dust", "neural lace", "neuromorphic engineering",
        "quantum communication", "quantum internet", "quantum sensing", "quantum metrology", "quantum imaging", "quantum radar", "quantum lidar", "quantum batteries",
        "molecular electronics", "single-molecule electronics", "DNA computing", "DNA data storage", "molecular robotics", "DNA origami", "protein origami",
        "computational matter", "programmable matter", "4D printing", "bioprinting", "food printing", "construction printing", "space manufacturing", "in-situ resource utilization",
        "asteroid mining", "space elevators", "solar sails", "magnetic sails", "beam-powered propulsion", "nuclear pulse propulsion", "antimatter propulsion",
        "interstellar travel", "breakthrough starshot", "generation ships", "suspended animation", "planetary terraforming", "geoengineering", "carbon sequestration",
        "direct air capture", "ocean fertilization", "solar radiation management", "climate intervention", "weather modification", "ecosystem restoration",
        "de-extinction", "rewilding", "sustainable agriculture", "vertical farming", "precision agriculture", "cellular agriculture", "lab-grown meat", "cultured meat",
        "food technology", "alternative proteins", "phytoremediation", "mycoremediation", "bioremediation", "environmental genomics", "conservation technology",
        "earth system science", "planetary boundaries", "anthropocene studies", "sustainability science", "circular economy", "industrial ecology", "ecological economics",
        
        # Applied Science & Technology - expanded
        "renewable energy", "solar energy", "photovoltaics", "concentrated solar power", "wind energy", "hydroelectric power", "geothermal energy", "tidal energy",
        "wave energy", "ocean thermal energy", "biomass energy", "biofuels", "hydrogen energy", "fuel cells", "energy storage", "batteries", "flow batteries",
        "supercapacitors", "compressed air energy storage", "pumped hydro storage", "thermal energy storage", "mechanical energy storage", "flywheel energy storage",
        "smart grid", "microgrid", "distributed generation", "demand response", "energy efficiency", "energy harvesting", "nuclear energy", "nuclear fission",
        "nuclear fusion", "tokamak", "stellarator", "inertial confinement fusion", "muon-catalyzed fusion", "cold fusion", "zero-point energy", "vacuum energy",
        "aerospace engineering", "aeronautical engineering", "astronautical engineering", "propulsion systems", "jet engines", "rocket engines", "scramjet engines",
        "ion thrusters", "hall thrusters", "magnetoplasmadynamic thrusters", "nuclear thermal rockets", "solar sails", "aerodynamics", "computational fluid dynamics",
        "fluid mechanics", "flight dynamics", "aircraft design", "spacecraft design", "satellite technology", "space stations", "space habitats", "mechanical engineering",
        "robotics", "mechatronics", "control systems", "automation", "manufacturing technology", "additive manufacturing", "3D printing", "4D printing", "subtractive manufacturing",
        "computer-aided design", "computer-aided manufacturing", "computer-integrated manufacturing", "industrial robotics", "collaborative robotics", "soft robotics",
        "biomimetic robotics", "swarm robotics", "micro-robotics", "nano-robotics", "robotic exoskeletons", "prosthetics", "bionics", "biomechatronics",
        "civil engineering", "structural engineering", "architectural engineering", "transportation engineering", "traffic engineering", "highway engineering",
        "railway engineering", "bridge engineering", "tunnel engineering", "earthquake engineering", "geotechnical engineering", "foundation engineering",
        "soil mechanics", "rock mechanics", "construction engineering", "construction management", "building information modeling", "sustainable construction",
        "green building", "resilient infrastructure", "smart cities", "urban planning", "environmental engineering", "water resources engineering",
        "wastewater engineering", "sanitary engineering", "waste management", "air pollution control", "noise pollution control", "remediation", "electrical engineering",
        "power engineering", "power electronics", "electric machines", "high voltage engineering", "electronic engineering", "microelectronics", "nanoelectronics",
        "integrated circuits", "semiconductor devices", "optoelectronics", "photonics", "quantum electronics", "telecommunications engineering", "wireless communications",
        "optical communications", "satellite communications", "network engineering", "signal processing", "digital signal processing", "image processing",
        "medical engineering", "biomedical engineering", "clinical engineering", "rehabilitation engineering", "medical imaging", "medical devices", "biosensors",
        "bioinstrumentation", "healthcare technology", "telemedicine", "e-health", "m-health", "wearable health technology", "implantable devices", "prosthetics",
        "artificial organs", "dialysis technology", "life support systems", "medical robotics", "surgical robotics", "minimally invasive surgery", "robotic surgery",
        "agricultural engineering", "agricultural machinery", "irrigation systems", "drainage systems", "precision agriculture", "smart farming", "agricultural robotics",
        "vertical farming", "controlled environment agriculture", "hydroponics", "aeroponics", "aquaponics", "food engineering", "food processing", "food preservation",
        "food packaging", "food safety", "mining engineering", "mineral processing", "metallurgical engineering", "extractive metallurgy", "mineral exploration",
        "petroleum engineering", "reservoir engineering", "drilling engineering", "production engineering", "well engineering", "offshore engineering",
        "marine engineering", "naval architecture", "ship design", "ocean engineering", "coastal engineering", "underwater technology", "submarine technology",
        "fire protection engineering", "safety engineering", "reliability engineering", "quality engineering", "systems engineering", "operations research",
        "industrial engineering", "human factors engineering", "ergonomics", "acoustic engineering", "noise control", "vibration control", "illumination engineering",
        "lighting design", "textile engineering", "polymer engineering", "ceramic engineering", "paper engineering", "packaging engineering", "nuclear engineering",
        "radiation protection", "radiological engineering", "isotope engineering", "forensic engineering", "failure analysis", "reverse engineering",
        
        # Scientific Methodologies & Approaches - expanded
        "scientific method", "empirical research", "theoretical research", "computational research", "experimental design", "observational studies", "longitudinal studies",
        "cross-sectional studies", "case studies", "field studies", "laboratory studies", "randomized controlled trials", "quasi-experimental design", "natural experiments",
        "simulation", "modeling", "mathematical modeling", "statistical modeling", "agent-based modeling", "monte carlo methods", "molecular dynamics", "finite element analysis",
        "computational fluid dynamics", "systems dynamics", "qualitative research", "quantitative research", "mixed methods research", "interdisciplinary research",
        "transdisciplinary research", "multidisciplinary research", "systems thinking", "reductionist approach", "holistic approach", "complexity science",
        "network science", "data analysis", "statistical analysis", "multivariate analysis", "factor analysis", "cluster analysis", "principal component analysis",
        "regression analysis", "time series analysis", "spatial analysis", "network analysis", "meta-analysis", "systematic review", "critical review",
        "peer review", "open science", "citizen science", "reproducibility", "replicability", "validation", "verification", "falsifiability", "research ethics",
        "scientific integrity", "research integrity", "responsible conduct of research", "scientific writing", "science communication", "scientific visualization",
        "data visualization", "infographics", "science policy", "research policy", "science diplomacy", "science advocacy", "science education", "STEM education",
        "scientific literacy", "technological literacy", "digital literacy", "data literacy", "science history", "history of technology", "philosophy of science",
        "epistemology", "ontology", "scientific realism", "scientific antirealism", "scientific instrumentalism", "scientific pragmatism", "scientific pluralism",
        "sociology of science", "science and technology studies", "science and society", "public understanding of science", "science and policy", "science and politics",
        "science and religion", "science and ethics", "ethics of science", "ethics of technology", "responsible innovation", "anticipatory governance",
        "technology assessment", "impact assessment", "risk assessment", "uncertainty analysis", "sensitivity analysis", "benchmarking", "standardization",
        "scientific instruments", "measurement", "metrology", "calibration", "precision", "accuracy", "resolution", "sensitivity", "specificity", "reliability",
        "validity", "robustness", "scalability", "interoperability", "technological readiness levels", "innovation", "invention", "diffusion of innovations",
        "disruptive technologies", "emerging technologies", "converging technologies", "technology transfer", "knowledge transfer", "commercialization",
        "intellectual property", "patents", "trademarks", "copyrights", "trade secrets", "licensing", "entrepreneurship", "startups", "spinoffs", "incubators",
        "accelerators", "research and development", "basic research", "applied research", "translational research", "development", "demonstration", "deployment",
        "scaling", "adoption", "implementation", "sustainability", "circular economy", "cradle-to-cradle design", "life cycle assessment", "environmental impact assessment",
        "social impact assessment", "health impact assessment", "technology impact assessment", "ethical impact assessment", "privacy impact assessment",
        "scientific paradigms", "paradigm shifts", "scientific revolutions", "scientific consensus", "scientific controversy", "scientific uncertainty",
        "scientific skepticism", "scientific literacy", "pseudoscience", "fringe science", "protoscience", "scientism", "anti-science", "science denial",
        "scientific misconduct", "fabrication", "falsification", "plagiarism", "research bias", "publication bias", "funding bias", "citation bias", "researcher degrees of freedom",
        "p-hacking", "HARKing", "preregistration", "registered reports", "open data", "FAIR data principles", "open access", "open source", "open hardware",
        "scientific databases", "scientific repositories", "scientific journals", "scientific conferences", "scientific societies", "scientific academies",
        "science museums", "science centers", "scientific collections", "scientific expeditions", "citizen science", "participatory research", "community-based research",
        "indigenous knowledge", "traditional ecological knowledge", "local knowledge", "tacit knowledge", "explicit knowledge", "codified knowledge"
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
    print(f"Total keywords in file: {len(combined_df)}")
    return combined_df

if __name__ == "__main__":
    save_science_keywords()