import pandas as pd
import os

def get_fashion_keywords():
    """Return a DataFrame of comprehensive fashion-related keywords."""
    keywords = [
        # Core Fashion Terms
        "fashion", "style", "clothing", "accessories", "footwear", "hairstyles", "makeup", "skincare", "runway", "fashion week",
        "designer brands", "street style", "vintage fashion", "high fashion", "fast fashion", "ethical fashion", "sustainable fashion", 
        "luxury brands", "casual wear", "formal wear", "activewear", "sportswear", "swimwear", "outerwear", "seasonal fashion", 
        "trendy", "classic style", "boho chic", "minimalist style", "urban fashion", "fashion accessories", "handbags", "jewelry", 
        "sunglasses", "watches", "fashion influencers", "style bloggers", "fashion photography", "editorial shoots", "lookbooks",
        
        # Fashion Publications & Media
        "fashion magazines", "Vogue", "Elle", "Harper's Bazaar", "GQ", "InStyle", "W Magazine", "Cosmopolitan", "Marie Claire", 
        "Vanity Fair", "V Magazine", "The Gentlemen's Quarterly", "Esquire", "Numero", "Dazed", "i-D Magazine", "Paper Magazine", 
        "Teen Vogue", "Glamour", "fashion podcasts", "style vlogs", "fashion livestreams", "fashion documentaries", "fashion journalism",
        
        # Fashion Industry
        "fashion shows", "catwalk", "couture", "ready-to-wear", "bespoke tailoring", "streetwear", "athleisure", "pret-a-porter", 
        "haute couture", "fashion merchandising", "fashion buying", "fashion marketing", "fashion retail", "fashion distribution", 
        "fashion production", "supply chain", "fashion manufacturing", "clothing factories", "garment industry", "textile industry",
        "fashion wholesale", "fashion showrooms", "fashion trade shows", "fashion weeks", "fashion capitals", "fashion districts",
        
        # Sustainability & Ethics
        "sustainable fabrics", "organic cotton", "recycled fabrics", "fashion sustainability", "fashion revolution", "fast fashion impact", 
        "upcycled fashion", "eco-friendly brands", "ethical sourcing", "fair trade fashion", "zero-waste fashion", "circular fashion", 
        "biodegradable fabrics", "vegan fashion", "cruelty-free fashion", "slow fashion", "fashion transparency", "garment worker rights", 
        "fashion activism", "sustainable luxury", "green fashion", "eco-conscious", "clothing lifecycle", "textile recycling", 
        "fashion footprint", "water conservation", "fashion carbon offset", "fashion pollution", "microplastics", "fashion waste",
        
        # Design & Creation
        "fashion design", "couture collections", "fashion houses", "fashion designers", "runway shows", "fashion stylists", 
        "custom clothing", "tailored suits", "seamstress", "fashion trends", "fashion forecasting", "seasonal trends", "color trends", 
        "fabric trends", "pattern making", "draping", "fashion sketching", "fashion illustrations", "fashion prototyping", "fashion CAD", 
        "fashion technology", "3D printing fashion", "textile design", "fabric manipulation", "fashion embellishment", "embroidery", 
        "beading", "fashion appliqué", "fashion silhouettes", "fashion proportions", "fashion fit", "fashion styling",
        
        # Digital Fashion & Technology
        "digital fashion", "virtual fashion", "fashion tech", "wearable technology", "smart textiles", "fashion apps", "virtual try-on", 
        "augmented reality fashion", "3D fashion design", "fashion NFTs", "meta fashion", "fashion in metaverse", "digital clothing", 
        "fashion gaming", "fashion avatars", "fashion e-commerce", "fashion algorithms", "fashion recommendation systems", 
        "fashion image recognition", "fashion virtual showrooms", "fashion AR filters", "fashion QR codes", "RFID fashion", 
        "blockchain in fashion", "fashion data analytics", "fashion AI", "fashion machine learning",
        
        # Specific Clothing Items
        "dresses", "skirts", "pants", "trousers", "jeans", "denim", "t-shirts", "blouses", "shirts", "sweaters", "cardigans", 
        "hoodies", "jackets", "coats", "blazers", "suits", "tuxedos", "vests", "waistcoats", "shorts", "leggings", "loungewear", 
        "sleepwear", "underwear", "lingerie", "socks", "tights", "stockings", "hats", "caps", "berets", "scarves", "gloves", 
        "mittens", "belts", "ties", "bow ties", "suspenders", "braces", "pocket squares", "handkerchiefs", "cufflinks", "lapel pins",
        
        # Footwear
        "shoes", "boots", "sneakers", "trainers", "athletic shoes", "running shoes", "basketball shoes", "tennis shoes", "heels", 
        "high heels", "pumps", "stilettos", "flats", "loafers", "oxfords", "brogues", "wingtips", "moccasins", "boat shoes", 
        "espadrilles", "sandals", "flip-flops", "sliders", "slippers", "mules", "clogs", "wedges", "platforms", "block heels", 
        "kitten heels", "ankle boots", "knee-high boots", "thigh-high boots", "Wellington boots", "rain boots", "snow boots", 
        "hiking boots", "combat boots", "desert boots", "Chelsea boots", "cowboy boots",
        
        # Fashion Cultures & Subcultures
        "street fashion", "urban style", "hip-hop fashion", "punk fashion", "gothic fashion", "lolita fashion", "kawaii fashion", 
        "harajuku style", "preppy fashion", "ivy league style", "western wear", "country fashion", "bohemian fashion", "hippie style", 
        "mod fashion", "retro fashion", "rockabilly style", "grunge fashion", "rave fashion", "festival fashion", "cyberpunk fashion", 
        "steampunk fashion", "normcore", "dark academia", "light academia", "cottagecore", "y2k fashion", "techwear", "vaporwave aesthetic", 
        "hypebeast culture", "luxury streetwear", "skater style", "surfer style", "biker fashion", "military fashion", "nautical fashion",
        
        # Regional & Cultural Fashion
        "Japanese fashion", "Korean fashion", "Chinese fashion", "Indian fashion", "African fashion", "European fashion", "Scandinavian fashion", 
        "French fashion", "Italian fashion", "British fashion", "American fashion", "Latin American fashion", "Middle Eastern fashion", 
        "traditional clothing", "cultural garments", "ethnic fashion", "indigenous fashion", "folk costume", "national dress", "sari", 
        "kimono", "hanbok", "qipao", "cheongsam", "kaftan", "djellaba", "dashiki", "kente cloth", "tartan", "plaid", "batik", 
        "ankara fabric", "embroidered garments", "beaded clothing", "cultural appropriation", "cultural appreciation", "fusion fashion",
        
        # Fabrics & Materials
        "cotton", "wool", "silk", "linen", "polyester", "nylon", "rayon", "viscose", "tencel", "modal", "cashmere", "angora", 
        "mohair", "alpaca", "merino wool", "tweed", "denim", "canvas", "corduroy", "velvet", "velour", "chiffon", "organza", 
        "georgette", "crepe", "satin", "taffeta", "tulle", "lace", "leather", "faux leather", "suede", "faux suede", "fur", 
        "faux fur", "sherpa", "fleece", "spandex", "lycra", "elastane", "jersey", "piqué", "ottoman", "brocade", "jacquard", 
        "bouclé", "gingham", "herringbone", "houndstooth", "plaid", "tartan", "check", "polka dot", "stripes", "sequins", 
        "metallic fabrics", "mesh", "neoprene", "techno fabrics", "waterproof fabrics", "breathable fabrics", "moisture-wicking fabrics",
        
        # Fashion Business & Marketing
        "fashion brands", "fashion logos", "fashion advertising", "fashion campaigns", "fashion lookbooks", "fashion catalogs", 
        "fashion product photography", "fashion editorials", "fashion e-commerce", "online fashion retailers", "fashion marketplaces", 
        "fashion pop-up shops", "concept stores", "department stores", "fashion boutiques", "fashion flagships", "fashion outlets", 
        "fashion discount codes", "fashion sales", "fashion seasonality", "fashion calendar", "fashion drops", "limited editions", 
        "fashion collabs", "fashion collaborations", "celebrity fashion lines", "designer partnerships", "fashion licensing", 
        "fashion distribution channels", "fashion direct-to-consumer", "fashion consumer behavior", "fashion target market", 
        "fashion pricing strategy", "fashion luxury positioning", "fashion brand identity", "fashion customer loyalty",
        
        # Fashion Professionals
        "fashion bloggers", "fashion vloggers", "fashion influencers", "fashion content creators", "fashion photographers", 
        "fashion models", "fashion stylists", "fashion editors", "fashion journalists", "fashion writers", "fashion buyers", 
        "fashion merchandisers", "fashion designers", "fashion illustrators", "fashion forecasters", "trend analysts", 
        "color specialists", "pattern makers", "fashion cutters", "fashion sewers", "fashion sample makers", "fashion technicians", 
        "fashion production managers", "fashion show producers", "fashion casting directors", "fashion show coordinators", 
        "fashion public relations", "fashion marketing specialists", "fashion brand managers", "fashion entrepreneurs", 
        "fashion retailers", "fashion consultants", "personal stylists", "wardrobe stylists", "celebrity stylists", "fashion scouts",
        
        # Fashion Events & Occasions
        "fashion weeks", "New York Fashion Week", "London Fashion Week", "Milan Fashion Week", "Paris Fashion Week", "Tokyo Fashion Week", 
        "Seoul Fashion Week", "Copenhagen Fashion Week", "fashion trade shows", "Pitti Uomo", "fashion exhibitions", "costume exhibitions", 
        "fashion museums", "fashion galas", "Met Gala", "fashion awards", "CFDA awards", "British Fashion Awards", "fashion fundraisers", 
        "fashion charity events", "trunk shows", "sample sales", "fashion conferences", "fashion summits", "fashion talks", 
        "fashion networking events", "fashion after-parties", "fashion dinners", "fashion launches", "fashion pop-ups",
        
        # Fashion History & Vintage
        "fashion history", "costume history", "historical fashion", "fashion archives", "fashion eras", "1920s fashion", "1930s fashion", 
        "1940s fashion", "1950s fashion", "1960s fashion", "1970s fashion", "1980s fashion", "1990s fashion", "2000s fashion", 
        "vintage clothing", "retro fashion", "antique clothing", "second-hand clothing", "pre-loved fashion", "thrift shopping", 
        "vintage shopping", "vintage dealers", "vintage collectors", "fashion curation", "vintage reproduction", "historical costumes", 
        "period clothing", "historical reenactment", "vintage patterns", "vintage fabrics", "fashion preservation", "fashion restoration",
        
        # Fashion Psychology & Sociology
        "fashion psychology", "fashion sociology", "fashion identity", "fashion self-expression", "fashion and gender", "fashion and race", 
        "fashion and class", "fashion and culture", "fashion and politics", "fashion statements", "fashion symbolism", "fashion communication", 
        "fashion semiotics", "fashion theory", "fashion academia", "fashion research", "fashion studies", "fashion anthropology", 
        "fashion tribalism", "fashion group dynamics", "fashion conformity", "fashion rebellion", "fashion and body image", 
        "fashion inclusivity", "fashion diversity", "fashion representation", "fashion and feminism", "fashion and masculinity", 
        "fashion and sexuality", "fashion and religion", "fashion and subcultures", "fashion and power",
        
        # Fashion Trends & Movements
        "color blocking", "pattern mixing", "layering", "oversized", "androgynous", "gender-neutral fashion", "unisex clothing", 
        "size-inclusive fashion", "plus-size fashion", "adaptive fashion", "modest fashion", "maximalism", "minimalism", 
        "color trends", "Pantone color of the year", "seasonal color palettes", "fashion week trends", "street style trends", 
        "micro-trends", "macro-trends", "trend cycles", "trend forecasting", "trend prediction", "fashion fads", "fashion classics", 
        "wardrobe essentials", "investment pieces", "staple items", "capsule wardrobe", "fashion uniforms", "signature style", 
        "iconic fashion", "statement pieces", "fashion comebacks", "fashion revivals", "nostalgic fashion",
        
        # Accessories & Details
        "bags", "purses", "handbags", "tote bags", "clutches", "backpacks", "messenger bags", "crossbody bags", "bucket bags", 
        "fanny packs", "belt bags", "luggage", "wallets", "card holders", "key chains", "phone cases", "tech accessories", 
        "jewelry", "necklaces", "pendants", "chokers", "earrings", "studs", "hoops", "rings", "bangles", "bracelets", "cuffs", 
        "watches", "broaches", "pins", "hair accessories", "scrunchies", "hair clips", "headbands", "hair ties", "glasses", 
        "sunglasses", "eyewear", "optical frames", "scarves", "wraps", "shawls", "ties", "bow ties", "belts", "suspenders", 
        "gloves", "mittens", "hats", "caps", "beanies", "berets", "fedoras", "baseball caps", "bucket hats", "visors", 
        "umbrellas", "fans", "handkerchiefs", "pocket squares", "buttons", "zippers", "laces", "ribbons", "trims", 
        "appliqués", "patches", "embroidery", "embellishments", "sequins", "beads", "rhinestones", "studs", "spikes", "tassels", "fringe",
        
        # Fashion Education & Learning
        "fashion schools", "fashion colleges", "fashion universities", "fashion institutes", "fashion courses", "fashion degrees", 
        "fashion diplomas", "fashion workshops", "fashion masterclasses", "fashion tutorials", "fashion webinars", "fashion education", 
        "fashion mentorship", "fashion apprenticeships", "fashion internships", "fashion residencies", "fashion competitions", 
        "fashion scholarships", "fashion grants", "fashion portfolios", "fashion presentations", "fashion critiques", 
        "fashion curriculum", "fashion pedagogy", "fashion research", "fashion academia", "fashion theory", "fashion textbooks", 
        "fashion learning resources", "fashion skills", "fashion techniques", "fashion craftsmanship",
        
        # Beauty & Related Fields
        "beauty trends", "makeup looks", "skincare routines", "hair styling", "nail art", "perfume", "fragrance", "cosmetics", 
        "makeup brands", "skincare brands", "beauty products", "beauty tools", "beauty tech", "beauty influencers", "beauty bloggers", 
        "beauty tutorials", "beauty photography", "beauty campaigns", "beauty advertising", "clean beauty", "natural beauty", 
        "organic beauty", "vegan beauty", "cruelty-free beauty", "makeup artistry", "hair styling", "hair coloring", 
        "hair cutting", "beauty treatments", "beauty services", "beauty salons", "beauty spas", "self-care routines",
        
        # Fashion Law & Regulation
        "fashion law", "fashion intellectual property", "fashion trademarks", "fashion patents", "fashion copyright", 
        "fashion design protection", "fashion counterfeits", "fashion knockoffs", "fashion legal disputes", "fashion lawsuits", 
        "fashion contracts", "fashion licensing agreements", "fashion trade agreements", "fashion import regulations", 
        "fashion export regulations", "fashion tariffs", "fashion customs", "fashion labeling requirements", "fashion product safety", 
        "fashion standards", "fashion certifications", "fashion compliance", "fashion worker rights", "fashion labor laws", 
        "fashion unions", "fashion activism", "fashion advocacy", "fashion policy", "fashion legislation",
        
        # Advanced Categories
        "neurofashion", "biofashion", "biomimicry in fashion", "adaptive clothing", "therapeutic fashion", "sensory fashion", 
        "interactive fashion", "responsive garments", "climate-adaptive clothing", "temperature-regulating fabrics", "phase-change materials", 
        "body-monitoring garments", "fashion robotics", "fashion automation", "fashion 3D printing", "fashion parametric design", 
        "computational fashion", "algorithmic fashion", "generative design", "AI fashion design", "fashion data science", 
        "fashion analytics", "fashion forecasting algorithms", "fashion blockchain", "fashion supply chain transparency", 
        "fashion traceability", "fashion circular economy", "fashion biodesign", "fashion mycology", "grown garments", 
        "biofabricated materials", "lab-grown leather", "cultured fur", "bacterial dyes", "natural pigments", "living textiles", 
        "fashion photovoltaics", "energy-harvesting clothing", "fashion IoT", "fashion genderless", "fashion inclusive design", 
        "fashion universal design", "fashion for disabilities", "fashion for aging", "fashion gerontology", "fashion anthropometrics",
        
        # Global Fashion Brands
        "Gucci", "Louis Vuitton", "Chanel", "Dior", "Prada", "Hermès", "Balenciaga", "Valentino", "Versace", "Fendi", 
        "Saint Laurent", "Burberry", "Givenchy", "Loewe", "Celine", "Bottega Veneta", "Alexander McQueen", "Dolce & Gabbana", 
        "Marc Jacobs", "Jacquemus", "Off-White", "Vetements", "Marine Serre", "Acne Studios", "Maison Margiela", "Rick Owens", 
        "Raf Simons", "JW Anderson", "Simone Rocha", "Erdem", "Stella McCartney", "Isabel Marant", "Chloé", "Balmain", "Mugler", 
        "Schiaparelli", "Jil Sander", "The Row", "Supreme", "Palace", "Stüssy", "Nike", "Adidas", "Puma", "New Balance", 
        "Converse", "Vans", "Dr. Martens", "Birkenstock", "Zara", "H&M", "Uniqlo", "Cos", "& Other Stories", "Arket", 
        "Everlane", "Reformation", "Ganni", "Nanushka", "Rejina Pyo", "Coperni", "Casablanca", "Lemaire", "A.P.C.", 
        "Totême", "Khaite", "Wardrobe.NYC", "Fear of God", "Amiri", "Awake NY", "Bode", "Telfar", "Brother Vellies", 
        "Christopher John Rogers", "Dries Van Noten", "Comme des Garçons", "Junya Watanabe", "Yohji Yamamoto", "Issey Miyake", 
        "Sacai", "Undercover", "Visvim", "Kapital", "Needles", "Neighborhood", "WTAPS", "Kenzo", "Moncler", "Stone Island", 
        "CP Company", "Brunello Cucinelli", "Loro Piana", "Missoni", "Marni", "Mara Hoffman"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "fashion"})
    df = df.drop_duplicates(subset=["keyword"]) 
    return df

def save_fashion_keywords(output_file="fashion_keywords.parquet"):
    """Save fashion keywords to a parquet file. Append if the file already exists."""
    new_df = get_fashion_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique fashion keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique fashion keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_fashion_keywords()