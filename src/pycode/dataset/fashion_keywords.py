import pandas as pd
import os

def get_fashion_keywords():
    """Return a DataFrame of comprehensive fashion-related keywords."""
    keywords = [
        
        "fashion", "style", "clothing", "accessories", "footwear", "hairstyles", "makeup", "skincare", "runway", "fashion week",
        "designer brands", "street style", "vintage fashion", "high fashion", "fast fashion", "ethical fashion", "sustainable fashion", 
        "luxury brands", "casual wear", "formal wear", "activewear", "sportswear", "swimwear", "outerwear", "seasonal fashion", 
        "trendy", "classic style", "boho chic", "minimalist style", "urban fashion", "fashion accessories", "handbags", "jewelry", 
        "sunglasses", "watches", "fashion influencers", "style bloggers", "fashion photography", "editorial shoots", "lookbooks",
        
       
        "fashion magazines", "Vogue", "Elle", "Harper's Bazaar", "GQ", "InStyle", "W Magazine", "Cosmopolitan", "Marie Claire", 
        "Vanity Fair", "V Magazine", "The Gentlemen's Quarterly", "Esquire", "Numero", "Dazed", "i-D Magazine", "Paper Magazine", 
        "Teen Vogue", "Glamour", "fashion podcasts", "style vlogs", "fashion livestreams", "fashion documentaries", "fashion journalism",
        
        
        "fashion shows", "catwalk", "couture", "ready-to-wear", "bespoke tailoring", "streetwear", "athleisure", "pret-a-porter", 
        "haute couture", "fashion merchandising", "fashion buying", "fashion marketing", "fashion retail", "fashion distribution", 
        "fashion production", "supply chain", "fashion manufacturing", "clothing factories", "garment industry", "textile industry",
        "fashion wholesale", "fashion showrooms", "fashion trade shows", "fashion weeks", "fashion capitals", "fashion districts",
        
        
        "sustainable fabrics", "organic cotton", "recycled fabrics", "fashion sustainability", "fashion revolution", "fast fashion impact", 
        "upcycled fashion", "eco-friendly brands", "ethical sourcing", "fair trade fashion", "zero-waste fashion", "circular fashion", 
        "biodegradable fabrics", "vegan fashion", "cruelty-free fashion", "slow fashion", "fashion transparency", "garment worker rights", 
        "fashion activism", "sustainable luxury", "green fashion", "eco-conscious", "clothing lifecycle", "textile recycling", 
        "fashion footprint", "water conservation", "fashion carbon offset", "fashion pollution", "microplastics", "fashion waste",
        
        
        "fashion design", "couture collections", "fashion houses", "fashion designers", "runway shows", "fashion stylists", 
        "custom clothing", "tailored suits", "seamstress", "fashion trends", "fashion forecasting", "seasonal trends", "color trends", 
        "fabric trends", "pattern making", "draping", "fashion sketching", "fashion illustrations", "fashion prototyping", "fashion CAD", 
        "fashion technology", "3D printing fashion", "textile design", "fabric manipulation", "fashion embellishment", "embroidery", 
        "beading", "fashion appliqué", "fashion silhouettes", "fashion proportions", "fashion fit", "fashion styling",
        
        
        "digital fashion", "virtual fashion", "fashion tech", "wearable technology", "smart textiles", "fashion apps", "virtual try-on", 
        "augmented reality fashion", "3D fashion design", "fashion NFTs", "meta fashion", "fashion in metaverse", "digital clothing", 
        "fashion gaming", "fashion avatars", "fashion e-commerce", "fashion algorithms", "fashion recommendation systems", 
        "fashion image recognition", "fashion virtual showrooms", "fashion AR filters", "fashion QR codes", "RFID fashion", 
        "blockchain in fashion", "fashion data analytics", "fashion AI", "fashion machine learning",
        
        
        "dresses", "skirts", "pants", "trousers", "jeans", "denim", "t-shirts", "blouses", "shirts", "sweaters", "cardigans", 
        "hoodies", "jackets", "coats", "blazers", "suits", "tuxedos", "vests", "waistcoats", "shorts", "leggings", "loungewear", 
        "sleepwear", "underwear", "lingerie", "socks", "tights", "stockings", "hats", "caps", "berets", "scarves", "gloves", 
        "mittens", "belts", "ties", "bow ties", "suspenders", "braces", "pocket squares", "handkerchiefs", "cufflinks", "lapel pins",
        
        
        "shoes", "boots", "sneakers", "trainers", "athletic shoes", "running shoes", "basketball shoes", "tennis shoes", "heels", 
        "high heels", "pumps", "stilettos", "flats", "loafers", "oxfords", "brogues", "wingtips", "moccasins", "boat shoes", 
        "espadrilles", "sandals", "flip-flops", "sliders", "slippers", "mules", "clogs", "wedges", "platforms", "block heels", 
        "kitten heels", "ankle boots", "knee-high boots", "thigh-high boots", "Wellington boots", "rain boots", "snow boots", 
        "hiking boots", "combat boots", "desert boots", "Chelsea boots", "cowboy boots",
        
        
        "street fashion", "urban style", "hip-hop fashion", "punk fashion", "gothic fashion", "lolita fashion", "kawaii fashion", 
        "harajuku style", "preppy fashion", "ivy league style", "western wear", "country fashion", "bohemian fashion", "hippie style", 
        "mod fashion", "retro fashion", "rockabilly style", "grunge fashion", "rave fashion", "festival fashion", "cyberpunk fashion", 
        "steampunk fashion", "normcore", "dark academia", "light academia", "cottagecore", "y2k fashion", "techwear", "vaporwave aesthetic", 
        "hypebeast culture", "luxury streetwear", "skater style", "surfer style", "biker fashion", "military fashion", "nautical fashion",
        
        
        "Japanese fashion", "Korean fashion", "Chinese fashion", "Indian fashion", "African fashion", "European fashion", "Scandinavian fashion", 
        "French fashion", "Italian fashion", "British fashion", "American fashion", "Latin American fashion", "Middle Eastern fashion", 
        "traditional clothing", "cultural garments", "ethnic fashion", "indigenous fashion", "folk costume", "national dress", "sari", 
        "kimono", "hanbok", "qipao", "cheongsam", "kaftan", "djellaba", "dashiki", "kente cloth", "tartan", "plaid", "batik", 
        "ankara fabric", "embroidered garments", "beaded clothing", "cultural appropriation", "cultural appreciation", "fusion fashion",
        
        
        "cotton", "wool", "silk", "linen", "polyester", "nylon", "rayon", "viscose", "tencel", "modal", "cashmere", "angora", 
        "mohair", "alpaca", "merino wool", "tweed", "denim", "canvas", "corduroy", "velvet", "velour", "chiffon", "organza", 
        "georgette", "crepe", "satin", "taffeta", "tulle", "lace", "leather", "faux leather", "suede", "faux suede", "fur", 
        "faux fur", "sherpa", "fleece", "spandex", "lycra", "elastane", "jersey", "piqué", "ottoman", "brocade", "jacquard", 
        "bouclé", "gingham", "herringbone", "houndstooth", "plaid", "tartan", "check", "polka dot", "stripes", "sequins", 
        "metallic fabrics", "mesh", "neoprene", "techno fabrics", "waterproof fabrics", "breathable fabrics", "moisture-wicking fabrics",
        
        
        "fashion brands", "fashion logos", "fashion advertising", "fashion campaigns", "fashion lookbooks", "fashion catalogs", 
        "fashion product photography", "fashion editorials", "fashion e-commerce", "online fashion retailers", "fashion marketplaces", 
        "fashion pop-up shops", "concept stores", "department stores", "fashion boutiques", "fashion flagships", "fashion outlets", 
        "fashion discount codes", "fashion sales", "fashion seasonality", "fashion calendar", "fashion drops", "limited editions", 
        "fashion collabs", "fashion collaborations", "celebrity fashion lines", "designer partnerships", "fashion licensing", 
        "fashion distribution channels", "fashion direct-to-consumer", "fashion consumer behavior", "fashion target market", 
        "fashion pricing strategy", "fashion luxury positioning", "fashion brand identity", "fashion customer loyalty",
        
        
        "fashion bloggers", "fashion vloggers", "fashion influencers", "fashion content creators", "fashion photographers", 
        "fashion models", "fashion stylists", "fashion editors", "fashion journalists", "fashion writers", "fashion buyers", 
        "fashion merchandisers", "fashion designers", "fashion illustrators", "fashion forecasters", "trend analysts", 
        "color specialists", "pattern makers", "fashion cutters", "fashion sewers", "fashion sample makers", "fashion technicians", 
        "fashion production managers", "fashion show producers", "fashion casting directors", "fashion show coordinators", 
        "fashion public relations", "fashion marketing specialists", "fashion brand managers", "fashion entrepreneurs", 
        "fashion retailers", "fashion consultants", "personal stylists", "wardrobe stylists", "celebrity stylists", "fashion scouts",
        
        
        "fashion weeks", "New York Fashion Week", "London Fashion Week", "Milan Fashion Week", "Paris Fashion Week", "Tokyo Fashion Week", 
        "Seoul Fashion Week", "Copenhagen Fashion Week", "fashion trade shows", "Pitti Uomo", "fashion exhibitions", "costume exhibitions", 
        "fashion museums", "fashion galas", "Met Gala", "fashion awards", "CFDA awards", "British Fashion Awards", "fashion fundraisers", 
        "fashion charity events", "trunk shows", "sample sales", "fashion conferences", "fashion summits", "fashion talks", 
        "fashion networking events", "fashion after-parties", "fashion dinners", "fashion launches", "fashion pop-ups",
        
        
        "fashion history", "costume history", "historical fashion", "fashion archives", "fashion eras", "1920s fashion", "1930s fashion", 
        "1940s fashion", "1950s fashion", "1960s fashion", "1970s fashion", "1980s fashion", "1990s fashion", "2000s fashion", 
        "vintage clothing", "retro fashion", "antique clothing", "second-hand clothing", "pre-loved fashion", "thrift shopping", 
        "vintage shopping", "vintage dealers", "vintage collectors", "fashion curation", "vintage reproduction", "historical costumes", 
        "period clothing", "historical reenactment", "vintage patterns", "vintage fabrics", "fashion preservation", "fashion restoration",
        
        
        "fashion psychology", "fashion sociology", "fashion identity", "fashion self-expression", "fashion and gender", "fashion and race", 
        "fashion and class", "fashion and culture", "fashion and politics", "fashion statements", "fashion symbolism", "fashion communication", 
        "fashion semiotics", "fashion theory", "fashion academia", "fashion research", "fashion studies", "fashion anthropology", 
        "fashion tribalism", "fashion group dynamics", "fashion conformity", "fashion rebellion", "fashion and body image", 
        "fashion inclusivity", "fashion diversity", "fashion representation", "fashion and feminism", "fashion and masculinity", 
        "fashion and sexuality", "fashion and religion", "fashion and subcultures", "fashion and power",
        
       
        "color blocking", "pattern mixing", "layering", "oversized", "androgynous", "gender-neutral fashion", "unisex clothing", 
        "size-inclusive fashion", "plus-size fashion", "adaptive fashion", "modest fashion", "maximalism", "minimalism", 
        "color trends", "Pantone color of the year", "seasonal color palettes", "fashion week trends", "street style trends", 
        "micro-trends", "macro-trends", "trend cycles", "trend forecasting", "trend prediction", "fashion fads", "fashion classics", 
        "wardrobe essentials", "investment pieces", "staple items", "capsule wardrobe", "fashion uniforms", "signature style", 
        "iconic fashion", "statement pieces", "fashion comebacks", "fashion revivals", "nostalgic fashion",
        
        
        "bags", "purses", "handbags", "tote bags", "clutches", "backpacks", "messenger bags", "crossbody bags", "bucket bags", 
        "fanny packs", "belt bags", "luggage", "wallets", "card holders", "key chains", "phone cases", "tech accessories", 
        "jewelry", "necklaces", "pendants", "chokers", "earrings", "studs", "hoops", "rings", "bangles", "bracelets", "cuffs", 
        "watches", "broaches", "pins", "hair accessories", "scrunchies", "hair clips", "headbands", "hair ties", "glasses", 
        "sunglasses", "eyewear", "optical frames", "scarves", "wraps", "shawls", "ties", "bow ties", "belts", "suspenders", 
        "gloves", "mittens", "hats", "caps", "beanies", "berets", "fedoras", "baseball caps", "bucket hats", "visors", 
        "umbrellas", "fans", "handkerchiefs", "pocket squares", "buttons", "zippers", "laces", "ribbons", "trims", 
        "appliqués", "patches", "embroidery", "embellishments", "sequins", "beads", "rhinestones", "studs", "spikes", "tassels", "fringe",
        
        
        "fashion schools", "fashion colleges", "fashion universities", "fashion institutes", "fashion courses", "fashion degrees", 
        "fashion diplomas", "fashion workshops", "fashion masterclasses", "fashion tutorials", "fashion webinars", "fashion education", 
        "fashion mentorship", "fashion apprenticeships", "fashion internships", "fashion residencies", "fashion competitions", 
        "fashion scholarships", "fashion grants", "fashion portfolios", "fashion presentations", "fashion critiques", 
        "fashion curriculum", "fashion pedagogy", "fashion research", "fashion academia", "fashion theory", "fashion textbooks", 
        "fashion learning resources", "fashion skills", "fashion techniques", "fashion craftsmanship",
        
        
        "beauty trends", "makeup looks", "skincare routines", "hair styling", "nail art", "perfume", "fragrance", "cosmetics", 
        "makeup brands", "skincare brands", "beauty products", "beauty tools", "beauty tech", "beauty influencers", "beauty bloggers", 
        "beauty tutorials", "beauty photography", "beauty campaigns", "beauty advertising", "clean beauty", "natural beauty", 
        "organic beauty", "vegan beauty", "cruelty-free beauty", "makeup artistry", "hair styling", "hair coloring", 
        "hair cutting", "beauty treatments", "beauty services", "beauty salons", "beauty spas", "self-care routines",
        
       
        "fashion law", "fashion intellectual property", "fashion trademarks", "fashion patents", "fashion copyright", 
        "fashion design protection", "fashion counterfeits", "fashion knockoffs", "fashion legal disputes", "fashion lawsuits", 
        "fashion contracts", "fashion licensing agreements", "fashion trade agreements", "fashion import regulations", 
        "fashion export regulations", "fashion tariffs", "fashion customs", "fashion labeling requirements", "fashion product safety", 
        "fashion standards", "fashion certifications", "fashion compliance", "fashion worker rights", "fashion labor laws", 
        "fashion unions", "fashion activism", "fashion advocacy", "fashion policy", "fashion legislation",
        
        
        "neurofashion", "biofashion", "biomimicry in fashion", "adaptive clothing", "therapeutic fashion", "sensory fashion", 
        "interactive fashion", "responsive garments", "climate-adaptive clothing", "temperature-regulating fabrics", "phase-change materials", 
        "body-monitoring garments", "fashion robotics", "fashion automation", "fashion 3D printing", "fashion parametric design", 
        "computational fashion", "algorithmic fashion", "generative design", "AI fashion design", "fashion data science", 
        "fashion analytics", "fashion forecasting algorithms", "fashion blockchain", "fashion supply chain transparency", 
        "fashion traceability", "fashion circular economy", "fashion biodesign", "fashion mycology", "grown garments", 
        "biofabricated materials", "lab-grown leather", "cultured fur", "bacterial dyes", "natural pigments", "living textiles", 
        "fashion photovoltaics", "energy-harvesting clothing", "fashion IoT", "fashion genderless", "fashion inclusive design", 
        "fashion universal design", "fashion for disabilities", "fashion for aging", "fashion gerontology", "fashion anthropometrics",
        
        
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
        "CP Company", "Brunello Cucinelli", "Loro Piana", "Missoni", "Marni", "Mara Hoffman",
        "Proenza Schouler", "Thom Browne", "Zimmermann", "Molly Goddard", "Wales Bonner", "Eckhaus Latta", "Vaquera", 
    "Richard Quinn", "Craig Green", "Y/Project", "GmbH", "Martine Rose", "Peter Do", "Alyx", "Matthew Williams", 
    "Ludovic de Saint Sernin", "Charles Jeffrey Loverboy", "Matty Bovan", "Area", "Collina Strada", "Ahluwalia", 
    "Asai", "Rokh", "Cecilie Bahnsen", "Supriya Lele", "Commission", "Chopova Lowena", "Stefan Cooke", "Maisie Wilen", 
    "Kenneth Ize", "Thebe Magugu", "Ambush", "Judy Turner", "Denzilpatrick", "Bianca Saunders", "Ernest W. Baker", 
    "Kiko Kostadinov", "Charlotte Knowles", "Labrum London", "Maximilian", "S.S. Daley", "Connor Ives", "Harris Reed",
    
    
    "Lululemon", "Gymshark", "Alo Yoga", "Athleta", "Fabletics", "Under Armour", "Vuori", "Outdoor Voices", "Sweaty Betty", 
    "P.E Nation", "Girlfriend Collective", "Beyond Yoga", "Rhone", "Ten Thousand", "On Running", "Hoka", "Brooks", 
    "compression wear", "workout sets", "gym leggings", "seamless activewear", "yoga pants", "sports bras", "performance fabrics", 
    "moisture-wicking technology", "athletic shorts", "compression socks", "fitness trackers", "smartwatches", "running gear",
    "tennis skirts", "workout tanks", "racerback tops", "quarter-zip pullovers", "track jackets", "weightlifting belts", 
    "lifting gloves", "sweatbands", "cooling towels", "recovery wear", "posture correctors", "muscle recovery", "thermal base layers",
    
    
    "old money aesthetic", "clean girl aesthetic", "coastal grandmother", "barbiecore", "balletcore", "coquette", "mermaidcore", 
    "indie sleaze", "twee fashion", "demure fashion", "whimsigothic", "clowncore", "fairycore", "angelcore", "goblincore", 
    "regencycore", "royalcore", "soft goth", "dreamcore", "weirdcore", "heroinecore", "villain era", "quiet luxury", 
    "mob wife aesthetic", "blokecore", "dopamine dressing", "grandpacore", "gorpcore", "avant apocalypse", "avant basic", 
    "brat summer", "coconut girl", "eclectic grandpa", "feral girl summer", "night luxe", "rom-com core", "horsegirl aesthetic", 
    "librarian chic", "ski lodge aesthetic", "post-apocalyptic fashion", "folklore aesthetic", "witchcore", "parisian aesthetic",
    
    
    "mom jeans", "dad jeans", "boyfriend jeans", "girlfriend jeans", "skinny jeans", "straight leg jeans", "wide leg jeans", 
    "barrel jeans", "bootcut jeans", "flared jeans", "low-rise jeans", "mid-rise jeans", "high-rise jeans", "ultra high-rise", 
    "cropped jeans", "distressed denim", "acid wash", "stone wash", "raw denim", "selvedge denim", "stretch denim", "rigid denim", 
    "tapered jeans", "carpenter jeans", "cigarette jeans", "jean shorts", "denim skirts", "denim dresses", "denim jumpsuits", 
    "denim jackets", "denim shirts", "denim overalls", "double denim", "canadian tuxedo", "denim repair", "denim care", "jean sizing",
    
    
    "tailored garments", "sartorial details", "made-to-measure", "custom tailoring", "suit alterations", "lapel styles", 
    "notch lapel", "peak lapel", "shawl lapel", "suit vents", "single vent", "double vent", "ventless", "suit canvassing", 
    "half-canvassed", "full-canvassed", "fused suits", "suit shoulders", "roped shoulders", "natural shoulders", "padded shoulders", 
    "sleeve buttons", "surgeon cuffs", "working buttonholes", "suit trouser breaks", "no break", "quarter break", "half break", 
    "full break", "suit fabrics", "super numbers", "tropical wool", "sharkskin fabric", "mohair suits", "linen suits", "dress codes", 
    "white tie", "black tie", "creative black tie", "cocktail attire", "business formal", "business casual", "smart casual", 
    "morning dress", "shirt collars", "spread collar", "button-down collar", "club collar", "cutaway collar", "shirt cuffs", 
    "barrel cuffs", "french cuffs", "convertible cuffs", "tie knots", "windsor knot", "half-windsor", "four-in-hand", "tuxedo parts",
    
    
    "argyle pattern", "animal prints", "leopard print", "cheetah print", "zebra print", "snakeskin print", "tiger stripe", 
    "cow print", "abstract prints", "geometric prints", "paisley pattern", "chevron pattern", "polka dots", "micro dots", 
    "pinstripes", "chalk stripes", "windowpane checks", "glen plaid", "Prince of Wales check", "tattersall check", "madras plaid", 
    "blackwatch tartan", "royal stewart tartan", "floral prints", "ditsy florals", "tropical prints", "toile pattern", 
    "camouflage print", "digital camo", "tie-dye", "shibori", "batik prints", "block prints", "ikat prints", "damask pattern", 
    "baroque prints", "art deco prints", "optical illusion prints", "conversational prints", "novelty prints", "monogram prints",
    
    
    "garment construction", "flat pattern making", "digital pattern making", "garment sampling", "fabric cutting", "fabric laying", 
    "marker making", "fabric yield", "fabric consumption", "sewing processes", "seam types", "overlocking", "coverstitching", 
    "flatlock stitching", "blind stitching", "topstitching", "edgestitching", "pick stitching", "hand stitching", "embroidery types", 
    "smocking", "shirring", "pleating", "darting", "gathering", "ruching", "garment finishing", "garment washing", "enzyme wash", 
    "distressing techniques", "garment dyeing", "piece dyeing", "yarn dyeing", "solution dyeing", "quality assurance", "garment inspection", 
    "clothing tolerances", "spec sheets", "tech packs", "grading", "size scaling", "fashion PLM software", "clothing manufacturing ERP",
    
    
    "fashion AI applications", "virtual showrooms", "digital fashion marketplaces", "fashion NFT platforms", "avatar styling", 
    "digital fashion design software", "CLO 3D", "Browzwear", "Optitex", "Gerber AccuMark", "Lectra", "Stylumia", "Heuritech", 
    "body scanning technology", "size recommendation systems", "fit prediction", "virtual fit models", "digital fashion influencers", 
    "virtual models", "fashion CGI", "digitally-rendered fashion", "haptic fashion", "programmable textiles", "e-textiles", 
    "conductive fabrics", "fashion robotics", "automated sewing", "knitting machines", "fashion drones", "fashion holographics", 
    "AR dressing rooms", "VR fashion experiences", "social shopping", "livestream commerce", "shoppable video", "fashion chatbots",
    
    
    "direct-to-consumer fashion", "fashion subscription boxes", "clothing rental services", "peer-to-peer fashion resale", 
    "authenticated luxury consignment", "pre-order models", "made-to-order fashion", "crowdfunded fashion", "community-supported apparel", 
    "fashion co-creation", "customization platforms", "fashion incubators", "design collectives", "fashion accelerators", 
    "fashion venture capital", "private equity in fashion", "fashion SPACs", "fashion IPOs", "fashion conglomerates", 
    "fashion holding companies", "fashion licenses", "fashion franchising", "capsule collections", "limited drops", 
    "fashion collaborations", "influencer collections", "celebrity fashion ventures", "athlete clothing lines", "fashion affiliates",
    
   
    "garment finishings", "raw edge", "selvage edge", "rolled hem", "lettuce edge", "bound edge", "faced edge", "french seam", 
    "flat-felled seam", "welt seam", "lapped seam", "princess seam", "raglan sleeve", "dolman sleeve", "bishop sleeve", 
    "butterfly sleeve", "bell sleeve", "puff sleeve", "leg-of-mutton sleeve", "cap sleeve", "kimono sleeve", "batwing sleeve", 
    "set-in sleeve", "yokes", "godet", "gusset", "ruffle", "flounce", "peplum", "basque", "gore", "placket", "welt pocket", 
    "jetted pocket", "patch pocket", "slant pocket", "ticket pocket", "kangaroo pocket", "coin pocket", "bellows pocket", 
    "slash pocket", "inseam pocket", "mandarin collar", "peter pan collar", "revere collar", "stand collar", "turtleneck", "cowl neck",
    
    
    "lyocell", "cupro", "qiana", "econyl", "rPET", "recycled polyester", "bioplastics", "PLA fiber", "micromodal", "milkweed fiber", 
    "lotus fiber", "pineapple leather", "mushroom leather", "mycelium materials", "apple leather", "grape leather", "cactus leather", 
    "corn fiber", "seacell", "banana fiber", "orange fiber", "soybean fiber", "regenerated protein fibers", "bast fibers", "hemp fiber", 
    "ramie", "jute", "nettle fiber", "kapok", "abaca", "coir", "bamboo viscose", "bamboo linen", "algae textiles", "chitosan fiber", 
    "seaweed fiber", "fish skin leather", "salmon leather", "eel skin leather", "bison fiber", "yak fiber", "qiviut", "vicuña wool", 
    "guanaco fiber", "muskox wool", "camel hair", "recycled silk", "peace silk", "wild silk", "spider silk", "engineered spider silk",
    
    
    "emerging fashion markets", "MENA fashion", "African fashion councils", "Lagos Fashion Week", "South African fashion", 
    "Ethiopian fashion", "Moroccan fashion", "Nigerian fashion", "Southeast Asian fashion", "Indonesian modest fashion", 
    "Malaysian fashion", "Thai fashion", "Vietnamese manufacturing", "Cambodian garment industry", "South American fashion", 
    "Brazilian fashion", "Colombian fashion", "Mexican fashion", "Argentine fashion", "Eastern European fashion", "Ukrainian fashion", 
    "Polish fashion", "Czech fashion", "Baltic fashion", "Russian fashion", "Central Asian fashion", "Nordic fashion", 
    "Finnish design", "Swedish minimalism", "Danish fashion", "Norwegian fashion", "Australian fashion", "New Zealand fashion",
    
    
    "fashion house archives", "museum fashion collections", "costume institutes", "fashion conservation", "textile conservation", 
    "fashion provenance", "vintage authentication", "designer archives", "runway archives", "editorial archives", "fashion libraries", 
    "fashion research collections", "fashion special collections", "fashion ephemera", "fashion memorabilia", "historical pattern books", 
    "vintage sewing patterns", "historical costume references", "fashion plates", "fashion illustration archives", "fashion photography archives",
    
    
    "omnichannel fashion retail", "unified commerce", "clienteling", "personal shopping", "VIP shopping", "private shopping", 
    "trunk shows", "fashion previews", "pre-season ordering", "made-to-measure events", "bespoke appointments", "alteration services", 
    "styling services", "wardrobe consultation", "closet organization", "fashion concierge", "luxury shopping tourism", 
    "duty-free fashion", "fashion outlet villages", "fashion department stores", "concept fashion stores", "fashion flagships", 
    "experiential retail", "interactive fashion displays", "smart fitting rooms", "magic mirrors", "fashion retail analytics", 
    "customer journey mapping", "fashion consumer insights", "fashion conversion optimization", "retail visual merchandising",
    
    
    "transitional wardrobes", "day-to-night fashion", "packable clothing", "travel wardrobes", "capsule wardrobes", "modular clothing", 
    "versatile pieces", "multifunctional garments", "convertible clothing", "reversible garments", "adjustable sizing", 
    "adaptive fashion", "inclusive design", "universal fashion", "accessible clothing", "sensory-friendly fashion", "easy dressing", 
    "magnetic closures", "velcro closures", "zip-free clothing", "pull-on styles", "elasticized waistbands", "stretch panels", 
    "postpartum fashion", "maternity wardrobes", "nursing-friendly fashion", "kid-proof fashion", "stain-resistant fabrics", 
    "water-repellent treatments", "wrinkle-resistant fabrics", "anti-microbial finishes", "odor-resistant textiles", "UV-protective clothing"
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