import pandas as pd
import os

def get_art_keywords():
    """Return a DataFrame of expanded art-related keywords."""
    keywords = [
        # Original keywords
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
        "environmental design", "space design", "product design", "industrial design", "exhibition design", "fashion design", "textile design", "furniture design", "interior design",
        
        # Art techniques & processes
        "glazing", "impasto", "underpainting", "fresco", "grisaille", "pointillism", "sfumato", "chiaroscuro", "trompe l'oeil", "wet-on-wet", "dry brush",
        "sgraffito", "encaustic", "etching", "linocut", "lithography", "screen printing", "monoprinting", "woodcut", "letterpress", "aquatint", "mezzotint",
        "drypoint", "intaglio", "relief printing", "photogravure", "cyanotype", "silkscreen", "stenciling", "airbrush", "spray paint", "stippling", "hatching",
        "cross-hatching", "scumbling", "alla prima", "gesso", "varnishing", "sanding", "polishing", "patina", "assemblage", "found object art",
        "kintsugi", "shibori", "batik", "weaving", "embroidery", "quilting", "felting", "knitting", "crochet", "macramé", "beadwork", "wirework",
        "metalworking", "welding", "casting", "forging", "smelting", "lost wax casting", "soldering", "glass blowing", "kiln forming", "cold working",
        "throwing", "hand building", "coiling", "slab building", "wheel throwing", "raku", "bisque firing", "glaze firing", "wood firing", "salt firing",
        "soda firing", "pit firing", "burnishing", "terra sigillata", "paper marbling", "bookbinding", "origami", "kirigami", "quilling", "decoupage",
        "ink wash", "sumi-e", "gouache", "tempera", "fresco secco", "egg tempera", "scratchboard", "silverpoint", "goldpoint", "burnishing",
        
        # Art tools & materials
        "canvas", "palette", "easel", "kiln", "potter's wheel", "loom", "chisel", "mallet", "printmaking press", "brush", "palette knife", "spatula",
        "modeling tools", "sculpting tools", "carving tools", "burnisher", "brayer", "awl", "gouge", "burin", "etching needle", "linoleum",
        "woodblock", "printing plate", "ink", "pigment", "medium", "solvent", "fixative", "primer", "ground", "stretcher bars", "frame", "mat board",
        "paper", "sketchbook", "charcoal", "graphite", "colored pencil", "marker", "pen and ink", "nib", "quill", "stylus", "digital pen", "tablet",
        "airbrush", "spray gun", "heat gun", "glue gun", "sewing machine", "loom", "spindle", "spinning wheel", "felting needle", "crochet hook",
        "knitting needle", "armature", "plaster", "clay", "polymer clay", "plasticine", "wax", "resin", "silicone", "plaster of paris", "papier-mâché",
        "fiberglass", "casting plaster", "stone", "marble", "granite", "limestone", "alabaster", "soapstone", "wood", "metal", "brass", "copper", "steel",
        "aluminum", "gold leaf", "silver leaf", "foil", "wire", "beads", "gemstones", "fabric", "leather", "yarn", "thread", "fiber", "raffia", "jute",
        "silk", "cotton", "linen", "wool", "film", "camera", "lens", "light box", "darkroom", "enlarger", "scanner", "printer", "3D printer", "laser cutter",
        "router", "drill", "saw", "sandpaper", "polisher", "buffer", "turntable", "studio lighting", "backdrops",
        
        # Digital art & technology
        "digital sculpting", "procedural art", "generative art", "algorithmic art", "AI art", "neural style transfer", "GAN art", "NFT art", "crypto art",
        "blockchain art", "virtual reality art", "VR art", "augmented reality art", "AR art", "mixed reality art", "immersive art", "interactive installation",
        "video mapping", "projection mapping", "holographic art", "laser art", "LED art", "kinetic art", "robotic art", "bioart", "new media art",
        "net art", "software art", "code art", "glitch art", "databending", "circuit bending", "creative coding", "processing", "TouchDesigner", "p5.js",
        "WebGL", "Unity3D", "Unreal Engine", "Blender", "ZBrush", "Maya", "Cinema 4D", "Houdini", "After Effects", "Premiere Pro", "Final Cut Pro",
        "Photoshop", "Illustrator", "InDesign", "Affinity Designer", "Affinity Photo", "Procreate", "Clip Studio Paint", "Krita", "GIMP", "Inkscape",
        "Substance Painter", "Substance Designer", "Marvelous Designer", "Daz3D", "SketchUp", "Rhino", "AutoCAD", "Fusion 360", "Tinkercad",
        "3D scanning", "photogrammetry", "motion capture", "rotoscoping", "chroma key", "green screen", "compositing", "CGI", "VFX", "special effects",
        "pixel art", "voxel art", "low poly", "isometric art", "vector illustration", "raster graphics", "digital drawing", "digital painting", "digital sketching",
        "matte painting", "concept art", "environment design", "character modeling", "rigging", "skinning", "texturing", "UV mapping", "normal mapping",
        "bump mapping", "displacement mapping", "particle systems", "fluid simulation", "cloth simulation", "physics simulation", "shader art", "GLSL",
        
        # Art movements & styles (historical and contemporary)
        "gothic art", "byzantine art", "rococo", "neoclassicism", "pre-raphaelite", "art brut", "outsider art", "folk art", "naive art", "primitivism",
        "symbolism", "fauvism", "orphism", "constructivism", "suprematism", "de stijl", "bauhaus", "art informel", "tachisme", "cobra", "lyrical abstraction",
        "hard-edge painting", "color field painting", "op art", "kinetic art", "land art", "earthworks", "arte povera", "fluxus", "happenings", "body art",
        "neo-dada", "nouveau réalisme", "photorealism", "hyperrealism", "transavantgarde", "neo-expressionism", "bad painting", "neo-geo", "yba",
        "young british artists", "relational aesthetics", "bio art", "post-internet art", "metamodernism", "superflat", "lowbrow art", "pop surrealism",
        "stuckism", "vaporwave aesthetics", "afrofuturism", "cyberpunk", "solarpunk", "steampunk", "dieselpunk", "atompunk", "biopunk", "nanopunk",
        "classical realism", "academic art", "hudson river school", "barbizon school", "macchiaioli", "tonalism", "luminism", "american impressionism",
        "ashcan school", "precisionism", "regionalism", "social realism", "harlem renaissance", "washington color school", "pattern and decoration",
        "superstudio", "memphis group", "deconstructivism", "critical design", "speculative design", "eco art", "tactical media", "new aesthetic",
        "vorticism", "rayonism", "purism", "precisionism", "synchronism", "neo-plasticism", "concrete art", "systems art", "support/surface",
        
        # World art traditions
        "asian art", "african art", "oceanic art", "middle eastern art", "islamic art", "arabic calligraphy", "byzantine iconography", "ukiyo-e",
        "chinese painting", "japanese painting", "korean painting", "indian painting", "persian miniature", "aboriginal art", "maori art", "inuit art",
        "native american art", "mesoamerican art", "andean art", "west african art", "east african art", "egyptian art", "mesopotamian art", "greco-roman art",
        "byzantine mosaic", "ottoman art", "mughal art", "tibetan thangka", "buddhist art", "hindu art", "jain art", "sikh art", "taoist art", "shinto art",
        "russian icon painting", "orthodox iconography", "khmer art", "thai art", "balinese art", "javanese art", "polynesian art", "melanesian art",
        "micronesian art", "navajo weaving", "pueblo pottery", "inuit sculpture", "yoruba sculpture", "dogon sculpture", "benin bronzes", "akan goldweights",
        "haida art", "maori carving", "maori tattoo", "ta moko", "aboriginal dot painting", "aboriginal bark painting", "polynesian tattoo", "samoan tattoo",
        "batik", "kente cloth", "adinkra", "mudcloth", "ikat", "suzani", "rangoli", "kolam", "mehndi", "henna art", "sand painting", "sand mandala",
        "thangka painting", "persian carpet", "turkish carpet", "kilim", "ikat", "kuba cloth", "adire", "shibori", "origami", "ukiyo-e", "sumi-e",
        "chinese calligraphy", "seal carving", "ink wash painting", "scroll painting", "miniature painting", "pottery", "porcelain", "celadon", "blue and white",
        
        # Contemporary art forms & interdisciplinary approaches
        "new media art", "social practice art", "relational aesthetics", "institutional critique", "appropriation art", "art activism", "artivism",
        "eco art", "environmental art", "sustainable art", "upcycled art", "recycled art", "bio art", "transgenic art", "sci-art", "science-art collaboration",
        "art-science", "art and technology", "STEAM", "intermedia", "transdisciplinary art", "interdisciplinary art", "hybridization", "post-studio practice",
        "site-specific installation", "socially engaged art", "community art", "participatory art", "collaborative art", "dialogical art", "interventionist art",
        "guerrilla art", "tactical media", "hacktivism", "culture jamming", "detournement", "psychogeography", "sound art", "noise art", "sonic art",
        "acoustic ecology", "field recording", "experimental music", "sound installation", "sound sculpture", "audiovisual art", "multimedia art",
        "transmedia storytelling", "expanded cinema", "video installation", "performance installation", "live art", "durational performance", "body art",
        "endurance art", "ritual performance", "shamanic performance", "food art", "culinary art", "olfactory art", "scent art", "tactile art", "light art",
        "holography", "kinetic sculpture", "mobile sculpture", "puppetry", "automata", "mechanical art", "robotic performance", "cybernetic art",
        "artificial life art", "living art", "ecological art", "land reclamation art", "remediation art", "activist art", "protest art", "political art",
        "identity-based art", "feminist art", "queer art", "disability art", "outsider art", "prison art", "elder art", "art therapy", "healing art",
        
        # Art education & profession
        "art education", "art pedagogy", "art curriculum", "visual literacy", "art appreciation", "art analysis", "formal analysis", "iconography",
        "semiotics in art", "visual culture", "visual studies", "art semiotics", "art hermeneutics", "art phenomenology", "art criticism", "art writing",
        "art journalism", "art blog", "art vlog", "art podcast", "art lecture", "art workshop", "art residency", "art fellowship", "art grant", "art prize",
        "art competition", "art fair", "art biennale", "art triennale", "art documenta", "commercial gallery", "nonprofit gallery", "alternative space",
        "artist-run space", "art collective", "art studio", "shared studio", "art coworking", "artist colony", "art commune", "art sanctuary", "art retreat",
        "art portfolio", "artist statement", "artist CV", "artist resume", "artist website", "art blog", "art marketing", "art business", "art entrepreneurship",
        "art pricing", "art sales", "art collecting", "art investment", "art financing", "art insurance", "art shipping", "art handling", "art installation",
        "art storage", "art conservation", "art restoration", "preventive conservation", "museum studies", "museology", "curatorial studies", "exhibition design",
        "exhibition curation", "art acquisition", "provenance research", "collection management", "registrar", "preparator", "art technician", "art fabricator",
        "art consultant", "art advisor", "art appraiser", "art authenticator", "art detective", "art law", "copyright in art", "intellectual property in art",
        "art licensing", "art reproduction rights", "creative commons", "public domain art", "open source art", "art commissioning", "public art commission",
        "percent for art", "art patronage", "art philanthropy", "art foundation", "art nonprofit", "art for social change", "community art program",
        "art therapy", "creative therapy", "expressive arts therapy", "art education therapy", "art outreach", "art accessibility", "inclusive art",
        "universal design in art", "assistive technology in art", "adapted art tools", "tactile art", "sensory art", "art for the blind", "audio description in art"
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