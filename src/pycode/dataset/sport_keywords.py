import pandas as pd
import os

def get_sports_keywords():
    """Return a comprehensive DataFrame of sports-related keywords."""
    keywords = [
        # Original general sports
        "football", "basketball", "soccer", "tennis", "walking", "baseball", "golf", "swimming", "volleyball", "hiking",
        "hockey", "rugby", "cricket", "track", "field", "martial arts", "wrestling", "boxing", "cycling",
        "badminton", "table tennis", "skiing", "snowboarding", "surfing", "skateboarding", "fencing", "archery", "rowing", "sailing",
        "climbing", "karate", "judo", "taekwondo", "weightlifting", "powerlifting", "bodybuilding", "crossfit", "pilates", "yoga",
        "gymnastics", "cheerleading", "equestrian", "polo", "lacrosse", "handball", "curling", "biathlon", "triathlon", "pentathlon",
        "motorsport", "karting", "drifting", "endurance racing", "motocross", "freediving", "scuba diving", "kayaking", "canoeing", "snorkeling",
        "paragliding", "skydiving", "bungee jumping", "parkour", "ultimate frisbee", "disc golf", "dodgeball", "kickball", "softball", "paddleboarding",
        "kitesurfing", "windsurfing", "sandboarding", "mountaineering", "orienteering",
        
        # Football specific terms
        "quarterback", "touchdown", "field goal", "tackle", "interception", "fumble", "offensive line", "defensive line", "wide receiver", 
        "running back", "NFL", "Super Bowl", "college football", "fantasy football", "flag football", "american football", "gridiron", 
        "punt", "kicker", "linebacker", "cornerback", "safety", "tight end", "blitz", "sack", "huddle", "pass rush", "end zone",
        "play action", "hail mary", "two-minute drill", "red zone", "first down", "fourth down", "unnecessary roughness",
        
        # Basketball specific terms
        "slam dunk", "three-pointer", "point guard", "shooting guard", "small forward", "power forward", "center", "assist", "rebound",
        "block", "steal", "fast break", "free throw", "NBA", "WNBA", "NCAA basketball", "March Madness", "backcourt", "frontcourt",
        "pick and roll", "alley-oop", "layup", "jump shot", "crossover", "airball", "double dribble", "traveling", "sixth man",
        "shot clock", "triple double", "buzzer beater", "paint", "low post", "high post", "streetball", "and-one", "euro step",
        
        # Soccer specific terms
        "goalkeeper", "striker", "midfielder", "defender", "penalty kick", "free kick", "corner kick", "yellow card", "red card",
        "offside", "header", "bicycle kick", "hat trick", "FIFA", "World Cup", "UEFA", "Champions League", "Premier League", "La Liga",
        "Serie A", "Bundesliga", "MLS", "pitch", "football boots", "footy", "soccer cleats", "VAR", "extra time", "stoppage time",
        "clean sheet", "own goal", "draw", "nil", "futsal", "indoor soccer", "tackle", "wall", "through ball", "chip shot",
        
        # Baseball specific terms
        "pitcher", "catcher", "batter", "infielder", "outfielder", "home run", "strikeout", "base hit", "stolen base", "double play",
        "MLB", "Little League", "batting average", "ERA", "RBI", "bullpen", "dugout", "bunt", "grand slam", "curveball", "fastball",
        "slider", "changeup", "knuckleball", "ballpark", "mound", "diamond", "inning", "at bat", "on deck", "designated hitter",
        "pinch hitter", "walk-off", "perfect game", "no-hitter", "sacrifice fly", "batting cage", "batting gloves", "baseball cleats",
        
        # Tennis specific terms
        "serve", "forehand", "backhand", "volley", "smash", "deuce", "advantage", "love", "Grand Slam", "Wimbledon", "US Open",
        "French Open", "Australian Open", "ATP", "WTA", "baseline", "net", "let", "ace", "fault", "double fault", "drop shot",
        "lob", "slice", "topspin", "tennis racquet", "tennis court", "clay court", "grass court", "hard court", "tennis ball",
        "match point", "set point", "break point", "tie break", "singles", "doubles", "mixed doubles", "wildcard", "rally",
        
        # Swimming specific terms
        "freestyle", "backstroke", "breaststroke", "butterfly", "medley", "lap", "lane", "flip turn", "diving", "Olympic swimming",
        "swim cap", "goggles", "swim fins", "kickboard", "pull buoy", "touch pad", "false start", "swimming pool", "open water swimming",
        "FINA", "swim meet", "relay", "individual medley", "split time", "streamline", "drafting", "tumble turn", "swim trunks",
        "wetsuit", "water aerobics", "aqua jogging", "swim spa", "triathlon swimming", "swimming drill", "swimwear",
        
        # Golf specific terms
        "tee", "fairway", "green", "rough", "bunker", "hazard", "par", "birdie", "eagle", "bogey", "PGA", "LPGA", "Masters",
        "US Open", "British Open", "golf club", "driver", "iron", "wedge", "putter", "golf ball", "tee time", "mulligan",
        "handicap", "caddie", "golf cart", "putting green", "golf course", "driving range", "hole-in-one", "fore", "golf swing",
        "golf lessons", "golf glove", "golf shoes", "miniature golf", "disc golf", "pitch shot", "chip shot", "draw", "fade",
        
        # Cycling specific terms
        "road cycling", "mountain biking", "BMX", "track cycling", "bicycle", "cycling helmet", "cycling jersey", "cycling shorts",
        "peloton", "paceline", "Tour de France", "Giro d'Italia", "Vuelta a España", "time trial", "criterium", "road race",
        "fixie", "single-speed", "velodrome", "cycling shoes", "clipless pedals", "drafting", "breakaway", "sprint", "climb",
        "descent", "bicycle chain", "bicycle frame", "derailleur", "groupset", "bike fit", "bicycle pump", "puncture", "flat tire",
        
        # Winter sports specific terms
        "alpine skiing", "cross-country skiing", "ski jumping", "snowboarding", "figure skating", "speed skating", "ice hockey",
        "bobsled", "luge", "skeleton", "curling", "Winter Olympics", "ski resort", "ski lift", "chairlift", "gondola", "ski poles",
        "ski boots", "ski bindings", "moguls", "slalom", "downhill", "super-G", "half-pipe", "freestyle skiing", "ski jumps",
        "ice rink", "ice skating", "snowshoes", "snowshoeing", "biathlon", "avalanche", "snow conditions", "winter sports",
        
        # Water sports specific terms
        "surfing", "windsurfing", "kitesurfing", "wakeboarding", "water skiing", "jet skiing", "scuba diving", "snorkeling",
        "freediving", "sailing", "yachting", "kayaking", "canoeing", "whitewater rafting", "paddleboarding", "stand-up paddleboarding",
        "bodyboarding", "synchronized swimming", "water polo", "cliff diving", "deep sea diving", "spearfishing", "aquathlon",
        "fishing", "fly fishing", "angling", "dock diving", "coasteering", "open water swimming", "tubing", "rip current",
        
        # Combat sports specific terms
        "boxing", "MMA", "UFC", "karate", "judo", "taekwondo", "wrestling", "jiu-jitsu", "muay thai", "kickboxing", "sumo",
        "fencing", "heavyweight", "lightweight", "welterweight", "knockout", "submission", "title fight", "bout", "sparring",
        "punching bag", "boxing gloves", "boxing ring", "octagon", "tatami", "martial arts dojo", "martial arts gi", "black belt",
        "belt ranking", "kata", "kumite", "takedown", "grappling", "striking", "self-defense", "combat training", "krav maga",
        
        # Athletic events
        "Olympics", "Summer Olympics", "Winter Olympics", "Paralympic Games", "Commonwealth Games", "Pan American Games",
        "Asian Games", "X Games", "Extreme Games", "World Championships", "National Championships", "marathon", "half marathon",
        "5K run", "10K run", "fun run", "track meet", "field day", "competition", "tournament", "match", "game", "race", "medal",
        "gold medal", "silver medal", "bronze medal", "qualifying round", "finals", "semifinals", "quarterfinals", "opening ceremony",
        
        # Sports equipment
        "sports equipment", "jersey", "uniform", "cleats", "sneakers", "athletic shoes", "helmet", "protective gear", "mouthguard",
        "shin guards", "knee pads", "elbow pads", "gloves", "balls", "racquet", "bat", "club", "stick", "goal", "net", "hoop",
        "basket", "scoreboard", "whistle", "stopwatch", "sports bag", "water bottle", "sports drink", "energy gel", "mats",
        "weight bench", "dumbbells", "barbells", "kettlebells", "resistance bands", "foam roller", "sports bra", "compression wear",
        
        # Sports venues
        "stadium", "arena", "court", "field", "pitch", "track", "pool", "gymnasium", "gym", "fitness center", "sports complex",
        "training facility", "locker room", "dugout", "sideline", "press box", "luxury box", "grandstand", "bleachers", "concessions",
        "press room", "weight room", "clubhouse", "paddock", "bowling alley", "skate park", "climbing wall", "ice rink", "velodrome",
        "sports hall", "cricket pitch", "tennis court", "basketball court", "volleyball court", "football field", "baseball diamond",
        
        # Fitness and training
        "fitness", "training", "workout", "exercise", "cardio", "strength training", "aerobics", "HIIT", "circuit training",
        "interval training", "stretching", "flexibility", "agility", "balance", "coordination", "endurance", "stamina", "conditioning",
        "pre-season", "off-season", "training camp", "sports medicine", "athletic trainer", "coach", "personal trainer", "physiotherapy",
        "recovery", "rest day", "deload week", "periodization", "sports nutrition", "hydration", "warm-up", "cool-down", "foam rolling",
        
        # Sports roles and professionals
        "athlete", "amateur", "professional", "semi-professional", "coach", "manager", "trainer", "referee", "umpire", "judge",
        "scorekeeper", "timekeeper", "sports agent", "sports analyst", "commentator", "broadcaster", "sports journalist",
        "sports photographer", "sports doctor", "physiotherapist", "team captain", "MVP", "rookie", "veteran", "all-star",
        "hall of fame", "sports scout", "sports psychologist", "nutritionist", "athletics director", "sports commissioner",
        
        # Sports organizations
        "sports league", "sports federation", "sports association", "sports club", "team", "franchise", "NCAA", "IOC", "FIFA", "NFL",
        "NBA", "MLB", "NHL", "ATP", "WTA", "PGA", "LPGA", "NASCAR", "Formula 1", "UFC", "sports commission", "anti-doping agency",
        "WADA", "sports ministry", "sports governance", "sports regulation", "rules committee", "drafting", "trading", "free agency",
        "salary cap", "sports contract", "sports sponsorship", "sports endorsement", "sports marketing", "sports broadcasting",
        
        # Emerging and niche sports
        "esports", "gaming", "Fortnite", "League of Legends", "Dota", "Overwatch", "competitive gaming", "drone racing", "chess boxing",
        "quidditch", "sepak takraw", "kabaddi", "hurling", "gaelic football", "aussie rules football", "pesäpallo", "bandy",
        "floorball", "korfball", "netball", "tchoukball", "underwater hockey", "underwater rugby", "roller derby", "competitive eating",
        "wife carrying", "toe wrestling", "extreme ironing", "caber toss", "strongman", "CrossFit Games", "obstacle course racing", "Spartan Race",
        
        # Sport psychology and concepts
        "sportsmanship", "fair play", "team spirit", "camaraderie", "motivation", "discipline", "mental toughness", "performance anxiety",
        "visualization", "flow state", "competitive spirit", "rivalry", "underdog", "clutch performance", "choke", "comeback",
        "upset victory", "athletic identity", "burnout", "overtraining", "sports psychology", "team building", "leadership", "goal setting",
        "performance enhancement", "mindfulness", "confidence", "focus", "concentration", "pressure", "sports ethic", "team chemistry",
        
        # Sports fan culture
        "sports fan", "supporter", "spectator", "home team", "away team", "rivalry", "derby", "sports bar", "tailgating", "face paint",
        "foam finger", "jersey", "replica kit", "memorabilia", "autograph", "sports collection", "sports card", "fantasy sports",
        "sports betting", "odds", "point spread", "over/under", "prop bet", "bracket", "pool", "draft", "sports trivia", "sports history",
        "championship parade", "bandwagon fan", "die-hard fan", "ultras", "hooliganism", "sports chant", "wave", "jumbotron",
        
        # Sports media and entertainment
        "sports broadcast", "play-by-play", "color commentary", "sports highlight", "instant replay", "sports documentary",
        "sports film", "sports biography", "sports autobiography", "sports novel", "sports podcast", "sports radio", "sports television",
        "sports channel", "sports streaming", "sports website", "sports app", "sports social media", "sports journalism", "sports magazine",
        "sports photography", "sports analytics", "sports statistics", "sports data", "sports technology", "e-sports", "sports video game",
        
        # Health and medical aspects
        "sports injury", "sprain", "strain", "fracture", "concussion", "ACL tear", "tendonitis", "shin splints", "plantar fasciitis",
        "tennis elbow", "golfer's elbow", "runner's knee", "sports medicine", "physical therapy", "rehabilitation", "sports massage",
        "athletic trainer", "first aid", "AED", "CPR", "taping", "bracing", "orthotics", "sports nutrition", "hydration", "electrolytes",
        "carbohydrate loading", "protein intake", "sports supplement", "recovery drink", "performance enhancing drug", "doping", "drug testing",
        
        # Sports technology
        "sports technology", "wearable tech", "fitness tracker", "heart rate monitor", "GPS tracker", "smart clothing", "performance analytics",
        "sports statistics", "video analysis", "biomechanics", "motion capture", "virtual reality training", "augmented reality",
        "sports simulation", "smart equipment", "sensor technology", "electronic scoring", "hawk-eye", "goal-line technology", "VAR",
        "instant replay", "timing systems", "photo finish", "starting blocks", "electronic target", "sports robotics", "AI coaching",
        
        # Sports business and economics
        "sports business", "sports industry", "sports economy", "sports marketing", "sports sponsorship", "sports endorsement",
        "athlete brand", "sports merchandise", "ticket sales", "season ticket", "box office", "gate revenue", "broadcasting rights",
        "media rights", "sports agent", "contract negotiation", "salary cap", "luxury tax", "revenue sharing", "collective bargaining",
        "sports franchise value", "sports investment", "sports venture capital", "sports startup", "sports innovation", "sports tourism",
        
        # Sports in education
        "physical education", "PE class", "school sports", "varsity sports", "junior varsity", "intramural sports", "recess games",
        "college sports", "NCAA", "athletic scholarship", "recruiting", "sports academy", "sports boarding school", "coaching certification",
        "referee certification", "sports degree", "sports science", "exercise physiology", "kinesiology", "biomechanics", "sports law",
        "sports management", "youth sports", "Little League", "youth development", "physical literacy", "motor skills", "coordination games",
        
        # Recreational sports
        "recreational sports", "pickup game", "casual sports", "backyard sports", "beach sports", "beer league", "amateur league",
        "community sports", "sports club", "social sports", "co-ed sports", "senior sports", "masters competition", "sports vacation",
        "sports tourism", "adventure sports", "outdoor recreation", "leisure activity", "family sports", "games", "sports day",
        "field day", "sports festival", "sports carnival", "fun run", "charity sports event", "sports fundraiser", "company sports day",
        
        # Adaptive and inclusive sports
        "adaptive sports", "paralympic sports", "special olympics", "inclusive sports", "wheelchair sports", "wheelchair basketball",
        "wheelchair tennis", "wheelchair rugby", "blind football", "goalball", "sitting volleyball", "amputee sports", "prosthetic technology",
        "adaptive equipment", "guide running", "para-swimming", "para-athletics", "para-cycling", "boccia", "unified sports",
        "therapeutic recreation", "rehabilitation sports", "sports therapy", "adaptive skiing", "adaptive surfing", "inclusive programming",
        
        # Extreme sports
        "extreme sports", "action sports", "adventure sports", "adrenaline sports", "risk sports", "X Games", "skateboarding", "BMX",
        "motocross", "freestyle motocross", "mountain biking", "downhill mountain biking", "rock climbing", "free climbing", "ice climbing",
        "bouldering", "parkour", "freerunning", "BASE jumping", "wingsuit flying", "skydiving", "bungee jumping", "cliff diving",
        "highlining", "slacklining", "kiteboarding", "big wave surfing", "whitewater kayaking", "free skiing", "snowboarding", "snowmobile freestyle",
        
        # Team dynamics
        "team building", "team chemistry", "team cohesion", "team dynamics", "team spirit", "teamwork", "locker room culture",
        "team leadership", "captain", "co-captain", "assistant captain", "team meeting", "huddle", "pep talk", "team strategy",
        "game plan", "playbook", "scouting report", "film study", "team practice", "scrimmage", "training camp", "team retreat",
        "team bonding", "team rules", "team culture", "team tradition", "team ritual", "team celebration", "team photo", "team bus",
        
        # Sports strategy and tactics
        "sports strategy", "tactics", "game plan", "playbook", "formation", "set play", "offensive strategy", "defensive strategy",
        "counter-attack", "pressing", "zone defense", "man-to-man defense", "full-court press", "offensive scheme", "defensive scheme",
        "special teams", "time management", "clock management", "substitution pattern", "rotation", "line change", "match-up",
        "double team", "pick and roll", "isolation play", "motion offense", "triangle offense", "spread offense", "option play",
        
        # Sports governance and ethics
        "sports governance", "sports ethics", "fair play", "sportsmanship", "integrity", "rules", "regulation", "sports law",
        "anti-doping", "performance enhancing drugs", "drug testing", "WADA", "match fixing", "sports corruption", "bribery",
        "sports betting scandal", "eligibility rules", "age requirement", "nationality requirement", "transfer rules", "trade regulations",
        "salary cap", "financial fair play", "gender verification", "transgender athlete policy", "sports court", "arbitration",
        
        # Sports and society
        "sports culture", "sports history", "sports heritage", "sports tradition", "national sport", "sports identity", "sports nationalism",
        "sports diplomacy", "Olympic truce", "sports boycott", "sports sanctions", "sports protest", "athlete activism", "social justice",
        "equal pay", "gender equality", "racial equality", "inclusion", "diversity", "accessibility", "sports development", "grassroots sports",
        "community sports", "sports for development", "sports for peace", "sports charity", "sports philanthropy", "athlete foundation"
    ]

    # Creating the DataFrame with unique values
    df = pd.DataFrame({"keyword": keywords, "category": "sports"})
    df = df.drop_duplicates(subset=["keyword"])
    return df

def save_sports_keywords(output_file="sports_keywords.parquet"):
    """Save sports keywords to a parquet file. Append if the file already exists."""
    new_df = get_sports_keywords()
    
    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique sports keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique sports keywords to {output_file}")
    
    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_sports_keywords()