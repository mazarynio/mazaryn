import pandas as pd
import os

def get_travel_keywords():
    """Return a DataFrame of travel-related keywords."""
    keywords = [
        "travel", "tourism", "vacation", "adventure", "road trip", "backpacking", "sightseeing", "cruise", "flight", "airlines",
        "hotel", "resort", "hostel", "camping", "glamping", "travel photography", "travel blogging", "cultural tourism", "ecotourism", "luxury travel",
        "budget travel", "solo travel", "family trip", "group tours", "city break", "beach holiday", "mountain climbing", "national parks", "historical sites", "world heritage",
        "travel insurance", "visa application", "travel guide", "itinerary", "tourist attractions", "local cuisine", "travel tips", "packing list", "travel apps", "language barriers",
        "adventure sports", "hiking trails", "scuba diving", "safari", "road maps", "eco-friendly travel", "digital nomad", "remote work travel", "staycations", "glamping sites",
        "wellness tourism", "sustainable tourism", "food tourism", "adventure tourism", "nature tourism", "volunteer travel", "medical tourism", "luxury cruises", "destination wedding",
        "family-friendly destinations", "honeymoon destinations", "off-the-beaten-path travel", "local experiences", "travel concierge", "personalized travel", "slow travel",
        "jet lag recovery", "travel gear", "travel safety", "cultural exchange", "workcations", "short trips", "weekend getaway", "travel bloggers", "travel influencers", "luxury hotels",
        "boutique hotels", "hotel reviews", "holiday homes", "vacation rentals", "digital detox", "traveler's health", "bucket list destinations", "volcano tourism", "snowboarding",
        "ski resorts", "wine tourism", "adventure parks", "tour packages", "private tours", "luxury safari", "glamping experiences", "group travel tips", "destination travel",
        "airline loyalty programs", "travel hacks", "local festivals", "destination guides", "travel podcasts", "cultural immersion", "authentic experiences",
        
        "all-inclusive resorts", "amusement parks", "ancient ruins", "architectural tours", "art galleries", "aurora viewing", "baggage allowance", "ballooning", "beach resorts", "beach vacations",
        "birdwatching tours", "boat tours", "booking platforms", "border crossing", "botanical gardens", "business class", "business travel", "cabin rentals", "camping equipment", "canal cruises",
        "car rental", "carry-on essentials", "castles", "cave exploration", "cenotes", "chartered flights", "château stays", "city tours", "coastal drives", "coastal towns",
        "coffee tourism", "cruise deals", "cruise destinations", "cruise ships", "culinary classes", "culinary destinations", "cultural attractions", "cultural festivals", "currency exchange", "customs regulations",
        "cycle touring", "dark tourism", "day trips", "desert expeditions", "desert safari", "destination management", "destination marketing", "domestic flights", "drone photography", "duty-free shopping",
        "early bird deals", "ecoresorts", "emergency travel", "exchange rates", "expedition cruises", "expedition planning", "family resorts", "farm stays", "fashion tourism", "festival tourism",
        "film location tours", "first class travel", "fishing trips", "fitness retreats", "fjord cruises", "flashpacking", "flight booking", "flight comparison", "flight deals", "flight upgrades",
        "food trails", "foreign exchange", "forest bathing", "garden tours", "gastronomy tours", "genealogy tourism", "ghost tours", "glacier hiking", "glacier tours", "global entry",
        "green tourism", "guided hikes", "guided tours", "hammock camping", "hand luggage", "heritage railways", "heritage sites", "heritage tours", "heritage trails", "hiking destinations",
        "hiking gear", "historical landmarks", "historical tours", "history tourism", "homestays", "hot air ballooning", "hot springs", "hotel amenities", "hotel booking", "hotel chains",
        "hotel rewards", "hunting tourism", "ice climbing", "ice hotels", "iconic landmarks", "indigenous tourism", "indoor attractions", "industrial tourism", "infinity pools", "international driving permit",
        "international flights", "island hopping", "island resorts", "kayaking adventures", "lake destinations", "lakeside retreats", "language learning travel", "last minute bookings", "last minute deals", "layover activities",
        "lighthouse tours", "literary tourism", "local accommodations", "local events", "local guides", "local markets", "local transportation", "long-distance hiking", "long-haul flights", "long-term travel",
        "low-cost airlines", "low-season travel", "luggage storage", "luggage tags", "luxury accommodations", "luxury camping", "luxury experiences", "luxury transportation", "luxury travel agents", "luxury villas",
        "marina resorts", "marine conservation", "marine parks", "maritime museums", "market tours", "meditation retreats", "memorials", "microadventures", "military tourism", "minimalist travel",
        "monastery stays", "monument visits", "motorcycle touring", "mountain biking", "mountain lodges", "mountain resorts", "multi-city trips", "multi-country tours", "museums", "musical tourism",
        "national monuments", "nature photography", "nature reserves", "nature walks", "night markets", "night safaris", "nomadic lifestyle", "northern lights", "ocean cruises", "ocean views",
        "oceanfront properties", "off-grid travel", "off-road adventures", "off-season travel", "olympic tourism", "online check-in", "online travel agencies", "outdoor activities", "outdoor adventures", "overland travel",
        "overwater bungalows", "package holidays", "paddleboarding", "palaces", "paragliding", "passport application", "passport renewal", "passport requirements", "pet-friendly accommodations", "pet-friendly travel",
        "photography tours", "pilgrimages", "pilgrimage routes", "polar expeditions", "port cities", "premium economy", "private islands", "private jets", "public transportation", "railway journeys",
        "rainforest expeditions", "recreational vehicles", "reef diving", "responsible tourism", "restaurant week", "restaurant guides", "return flights", "river cruises", "road trip planner", "rock climbing",
        "romantic destinations", "romantic getaways", "rural tourism", "RV camping", "RV rentals", "sailing trips", "sailing vacations", "scenic byways", "scenic drives", "scenic flights",
        "scenic railways", "scenic routes", "scooter rentals", "seaside towns", "seasonal destinations", "seasonal tourism", "seat selection", "senior travel", "short cruises", "short-term rentals",
        "shuttle services", "skydiving experiences", "snorkeling spots", "snow activities", "snow destinations", "snowmobile tours", "spa destinations", "spa hotels", "spa resorts", "spa retreats",
        "space tourism", "spiritual retreats", "sports tourism", "spring break destinations", "stargazing tours", "state parks", "street food tours", "student travel", "submarine tours", "summer destinations",
        "summit hikes", "sunbathing", "sunset cruises", "sustainable accommodations", "swimming holes", "swimwear", "temple visits", "tent camping", "thermal baths", "theme parks",
        "time zone adjustments", "tour guides", "tourist information centers", "tourist maps", "tourist visas", "township tourism", "traditional crafts", "train journeys", "train passes", "train stations",
        "train travel", "transit visas", "travel accessories", "travel adapters", "travel agents", "travel books", "travel budgeting", "travel clothing", "travel communities", "travel cuisines",
        "travel discounts", "travel documentaries", "travel emergencies", "travel etiquette", "travel experiences", "travel forums", "travel gifts", "travel inspiration", "travel insurance claims", "travel itineraries",
        "travel journals", "travel literature", "travel medicine", "travel memberships", "travel memories", "travel nursing", "travel photography tips", "travel pillow", "travel planning", "travel rewards",
        "travel seasons", "travel sickness", "travel souvenirs", "travel startups", "travel statistics", "travel stories", "travel tech", "travel toiletries", "travel trends", "travel vloggers",
        "travel vlogs", "travel vouchers", "travel websites", "travel writers", "travel writing", "travel yoga", "traveler's checks", "trekking destinations", "trekking equipment", "trekking permits",
        "trekking tours", "trip cancellation", "trip extensions", "trip insurance", "trip planning", "tropical destinations", "tropical resorts", "underwater hotels", "underwater photography", "underwater restaurants",
        "unique accommodations", "universal adapters", "urban adventures", "urban exploration", "urban hiking", "urban tourism", "valid passport", "van life", "van travel", "visa extensions",
        "visa on arrival", "visa requirements", "visa services", "visa waiver", "visitor centers", "volcano hiking", "voluntourism", "waterfall hikes", "waterfront properties", "weather forecasts",
        "wedding destinations", "wedding tourism", "wellness resorts", "whale watching", "white water rafting", "wildlife encounters", "wildlife photography", "wildlife reserves", "wildlife safaris", "wildlife sanctuaries",
        "wildlife tours", "wine regions", "wine routes", "wine tastings", "winter activities", "winter destinations", "winter resorts", "winter sports", "winter vacations", "working holiday",
        "world cruises", "world tours", "yacht charter", "yacht cruises", "yachting destinations", "yoga destinations", "yoga retreats", "youth hostels", "youth travel", "zip lining",
        "accessible travel", "adult-only resorts", "agritourism", "airport lounges", "airport parking", "airport shuttles", "airport transfers", "alpine experiences", "animal encounters", "animal safaris",
        "annual leave travel", "archeological sites", "archipelago tours", "art destinations", "art tourism", "astrotourism", "audio guides", "autumn colors", "autumn destinations", "ayurveda retreats",
        "backpacker hostels", "bareboat charters", "barrier reef diving", "battlefield tours", "bay cruises", "best travel time", "bicycle rentals", "bicycle tours", "bike-friendly cities", "biking trails",
        "black sand beaches", "bioluminescent experiences", "bleisure travel", "boardwalk destinations", "boat harbors", "boat rentals", "botanical tourism", "brewery tours", "bridge climbing", "buddhist temples",
        "business accommodations", "business visa", "cabin crew", "campervans", "camping permits", "canal tours", "canyoning adventures", "carbon offset travel", "caravan parks", "caravanning",
        "carbonate beaches", "carnival destinations", "castle tours", "caving adventures", "cemetery tourism", "charter flights", "charter tours", "children's activities", "church visits", "city passes",
        "cliff diving", "climate considerations", "coastal hikes", "coastal paths", "colonial architecture", "comfort travel", "commercial flights", "commuter flights", "concert tourism", "concierge services",
        "conference destinations", "congress tourism", "connecting flights", "contiki tours", "coral reefs", "cottage rentals", "country hopping", "countryside tours", "couples destinations", "couples retreats",
        "coworking retreats", "craft beer tourism", "craft tourism", "crater lakes", "creative tourism", "cruise excursions", "cruise packages", "cuisine routes", "cultural centers", "cultural differences",
        "cultural diversity", "cultural heritage", "cultural insights", "cultural landmarks", "cultural practices", "cultural sensitivity", "cultural sites", "cultural traits", "cultural understanding", "culture shock",
        "curated experiences", "custom itineraries", "destination experts", "destination retreats", "destination spas", "detox retreats", "digital travel planning"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "travel"})
    df = df.drop_duplicates(subset=["keyword"])  
    return df

def save_travel_keywords(output_file="travel_keywords.parquet"):
    """Save travel keywords to a parquet file. Append if the file already exists."""
    new_df = get_travel_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique travel keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique travel keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_travel_keywords()