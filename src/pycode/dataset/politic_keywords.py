import pandas as pd
import os

def get_politics_keywords():
    """Return a DataFrame of politics-related keywords (expanded to ~700 terms)."""
    keywords = [
        # Original keywords
        "democracy", "election", "voting", "parliament", "congress", "senate", "legislation", "policy", "political party",
        "campaign", "ballot", "constitution", "government", "candidate", "politician", "debate", "diplomacy", "foreign policy",
        "domestic policy", "geopolitics", "activism", "lobbying", "referendum", "constituency", "political science",
        "electoral college", "bipartisan", "filibuster", "gerrymandering", "polling", "propaganda", "political spectrum",
        "socialism", "capitalism", "communism", "liberalism", "conservatism", "populism", "nationalism", "globalism",
        "political reform", "legislation", "judiciary", "executive branch", "bill of rights", "political corruption",
        "protest", "civil liberties", "political asylum", "impeachment", "veto", "press briefing", "inauguration",
        "political strategy", "political commentary", "political analysis", "sanctions", "political ideology", "political discourse",
        
        # Political systems and theories
        "direct democracy", "representative democracy", "parliamentary democracy", "constitutional monarchy", "absolute monarchy",
        "oligarchy", "aristocracy", "autocracy", "theocracy", "dictatorship", "totalitarianism", "authoritarianism", "fascism",
        "anarchism", "libertarianism", "social democracy", "democratic socialism", "marxism", "leninism", "maoism", "trotskyism",
        "neo-liberalism", "neo-conservatism", "environmentalism", "feminism", "progressivism", "neo-fascism", "technocracy",
        "meritocracy", "plutocracy", "kleptocracy", "kakistocracy", "monarchy", "neo-marxism", "federalism", "confederalism",
        "republicanism", "constitutionalism", "egalitarianism", "isolationism", "interventionism", "unitary state", "federal state",
        "syndicalism", "anarcho-capitalism", "anarcho-syndicalism", "minarchism", "distributism", "third way", "guild socialism",
        "social liberalism", "classical liberalism", "green politics", "eco-socialism", "agrarianism", "confucianism", 
        "corporatism", "welfare state", "laissez-faire", "mercantilism", "keynesianism", "monetarism", "supply-side economics",
        "state capitalism", "crony capitalism", "neocolonialism", "post-colonialism", "secularism", "theocracy",
        
        # Political institutions and structures
        "house of representatives", "supreme court", "cabinet", "ministry", "department of state", "treasury", "defense department",
        "justice department", "white house", "downing street", "kremlin", "european union", "united nations", "world trade organization",
        "international monetary fund", "world bank", "security council", "general assembly", "european commission", "european parliament",
        "nato", "g7", "g20", "commonwealth", "asean", "nafta", "federal reserve", "central bank", "state department", "pentagon",
        "civil service", "bureaucracy", "diplomatic corps", "embassy", "consulate", "diplomatic mission", "intelligence agency",
        "federal bureau of investigation", "central intelligence agency", "national security agency", "federal government",
        "state government", "municipal government", "local government", "county government", "provincial government",
        "intergovernmental organization", "non-governmental organization", "multilateral institution", "think tank",
        "political action committee", "interest group", "political foundation", "parliamentary committee", "judicial review",
        "constitutional court", "appellate court", "district court", "state court", "regulatory agency", "oversight committee",
        "intelligence committee", "armed services committee", "judiciary committee", "ways and means committee",
        "appropriations committee", "ethics committee", "rules committee", "committee hearing", "house rules", "parliamentary procedure",
        
        # Electoral systems and processes
        "first-past-the-post", "proportional representation", "mixed-member proportional", "alternative vote", "ranked-choice voting",
        "single transferable vote", "party-list proportional representation", "bloc voting", "cumulative voting", "approval voting",
        "primary election", "caucus", "general election", "by-election", "special election", "midterm election", "runoff election",
        "plurality voting", "majority voting", "electoral system", "voter registration", "voter turnout", "voter suppression",
        "voter fraud", "voter id", "absentee voting", "mail-in voting", "early voting", "electronic voting", "voting machine",
        "ballot initiative", "ballot measure", "recall election", "vote counting", "exit poll", "opinion poll", "political poll",
        "polling station", "voter demographics", "swing voter", "base voter", "independent voter", "undecided voter",
        "voter mobilization", "get out the vote", "voter education", "electoral fraud", "ballot stuffing", "vote buying",
        "vote rigging", "election monitoring", "election observer", "election commission", "electoral tribunal", "electoral reform",
        "redistricting", "reapportionment", "electronic voting", "vote tabulation", "ballot design", "butterfly ballot",
        "hanging chad", "provisional ballot", "spoiled ballot", "ballot recount", "electoral integrity", "voter roll",
        "disenfranchisement", "voter purge", "voter eligibility", "voter verification", "electoral campaign",
        
        # Political parties and movements
        "political coalition", "political alliance", "political bloc", "political movement", "political organization",
        "political faction", "political wing", "political fringe", "mainstream party", "third party", "major party", "minor party",
        "opposition party", "ruling party", "incumbent party", "party platform", "party manifesto", "party convention",
        "party primary", "party caucus", "party discipline", "party whip", "party leader", "party secretary", "party chair",
        "party headquarters", "party membership", "party donor", "grassroots movement", "political base", "political establishment",
        "political machine", "political dynasty", "political elite", "political class", "political endorsement", "partisan politics",
        "political polarization", "political realignment", "political dealignment", "political fragmentation", "political unity",
        "political consensus", "political compromise", "political gridlock", "political deadlock", "political stalemate",
        "political capital", "political liability", "political fallout", "political backlash", "political turmoil",
        "political upheaval", "political revolution", "political evolution", "political reformation", "political restoration",
        "political reconciliation", "political transition", "political settlement", "political accommodation", "partisan divide",
        "party switching", "party loyalty", "party politics", "party funding", "party organization", "party structure",
        
        # Political offices and positions
        "president", "prime minister", "chancellor", "monarch", "emperor", "king", "queen", "governor", "mayor", "premier",
        "minister", "secretary of state", "attorney general", "speaker of the house", "majority leader", "minority leader",
        "house whip", "senate whip", "chief of staff", "ambassador", "consul", "envoy", "diplomat", "press secretary",
        "communications director", "policy advisor", "political appointee", "cabinet secretary", "white house counsel",
        "national security advisor", "economic advisor", "chairman of the joint chiefs", "administrator", "commissioner",
        "director", "chief executive", "deputy director", "undersecretary", "assistant secretary", "deputy minister",
        "lieutenant governor", "vice president", "vice premier", "crown prince", "crown princess", "head of state",
        "head of government", "commander-in-chief", "chief diplomat", "chief legislator", "party secretary", "first minister",
        "deputy prime minister", "leader of the opposition", "shadow cabinet", "shadow minister", "president pro tempore",
        "solicitor general", "comptroller", "treasurer", "ombudsman", "chief justice", "associate justice", "parliamentarian",
        "presiding officer", "floor leader", "political officer", "resident commissioner", "delegate", "political director",
        
        # Political activities and processes
        "lawmaking", "policymaking", "decision-making", "governance", "adjudication", "enforcement", "regulation", "administration",
        "deliberation", "negotiation", "mediation", "arbitration", "conflict resolution", "consensus building", "coalition building",
        "political campaigning", "political fundraising", "political canvassing", "political advertising", "political messaging",
        "political strategy", "political tactics", "political operation", "political maneuver", "political positioning",
        "political posturing", "political rhetoric", "political speech", "political debate", "political discourse", "political dialogue",
        "political communication", "political persuasion", "political propaganda", "political spin", "political optics",
        "political framing", "political narrative", "political symbolism", "political theater", "political stunt",
        "political protest", "political demonstration", "political rally", "political march", "political riot", "political violence",
        "political terrorism", "political assassination", "political imprisonment", "political exile", "political asylum",
        "political refugee", "political defection", "political purge", "political persecution", "political repression",
        "political oppression", "political censorship", "political surveillance", "political intimidation", "political retaliation",
        "political targeting", "political discrimination", "political bias", "political favoritism", "political nepotism",
        "political cronyism", "political clientelism", "political patronage", "political spoils system", "political scandal",
        
        # Political documents and legal concepts
        "magna carta", "declaration of independence", "bill of rights", "universal declaration of human rights", "geneva convention",
        "treaty", "accord", "agreement", "convention", "protocol", "memorandum of understanding", "executive order", "presidential directive",
        "executive agreement", "signing statement", "legislative act", "statutory law", "common law", "constitutional law",
        "international law", "humanitarian law", "criminal law", "civil law", "administrative law", "public law", "private law",
        "case law", "precedent", "jurisdiction", "sovereignty", "territorial integrity", "national sovereignty", "self-determination",
        "extradition", "diplomatic immunity", "parliamentary immunity", "parliamentary privilege", "executive privilege",
        "state secret", "classified information", "freedom of information", "transparency", "accountability", "rule of law",
        "separation of powers", "checks and balances", "judicial review", "constitutional review", "legal challenge",
        "constitutional challenge", "legal precedent", "legal doctrine", "legal principle", "legal standard", "legal threshold",
        "burden of proof", "preponderance of evidence", "beyond reasonable doubt", "legal standing", "legal jurisdiction",
        "constitutional interpretation", "originalism", "textualism", "living constitution", "judicial activism", "judicial restraint",
        
        # Political issues and policy areas
        "healthcare policy", "education policy", "economic policy", "fiscal policy", "monetary policy", "tax policy", "trade policy",
        "immigration policy", "refugee policy", "border security", "national security", "homeland security", "defense policy",
        "military policy", "veterans affairs", "foreign aid", "development assistance", "humanitarian aid", "climate policy",
        "environmental policy", "energy policy", "agricultural policy", "industrial policy", "labor policy", "housing policy",
        "transportation policy", "infrastructure policy", "urban policy", "rural policy", "social policy", "welfare policy",
        "poverty reduction", "income inequality", "wealth distribution", "social security", "pension policy", "disability policy",
        "public health", "mental health policy", "drug policy", "substance abuse policy", "criminal justice reform", "police reform",
        "prison reform", "gun policy", "abortion policy", "reproductive rights", "lgbtq rights", "civil rights", "voting rights",
        "minority rights", "indigenous rights", "women's rights", "children's rights", "elder care policy", "consumer protection",
        "financial regulation", "banking regulation", "securities regulation", "antitrust policy", "corporate governance",
        "intellectual property", "digital policy", "technology policy", "internet governance", "data privacy", "cybersecurity",
        "science policy", "research funding", "space policy", "telecommunications policy", "media policy", "cultural policy",
        
        # Political terminology and concepts
        "realpolitik", "raison d'état", "soft power", "hard power", "smart power", "balance of power", "power vacuum",
        "spheres of influence", "deterrence", "containment", "détente", "appeasement", "brinkmanship", "chicken game",
        "domino theory", "hegemony", "imperialism", "colonialism", "decolonization", "self-determination", "sovereignty",
        "statecraft", "power politics", "gunboat diplomacy", "dollar diplomacy", "shuttle diplomacy", "public diplomacy",
        "cultural diplomacy", "summit diplomacy", "track one diplomacy", "track two diplomacy", "backdoor diplomacy",
        "secret diplomacy", "open diplomacy", "preventive diplomacy", "coercive diplomacy", "economic statecraft",
        "sanctions regime", "embargoes", "blockade", "strategic partnership", "special relationship", "client state",
        "proxy war", "buffer state", "failed state", "rogue state", "pariah state", "nation building", "regime change",
        "color revolution", "peaceful transition", "peaceful transfer of power", "deep state", "shadow government",
        "permanent government", "military-industrial complex", "iron triangle", "fourth branch", "fifth estate",
        "fifth column", "vested interests", "special interests", "dark money", "astroturfing", "political machine",
        "party apparatus", "party discipline", "party line", "political calculus", "political feasibility",
        "political expediency", "political pragmatism", "political realism", "political idealism", "political capital",
        "political legitimacy", "political mandate", "political honeymoon", "lame duck", "political dynasty", "political maverick"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "politics"})
    df = df.drop_duplicates(subset=["keyword"])
    return df

def save_politics_keywords(output_file="politics_keywords.parquet"):
    """Save politics keywords to a parquet file. Append if the file already exists."""
    new_df = get_politics_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique politics keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique politics keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_politics_keywords()