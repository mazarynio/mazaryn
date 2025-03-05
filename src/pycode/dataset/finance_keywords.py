import pandas as pd
import os

def get_finance_keywords():
    """Return a DataFrame of expanded finance-related keywords organized by subcategories."""
    
    # Investment Vehicles and Asset Classes
    investment_assets = [
        "investment", "stocks", "bonds", "mutual funds", "ETFs", "index funds", "cryptocurrency", "Bitcoin", "Ethereum", 
        "Solana", "Cardano", "Ripple", "XRP", "Polkadot", "Avalanche", "forex", "commodities", "precious metals", "gold", 
        "silver", "platinum", "oil futures", "natural gas", "agricultural commodities", "real estate", "REITs", 
        "commercial real estate", "residential real estate", "industrial real estate", "land development", "property flipping",
        "rental properties", "property management", "fixed income", "government bonds", "corporate bonds", "municipal bonds", 
        "treasury bills", "treasury notes", "TIPS", "high-yield bonds", "junk bonds", "convertible bonds", "zero-coupon bonds",
        "sovereign debt", "blue-chip stocks", "penny stocks", "growth stocks", "value stocks", "dividend stocks", "preferred stocks",
        "small-cap stocks", "mid-cap stocks", "large-cap stocks", "mega-cap stocks", "sector funds", "thematic investing",
        "ESG investing", "socially responsible investing", "impact investing", "alternative investments", "collectibles",
        "art investment", "wine investment", "luxury watches", "vintage cars", "memorabilia"
    ]
    
    # Retirement and Financial Planning
    retirement_planning = [
        "financial planning", "retirement savings", "pension funds", "401k", "IRA", "Roth IRA", "SEP IRA", "SIMPLE IRA",
        "traditional IRA", "401k match", "401k rollover", "pension vesting", "defined benefit plan", "defined contribution plan",
        "retirement calculator", "retirement portfolio", "retirement income", "required minimum distributions", "RMDs",
        "social security benefits", "social security claiming strategy", "early retirement", "FIRE movement", "financial independence",
        "tax-advantaged accounts", "annuities", "fixed annuities", "variable annuities", "immediate annuities", "deferred annuities",
        "retirement withdrawal rate", "4% rule", "sequence of returns risk", "longevity risk", "retirement age", "early retirement penalty",
        "catch-up contributions", "retirement tax planning", "retirement healthcare costs", "Medicare planning", "long-term care insurance",
        "retirement relocation", "post-retirement career", "phased retirement", "retirement communities", "inflation-adjusted retirement planning"
    ]
    
    # Tax and Estate Planning
    tax_estate = [
        "tax planning", "capital gains", "capital losses", "tax-loss harvesting", "dividends", "qualified dividends", "ordinary dividends",
        "dividend reinvestment", "DRIP", "tax brackets", "marginal tax rate", "effective tax rate", "tax deductions", "tax credits",
        "standard deduction", "itemized deductions", "tax-exempt income", "municipal bond tax advantages", "tax-efficient investing",
        "tax-deferred growth", "tax-free growth", "estate planning", "inheritance tax", "estate tax", "gift tax", "generation-skipping tax",
        "trust funds", "revocable living trust", "irrevocable trust", "charitable remainder trust", "bypass trust", "spendthrift trust",
        "special needs trust", "testamentary trust", "grantor retained annuity trust", "GRAT", "family limited partnership", "will preparation",
        "probate process", "executor duties", "beneficiary designation", "power of attorney", "healthcare directive", "living will",
        "guardianship designation", "charitable giving", "donor-advised funds", "qualified charitable distributions", "legacy planning",
        "dynasty trusts", "inheritance planning", "step-up in basis", "basis reset", "wealth transfer"
    ]
    
    # Financial Analysis and Accounting
    financial_analysis = [
        "cash flow", "cash flow analysis", "cash flow statement", "cash flow forecasting", "operating cash flow", "free cash flow",
        "discounted cash flow", "DCF analysis", "balance sheet", "assets", "liabilities", "shareholders equity", "book value",
        "tangible assets", "intangible assets", "goodwill", "accounts receivable", "accounts payable", "inventory valuation",
        "income statement", "revenue recognition", "cost of goods sold", "COGS", "gross profit", "operating expenses", "EBITDA",
        "depreciation", "amortization", "net income", "earnings per share", "profit margins", "gross margin", "operating margin",
        "net profit margin", "return on assets", "ROA", "return on equity", "ROE", "return on investment", "ROI", "return on capital employed",
        "ROCE", "working capital", "current ratio", "quick ratio", "debt-to-equity ratio", "interest coverage ratio", "dividend payout ratio",
        "retained earnings", "capital expenditures", "CAPEX", "operational expenditures", "OPEX", "financial statement analysis",
        "horizontal analysis", "vertical analysis", "ratio analysis", "trend analysis", "variance analysis", "break-even analysis",
        "cost-benefit analysis", "sensitivity analysis", "scenario analysis"
    ]
    
    # Budgeting and Personal Finance
    personal_finance = [
        "budgeting", "personal budget", "zero-based budgeting", "50/30/20 budget", "envelope budgeting", "savings", "emergency fund",
        "sinking funds", "cash reserves", "personal savings rate", "debt management", "debt snowball", "debt avalanche", "debt consolidation",
        "debt refinancing", "student loans", "student loan refinancing", "student loan forgiveness", "income-based repayment", "auto loans",
        "mortgage", "mortgage refinancing", "reverse mortgage", "home equity loan", "HELOC", "fixed-rate mortgage", "adjustable-rate mortgage",
        "FHA loans", "VA loans", "jumbo loans", "conforming loans", "points and origination fees", "PMI", "private mortgage insurance",
        "credit cards", "credit card rewards", "cash back rewards", "travel rewards", "balance transfer", "credit card interest", "credit utilization",
        "credit score", "FICO score", "credit report", "credit bureaus", "credit monitoring", "credit freeze", "credit repair", "secured loans",
        "unsecured loans", "personal loans", "payday loans", "installment loans", "line of credit", "overdraft protection", "banking fees",
        "minimum balance requirements", "direct deposit", "automatic payments", "round-up savings", "micro-savings apps", "financial goals",
        "money market accounts", "high-yield savings", "certificates of deposit", "CDs", "checking accounts", "savings accounts"
    ]
    
    # Banking and Financial Services
    banking_services = [
        "banking", "retail banking", "commercial banking", "investment banking", "private banking", "correspondent banking", "online banking",
        "mobile banking", "digital banking", "branch banking", "ATM networks", "electronic funds transfer", "EFT", "wire transfers", "ACH transfers",
        "SWIFT network", "money transfers", "remittances", "payment processing", "payment gateway", "point of sale", "POS systems", "merchant services",
        "credit card processing", "debit card processing", "mobile payment", "digital wallets", "contactless payments", "NFC payments", "QR code payments",
        "P2P payments", "peer-to-peer transfers", "Venmo", "PayPal", "Zelle", "Cash App", "neobanks", "challenger banks", "fintech banks", "bank charter",
        "FDIC insurance", "bank reserves", "fractional reserve banking", "deposit insurance", "bank holidays", "bank reconciliation", "cashier's checks",
        "certified checks", "money orders", "safe deposit boxes", "bank statements", "account aggregation", "interest-bearing accounts", "overdraft fees",
        "insufficient funds", "NSF fees", "minimum balance requirements", "early direct deposit", "mobile check deposit", "ATM fees",
        "international transaction fees", "currency exchange services", "foreign transaction fees", "wire transfer fees"
    ]
    
    # Capital Markets and Investment Banking
    capital_markets = [
        "capital markets", "primary market", "secondary market", "IPO", "initial public offering", "direct listing", "SPAC", 
        "special purpose acquisition company", "mergers and acquisitions", "M&A", "leveraged buyout", "LBO", "management buyout", "MBO",
        "hostile takeover", "friendly merger", "acquisition premium", "divestiture", "spin-off", "carve-out", "equity financing",
        "debt financing", "mezzanine financing", "bridge loan", "seed funding", "Series A funding", "Series B funding", "Series C funding",
        "late-stage funding", "pre-IPO funding", "angel investors", "venture capital", "VC firms", "private equity", "PE firms",
        "hedge funds", "fund of funds", "sovereign wealth funds", "endowment funds", "investment banks", "bulge bracket banks",
        "boutique investment banks", "deal flow", "due diligence", "valuation", "business valuation", "enterprise value", "market capitalization",
        "discounted cash flow", "comparable company analysis", "precedent transaction analysis", "LBO analysis", "accretion/dilution analysis",
        "synergy analysis", "EBITDA multiple", "P/E ratio", "price-to-book ratio", "price-to-sales ratio", "terminal value", "exit multiple",
        "internal rate of return", "IRR", "hurdle rate", "fair market value", "underwriting", "securities underwriting", "prospectus",
        "red herring", "roadshow", "book building", "greenshoe option", "lock-up period", "quiet period", "syndicated loans", "loan syndication"
    ]
    
    # Market Analysis and Trading Strategies
    market_analysis = [
        "market analysis", "technical analysis", "fundamental analysis", "quantitative analysis", "sentiment analysis", "market indicators",
        "economic indicators", "leading indicators", "lagging indicators", "coincident indicators", "market trends", "bull market", "bear market",
        "sideways market", "market correction", "market crash", "market rebound", "market volatility", "VIX", "volatility index", "support levels",
        "resistance levels", "breakout trading", "breakdown trading", "gap trading", "trend following", "mean reversion", "momentum trading",
        "contrarian investing", "swing trading", "position trading", "day trading", "scalping", "arbitrage", "statistical arbitrage",
        "pairs trading", "market neutral", "algorithmic trading", "algo trading", "high-frequency trading", "HFT", "program trading",
        "basket trading", "block trades", "dark pools", "order types", "market order", "limit order", "stop order", "stop-limit order",
        "trailing stop", "good-till-cancelled", "GTC", "fill or kill", "all or none", "immediate or cancel", "iceberg order", "order book",
        "bid-ask spread", "market depth", "market liquidity", "market makers", "order flow", "price action", "candlestick patterns",
        "chart patterns", "head and shoulders", "double top", "double bottom", "cup and handle", "flags and pennants", "triangles", "wedges",
        "moving averages", "MACD", "relative strength index", "RSI", "stochastic oscillator", "Bollinger Bands", "Fibonacci retracement",
        "Elliott Wave theory", "Dow Theory", "Gann theory", "market cycles", "sector rotation", "style rotation", "factor investing", "smart beta"
    ]
    
    # Risk Management and Insurance
    risk_insurance = [
        "risk management", "financial risk", "market risk", "credit risk", "liquidity risk", "operational risk", "systemic risk",
        "idiosyncratic risk", "interest rate risk", "currency risk", "inflation risk", "political risk", "legal risk", "regulatory risk",
        "model risk", "tail risk", "black swan events", "risk assessment", "risk identification", "risk mitigation", "risk transfer",
        "risk retention", "risk avoidance", "risk-adjusted return", "Sharpe ratio", "Treynor ratio", "Jensen's alpha", "beta coefficient",
        "value at risk", "VaR", "conditional VaR", "stress testing", "scenario analysis", "Monte Carlo simulation", "sensitivity analysis",
        "diversification", "hedging", "natural hedge", "derivative hedge", "portfolio insurance", "stop-loss strategies", "options strategies",
        "collar strategy", "protective put", "covered call", "insurance", "life insurance", "term life insurance", "whole life insurance",
        "universal life insurance", "variable life insurance", "indexed universal life", "group life insurance", "key person insurance",
        "health insurance", "HMO", "PPO", "high-deductible health plan", "HDHP", "health savings account", "HSA", "flexible spending account", "FSA",
        "property insurance", "homeowners insurance", "renters insurance", "auto insurance", "liability insurance", "umbrella insurance",
        "business insurance", "business interruption insurance", "professional liability insurance", "errors and omissions insurance", "E&O",
        "directors and officers insurance", "D&O", "workers compensation insurance", "disability insurance", "long-term care insurance",
        "annuities", "fixed annuities", "variable annuities", "indexed annuities", "immediate annuities", "deferred annuities",
        "self-insurance", "captive insurance", "reinsurance", "actuarial science", "mortality tables", "underwriting", "insurance premium",
        "insurance deductible", "policy limits", "exclusions", "riders", "claims process", "insurance adjuster"
    ]
    
    # Derivatives and Alternative Investments
    derivatives = [
        "derivatives", "options", "call options", "put options", "covered calls", "protective puts", "option chains", "option greeks",
        "delta", "gamma", "theta", "vega", "rho", "implied volatility", "historical volatility", "options strategies", "bull spread",
        "bear spread", "butterfly spread", "iron condor", "straddle", "strangle", "calendar spread", "diagonal spread", "collar",
        "futures", "futures contracts", "commodity futures", "financial futures", "index futures", "currency futures", "interest rate futures",
        "futures margin", "initial margin", "maintenance margin", "margin call", "contango", "backwardation", "futures roll", "forward contracts",
        "forward rate agreements", "FRAs", "swaps", "interest rate swaps", "currency swaps", "credit default swaps", "CDS", "total return swaps",
        "equity swaps", "variance swaps", "commodity swaps", "structured products", "equity-linked notes", "principal-protected notes",
        "reverse convertibles", "accumulators", "autocallables", "barrier options", "exotic options", "Asian options", "lookback options",
        "binary options", "digital options", "compound options", "quanto options", "basket options", "rainbow options", "warrants",
        "convertible securities", "convertible bonds", "convertible preferred stock", "alternative investments", "private equity",
        "buyout funds", "growth equity", "venture capital", "seed capital", "Series A/B/C funding", "angel investing", "hedge funds",
        "market neutral funds", "long/short equity", "global macro", "event-driven", "distressed securities", "fixed income arbitrage",
        "convertible arbitrage", "managed futures", "CTAs", "commodity trading advisors", "fund of funds", "multi-strategy funds",
        "direct lending", "infrastructure investing", "timber investing", "farmland investing", "collectibles", "art investing",
        "wine investing", "precious metals", "royalty trusts", "music royalties", "intellectual property", "litigation financing"
    ]
    
    # Blockchain and Cryptocurrency
    blockchain_crypto = [
        "blockchain", "distributed ledger technology", "DLT", "blocks", "mining", "proof of work", "PoW", "proof of stake", "PoS",
        "delegated proof of stake", "consensus mechanism", "validators", "nodes", "blockchain oracle", "smart contracts", "gas fees",
        "blockchain scalability", "sharding", "sidechains", "layer 2 solutions", "interoperability", "cross-chain", "tokenization",
        "cryptocurrency", "digital currency", "virtual currency", "Bitcoin", "BTC", "Bitcoin halving", "Ethereum", "ETH", "Ethereum 2.0",
        "Ether", "Solana", "SOL", "Cardano", "ADA", "Ripple", "XRP", "Polkadot", "DOT", "Avalanche", "AVAX", "Chainlink", "LINK",
        "Litecoin", "LTC", "Bitcoin Cash", "BCH", "Monero", "XMR", "privacy coins", "stablecoins", "USDT", "Tether", "USDC", "USD Coin",
        "DAI", "algorithmic stablecoins", "CBDC", "central bank digital currency", "altcoins", "memecoin", "governance tokens", "utility tokens",
        "security tokens", "non-fungible tokens", "NFTs", "NFT marketplace", "NFT collections", "crypto collectibles", "cryptocurrency wallets",
        "hot wallet", "cold wallet", "hardware wallet", "software wallet", "custody solution", "private keys", "public keys", "seed phrases",
        "mining rigs", "mining pools", "ASIC miners", "hash rate", "block rewards", "cryptocurrency exchanges", "centralized exchanges", "CEX",
        "decentralized exchanges", "DEX", "automated market makers", "AMM", "liquidity pools", "liquidity providers", "LP tokens",
        "decentralized finance", "DeFi", "yield farming", "staking", "staking rewards", "lending protocols", "borrowing protocols",
        "collateralized debt positions", "CDP", "flash loans", "impermanent loss", "total value locked", "TVL", "annual percentage yield", "APY",
        "decentralized autonomous organizations", "DAOs", "governance voting", "protocol governance", "initial coin offering", "ICO",
        "security token offering", "STO", "initial DEX offering", "IDO", "initial exchange offering", "IEO", "airdrop", "token distribution",
        "token vesting", "token economics", "tokenomics", "token burn", "deflationary tokens", "crypto derivatives", "perpetual futures",
        "funding rate", "blockchain analytics", "on-chain analysis", "crypto regulation", "KYC/AML in crypto", "crypto taxation"
    ]
    
    # Financial Regulations and Compliance
    regulation_compliance = [
        "financial regulations", "regulatory compliance", "financial supervision", "central banks", "Federal Reserve", "European Central Bank",
        "Bank of England", "Bank of Japan", "People's Bank of China", "monetary policy", "interest rate policy", "quantitative easing", "QE",
        "quantitative tightening", "QT", "open market operations", "reserve requirements", "discount window", "federal funds rate", "LIBOR",
        "SOFR", "SONIA", "reference rates", "yield curve", "yield curve control", "forward guidance", "financial stability", "systemic risk",
        "macroprudential policy", "bank regulation", "Basel Accords", "Basel III", "Basel IV", "capital requirements", "tier 1 capital",
        "tier 2 capital", "risk-weighted assets", "capital adequacy ratio", "liquidity coverage ratio", "LCR", "net stable funding ratio", "NSFR",
        "leverage ratio", "banking supervision", "stress tests", "securities regulation", "Securities and Exchange Commission", "SEC",
        "Financial Industry Regulatory Authority", "FINRA", "Commodity Futures Trading Commission", "CFTC", "European Securities and Markets Authority",
        "ESMA", "Financial Conduct Authority", "FCA", "Securities and Exchange Board of India", "SEBI", "financial market regulation",
        "market manipulation", "insider trading", "front-running", "spoofing", "layering", "wash trading", "pump and dump", "bear raid",
        "short and distort", "Dodd-Frank Act", "Volcker Rule", "MiFID II", "EMIR", "Sarbanes-Oxley Act", "SOX", "FATCA", "CRS", "bank secrecy act",
        "anti-money laundering", "AML", "know your customer", "KYC", "customer due diligence", "CDD", "enhanced due diligence", "EDD",
        "beneficial ownership", "suspicious activity report", "SAR", "currency transaction report", "CTR", "Office of Foreign Assets Control", "OFAC",
        "sanctions compliance", "politically exposed persons", "PEPs", "compliance risk", "compliance program", "compliance audit",
        "regulatory reporting", "prudential regulation", "consumer financial protection", "CFPB", "UDAAP", "fair lending", "truth in lending",
        "TILA", "Real Estate Settlement Procedures Act", "RESPA", "Equal Credit Opportunity Act", "ECOA", "Fair Credit Reporting Act", "FCRA",
        "Fair Debt Collection Practices Act", "FDCPA", "Electronic Fund Transfer Act", "EFTA", "Gramm-Leach-Bliley Act", "GLBA",
        "privacy regulation", "data protection", "GDPR", "CCPA", "information security", "cybersecurity regulation", "financial fraud",
        "fraud detection", "financial crime", "investment adviser regulation", "fiduciary duty", "broker-dealer regulation", "registered representatives",
        "investment company regulation", "fund regulation", "hedge fund regulation", "private equity regulation", "corporate governance",
        "proxy voting", "shareholder rights", "stock exchange listing requirements", "financial reporting", "GAAP", "IFRS", "accounting standards",
        "auditing standards", "disclosure requirements", "material information", "financial statement certification", "Form 10-K", "Form 10-Q",
        "Form 8-K", "prospectus requirements", "registration statement", "exempt offerings", "Regulation D", "Rule 144A", "Regulation S",
        "Regulation Crowdfunding", "blue sky laws", "payment services regulation", "money transmitter laws", "state licensing", "federal licensing",
        "cryptocurrency regulation", "virtual asset service providers", "VASPs", "stablecoin regulation", "DeFi regulation"
    ]
    
    # Global Economics and Market Trends
    economics_trends = [
        "economic indicators", "GDP", "gross domestic product", "nominal GDP", "real GDP", "GDP per capita", "GDP growth rate", "national income",
        "gross national income", "GNI", "gross national product", "GNP", "economic output", "industrial production", "manufacturing output",
        "service sector", "primary sector", "secondary sector", "tertiary sector", "unemployment rate", "employment-to-population ratio",
        "labor force participation rate", "job creation", "job losses", "nonfarm payrolls", "initial jobless claims", "continuing claims",
        "inflation", "consumer price index", "CPI", "producer price index", "PPI", "personal consumption expenditures", "PCE", "core inflation",
        "headline inflation", "inflation expectations", "deflation", "disinflation", "hyperinflation", "stagflation", "consumer confidence",
        "consumer sentiment", "business confidence", "purchasing managers index", "PMI", "ISM manufacturing", "ISM non-manufacturing",
        "durable goods orders", "factory orders", "retail sales", "personal income", "disposable income", "personal spending", "personal saving rate",
        "capacity utilization", "industrial capacity", "housing starts", "building permits", "new home sales", "existing home sales", "pending home sales",
        "Case-Shiller home price index", "construction spending", "trade balance", "current account", "capital account", "balance of payments",
        "imports", "exports", "terms of trade", "trade surplus", "trade deficit", "current account deficit", "foreign direct investment", "FDI",
        "foreign portfolio investment", "hot money flows", "international reserves", "sovereign debt", "government budget", "budget deficit",
        "budget surplus", "primary deficit", "primary surplus", "fiscal policy", "fiscal stimulus", "fiscal austerity", "government spending",
        "government revenue", "tax revenue", "debt-to-GDP ratio", "public debt", "national debt", "government bonds", "treasury securities",
        "bond yields", "yield curve", "inverted yield curve", "monetary policy", "central bank policy", "interest rates", "policy rate",
        "discount rate", "bank rate", "reserve ratio", "money supply", "M0", "M1", "M2", "M3", "money velocity", "monetary base", "quantitative easing",
        "quantitative tightening", "currency", "exchange rates", "currency appreciation", "currency depreciation", "devaluation", "revaluation",
        "currency intervention", "currency manipulation", "currency peg", "floating exchange rate", "fixed exchange rate", "foreign exchange reserves",
        "forex reserves", "economic cycles", "business cycle", "expansion", "peak", "contraction", "trough", "recession", "depression", "recovery",
        "soft landing", "hard landing", "economic growth", "economic contraction", "economic slowdown", "economic boom", "economic bubble",
        "market bubble", "asset bubble", "housing bubble", "stock market bubble", "bubble burst", "market correction", "market crash", "black swan event",
        "secular trends", "technological disruption", "digital transformation", "gig economy", "sharing economy", "remote work", "automation",
        "artificial intelligence impact", "demographic trends", "aging population", "population growth", "urbanization", "rural depopulation",
        "emerging markets", "frontier markets", "developed markets", "BRICS", "G7", "G20", "OECD countries", "geopolitical risk", "trade tensions",
        "trade wars", "sanctions", "embargoes", "capital controls", "protectionism", "globalization", "deglobalization", "reshoring", "nearshoring",
        "supply chain resilience", "economic sanctions", "climate change economics", "carbon pricing", "carbon tax", "cap and trade", "ESG factors",
        "sustainable investing", "green bonds", "climate finance", "energy transition", "renewable energy economics", "fossil fuel divestment"
    ]
    
    # Fintech and Financial Innovation
    fintech = [
        "financial technology", "fintech", "digital banking", "neobanks", "challenger banks", "digital-only banks", "open banking", "PSD2",
        "banking-as-a-service", "BaaS", "embedded finance", "API banking", "banking APIs", "account aggregation", "personal finance management", "PFM",
        "financial data aggregation", "robo-advisors", "automated investing", "digital wealth management", "algorithmic portfolio management",
        "goal-based investing", "micro-investing", "fractional shares", "zero-commission trading", "mobile trading", "mobile investing",
        "investment apps", "trading apps", "financial apps", "payment technology", "mobile payment", "digital payment", "contactless payment",
        "QR code payment", "NFC payment", "digital wallets", "e-wallets", "mobile wallets", "P2P payment", "peer-to-peer transfers", "instant payment",
        "real-time payment", "faster payments", "payment initiation", "payment processing", "payment gateway", "payment service provider", "PSP",
        "payment facilitator", "payment aggregator", "acquiring", "merchant acquiring", "merchant services", "point of sale", "POS systems",
        "mPOS", "mobile point of sale", "payment cards", "card networks", "card schemes", "card-not-present", "CNP transactions", "3D Secure",
        "tokenization", "payment tokens", "digital lending", "online lending", "P2P lending", "peer-to-peer lending", "marketplace lending",
        "alternative lending", "digital mortgages", "mortgage technology", "automated underwriting", "credit decisioning", "alternative credit scoring",
        "psychometric credit scoring", "behavioral scoring", "cash flow underwriting", "digital insurance", "insurtech", "insurance technology",
        "on-demand insurance", "usage-based insurance", "parametric insurance", "microinsurance", "P2P insurance", "insurance comparison",
        "digital brokers", "automated claims processing", "insurance APIs", "regtech", "regulatory technology", "compliance technology",
        "KYC technology", "digital KYC", "video KYC", "remote onboarding", "identity verification", "digital identity", "biometric authentication",
        "facial recognition", "fingerprint verification", "voice recognition", "liveness detection", "document verification", "transaction monitoring",
        "fraud detection", "anti-fraud systems", "behavioral biometrics", "device fingerprinting", "AML technology", "suspicious transaction detection",
        "blockchain technology", "distributed ledger technology", "DLT", "enterprise blockchain", "permissioned blockchain", "private blockchain",
        "public blockchain", "consortium blockchain", "blockchain interoperability", "cross-chain technology", "cryptocurrency exchanges",
        "digital asset exchanges", "crypto custody", "institutional custody", "prime brokerage", "crypto prime brokerage", "crypto lending",
        "crypto borrowing", "crypto yield", "crypto staking", "synthetic assets", "tokenization platforms", "asset tokenization", "security tokens",
        "digital securities", "NFT marketplaces", "NFT finance", "GameFi", "play-to-earn", "metaverse finance", "virtual real estate", "digital wallets",
        "crypto wallets", "central bank digital currencies", "CBDCs", "wholesale CBDC", "retail CBDC", "stablecoins", "algorithmic stablecoins",
        "decentralized finance", "DeFi", "DeFi protocols", "decentralized exchanges", "DEX", "automated market makers", "AMM", "liquidity pools",
        "yield farming", "liquidity mining", "yield aggregators", "lending protocols", "borrowing protocols", "flash loans", "collateralized debt",
        "synthetic assets", "derivatives protocols", "perpetual swaps", "decentralized insurance", "decentralized identity", "DID", "self-sovereign identity",
        "verifiable credentials", "big data analytics", "alternative data", "data science in finance", "predictive analytics", "AI in finance",
        "machine learning in finance", "natural language processing", "NLP", "sentiment analysis", "algorithmic trading", "high-frequency trading", "HFT",
        "quantitative trading", "quantum computing in finance", "quantum-resistant cryptography", "edge computing", "cloud computing in finance",
        "financial cloud", "SaaS for finance", "API economy", "financial API marketplace", "data marketplaces", "alternative data providers",
        "robotic process automation", "RPA", "intelligent automation", "digital transformation", "financial inclusion", "banking the unbanked",
        "underbanked services", "cross-border payments", "remittance technology", "international money transfers", "FX technology", "multi-currency wallets",
        "superapps", "financial superapps", "embedded lending", "embedded insurance", "buy now pay later", "BNPL", "earned wage access", "EWA",
        "income-share agreements", "subscription management", "recurring billing", "digital subscriptions", "subscription commerce", "direct indexing",
        "personalized indexing", "ESG investing platforms", "impact investing technology", "sustainable finance technology", "carbon footprint tracking",
        "climate fintech", "green fintech", "wealthtech", "personal taxation software", "tax optimization tools", "financial planning software",
        "financial wellness platforms", "behavioral finance apps", "gamified savings", "financial literacy apps", "digital budgeting tools"
    ]
    
    # All keywords combined
    all_keywords = (
        investment_assets + retirement_planning + tax_estate + financial_analysis + personal_finance + 
        banking_services + capital_markets + market_analysis + risk_insurance + derivatives + 
        blockchain_crypto + regulation_compliance + economics_trends + fintech
    )
    
    # Remove duplicates and create DataFrame
    unique_keywords = list(set(all_keywords))
    df = pd.DataFrame({"keyword": unique_keywords, "category": "finance"})
    return df

def save_finance_keywords(output_file="finance_keywords.parquet"):
    """Save finance keywords to a parquet file. Append if the file already exists."""
    new_df = get_finance_keywords()

    if os.path.exists(output_file):
        existing_df = pd.read_parquet(output_file)
        combined_df = pd.concat([existing_df, new_df]).drop_duplicates(subset=["keyword"])
        print(f"Appended {len(new_df)} unique finance keywords to {output_file}")
    else:
        combined_df = new_df
        print(f"Saved {len(new_df)} unique finance keywords to {output_file}")

    combined_df.to_parquet(output_file, engine="pyarrow", compression="snappy")
    return combined_df

if __name__ == "__main__":
    save_finance_keywords()