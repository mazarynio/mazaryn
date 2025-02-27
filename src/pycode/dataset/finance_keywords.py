import pandas as pd
import os

def get_finance_keywords():
    """Return a DataFrame of finance-related keywords."""
    keywords = [
        "investment", "stocks", "bonds", "mutual funds", "ETFs", "cryptocurrency", "Bitcoin", "Ethereum", "forex", "commodities",
        "real estate", "interest rates", "inflation", "deflation", "recession", "financial planning", "retirement savings", "pension funds", "401k", "IRA",
        "tax planning", "capital gains", "dividends", "cash flow", "balance sheet", "income statement", "profit and loss", "budgeting", "savings", "loans",
        "mortgages", "credit scores", "debt management", "hedge funds", "private equity", "venture capital", "IPO", "mergers and acquisitions", "valuation", "financial modeling", 
        "risk management", "insurance", "actuarial science", "banking", "fintech", "payment systems", "digital banking", "wealth management", "portfolio management",
        "asset allocation", "market analysis", "technical analysis", "fundamental analysis", "algorithmic trading", "day trading", "swing trading", "long-term investing",
        "short selling", "options", "futures", "derivatives", "hedging", "financial technology", "blockchain", "decentralized finance", "DeFi", "smart contracts", 
        "cryptocurrency exchanges", "NFTs", "ICO", "security tokens", "digital assets", "peer-to-peer lending", "crowdfunding", "micro-investing", "Robo-advisors",
        "peer-to-peer payments", "mobile wallets", "neobanks", "cryptocurrency wallets", "Bitcoin mining", "Ethereum staking", "stablecoins", "financial regulations",
        "SEC", "FCA", "Dodd-Frank Act", "Basel III", "credit risk", "market risk", "financial fraud", "AML", "KYC", "compliance", "financial reporting", 
        "financial independence", "fire movement", "early retirement", "personal budgeting", "student loans", "auto loans", "credit cards", "payday loans",
        "emergency fund", "insurance premiums", "health savings accounts", "estate planning", "trust funds", "charitable giving", "asset protection", "liability insurance",
        "financial literacy", "economic indicators", "GDP", "unemployment rate", "consumer confidence", "housing market", "stock market crash", "economic bubbles"
    ]

    df = pd.DataFrame({"keyword": keywords, "category": "finance"})
    df = df.drop_duplicates(subset=["keyword"])  
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
