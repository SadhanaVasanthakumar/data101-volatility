# Earnings Volatility Analytics
**Data 101 Final Project · Rutgers University**

Does earnings season make stocks more volatile? We test this question across five major tech stocks — META, AAPL, AMZN, NFLX, and GOOGL — using six years of daily price data (2019–2024).

🌐 **Live Website:** [sadhanavasanthakumar.github.io/data101-volatility](https://sadhanavasanthakumar.github.io/data101-volatility/)

---

## What This Project Does

We combine four analytical methods to study earnings-driven volatility:

1. **Descriptive statistics** — mean, standard deviation, skewness, and excess kurtosis of daily log returns for each ticker
2. **Hypothesis testing** — one-tailed Welch's *t*-test comparing absolute returns in earnings windows (±3 days around each quarterly earnings announcement) vs. normal trading days
3. **GARCH(1,1) modelling** — fits a standard volatility clustering model to each ticker; reports ω, α, β, persistence (α + β), AIC, and BIC
4. **Bayesian inference** — runs a full Bayesian GARCH model via PyMC on NFLX, producing posterior distributions for α and β rather than single-point estimates; compares frequentist vs. Bayesian forecast coverage

---

## Key Findings

| Ticker | Earnings vol | Normal vol | *p*-value | Reject H₀? |
|--------|-------------|------------|-----------|------------|
| META   | 3.21%       | 1.63%      | 0.00016   | ✅ Yes |
| AAPL   | 1.75%       | 1.33%      | 0.0059    | ✅ Yes |
| AMZN   | 2.73%       | 1.44%      | < 0.00001 | ✅ Yes |
| NFLX   | 3.07%       | 1.73%      | 0.00272   | ✅ Yes |
| GOOGL  | 2.29%       | 1.31%      | < 0.00001 | ✅ Yes |

All five stocks show statistically significant earnings-driven volatility spikes (α = 0.01).

GARCH persistence (α + β) ranges from 0.923 (GOOGL) to 0.993 (NFLX), indicating volatility shocks decay very slowly — especially for NFLX.

Bayesian GARCH on NFLX achieved **93% forecast coverage** vs. **86%** for the frequentist model, confirming that propagating parameter uncertainty produces better-calibrated intervals.

---

## Repository Structure

```
data101-volatility/
├── analysis.r          # Full R analysis pipeline (data download → JSON output)
├── app.py              # Flask dev server (serves index.html + /api/run endpoint)
├── index.html          # Interactive website (standalone, no build step)
└── outputs/
    └── results.json    # Pre-computed results (served to the website)
```

---

## Running Locally

### Option A — Just open the website

No server needed. Open `index.html` directly in a browser. The page loads `outputs/results.json` automatically.

### Option B — Re-run the R analysis

**Requirements:** R (≥ 4.0) with packages `quantmod`, `rugarch`, `jsonlite`, `xts`, `zoo`, `moments`

```r
# Install packages if needed
install.packages(c("quantmod", "rugarch", "jsonlite", "xts", "zoo", "moments"))
```

```bash
Rscript analysis.r
```

This downloads fresh price data from Yahoo Finance, re-runs all analysis, and overwrites `outputs/results.json`.

### Option C — Flask dev server

```bash
pip install flask flask-cors
python app.py
# Open http://localhost:5000
```

The `/api/run` endpoint triggers the R script on demand. The `/api/data` endpoint returns the cached JSON.

---

## Data Sources

- **Price data:** Yahoo Finance via the `quantmod` R package (adjusted close prices, 2019-01-01 to 2024-12-31)
- **Earnings dates:** Manually collected from Nasdaq's public historical earnings calendar (24 dates per ticker × 5 tickers = 120 total)

---

## Methods Overview

### Log Returns
Daily log returns are computed as r_t = log(P_t / P_{t-1}). Log returns are preferred over simple returns because they are time-additive and handle large moves more symmetrically.

### Earnings Windows
For each of 24 quarterly earnings dates per ticker, we flag 3 trading days before, the announcement day, and 1 day after as "earnings window" observations. All other days are "normal trading."

### Welch's t-Test
A one-tailed Welch's *t*-test (unequal variances) tests whether mean absolute returns are higher during earnings windows than normal trading days. Significance threshold: α = 0.01.

### GARCH(1,1)
Fitted via Maximum Likelihood Estimation using the `rugarch` package. The conditional variance equation is:

σ²_t = ω + α·ε²_{t-1} + β·σ²_{t-1}

α + β (persistence) measures how slowly volatility shocks decay.

### Bayesian GARCH
Implemented offline in Python using PyMC with NUTS sampling (800 draws, 2 chains, target_accept = 0.90). Priors: α ~ Beta(2,5), β ~ Beta(5,2), ω ~ Exponential(1.0). Results are cached in `results.json`.

---

## Societal Context

Earnings-driven volatility has real consequences: retail investors face outsized risk around announcement dates without the informational advantages held by institutional players. High persistence values (especially NFLX at 0.993) mean that a single bad earnings report can elevate risk for weeks — disproportionately harming investors who cannot monitor positions continuously. This analysis supports the case for clearer pre-earnings risk disclosures.

---

## Authors

Built for Data 101, Rutgers University. See the live report at the website above.
