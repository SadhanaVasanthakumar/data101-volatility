suppressPackageStartupMessages({
  library(quantmod)    # price data download
  library(rugarch)     # GARCH modelling
  library(jsonlite)    # JSON output
  library(xts)         # time-series operations
  library(zoo)         # rolling functions
  library(moments)     # skewness / kurtosis
})

#config
TICKERS    <- c("META", "AAPL", "AMZN", "NFLX", "GOOGL")
START_DATE <- "2019-01-01"
END_DATE   <- "2024-12-31"
ALPHA      <- 0.01          # significance level
PRE_DAYS   <- 3             # trading days before earnings
POST_DAYS  <- 1             # trading days after earnings
dir.create("outputs", showWarnings = FALSE)

cat("=== Data101 · Stock Volatility Analysis (R) ===\n")

# earnings dates
earnings_dates <- list(
  META = as.Date(c(
    "2019-01-30","2019-04-24","2019-07-24","2019-10-30",
    "2020-01-29","2020-04-22","2020-07-29","2020-10-29",
    "2021-01-27","2021-04-28","2021-07-28","2021-10-25",
    "2022-02-02","2022-04-27","2022-07-27","2022-10-26",
    "2023-02-01","2023-04-26","2023-07-26","2023-10-25",
    "2024-01-31","2024-04-24","2024-07-31","2024-10-30"
  )),
  AAPL = as.Date(c(
    "2019-01-29","2019-04-30","2019-07-30","2019-10-30",
    "2020-01-28","2020-04-30","2020-07-30","2020-10-29",
    "2021-01-27","2021-04-28","2021-07-27","2021-10-28",
    "2022-01-27","2022-04-28","2022-07-28","2022-10-27",
    "2023-02-02","2023-05-04","2023-08-03","2023-11-02",
    "2024-02-01","2024-05-02","2024-08-01","2024-10-31"
  )),
  AMZN = as.Date(c(
    "2019-02-01","2019-04-25","2019-07-25","2019-10-24",
    "2020-01-30","2020-04-30","2020-07-30","2020-10-29",
    "2021-02-04","2021-04-29","2021-07-29","2021-10-28",
    "2022-02-03","2022-04-28","2022-07-28","2022-10-27",
    "2023-02-02","2023-04-27","2023-08-03","2023-10-26",
    "2024-02-01","2024-05-02","2024-08-01","2024-10-31"
  )),
  NFLX = as.Date(c(
    "2019-01-17","2019-04-16","2019-07-17","2019-10-16",
    "2020-01-21","2020-04-21","2020-07-16","2020-10-20",
    "2021-01-19","2021-04-20","2021-07-20","2021-10-19",
    "2022-01-20","2022-04-19","2022-07-19","2022-10-18",
    "2023-01-19","2023-04-18","2023-07-19","2023-10-18",
    "2024-01-23","2024-04-22","2024-07-17","2024-10-16"
  )),
  GOOGL = as.Date(c(
    "2019-02-04","2019-04-29","2019-07-25","2019-10-28",
    "2020-02-03","2020-05-04","2020-07-28","2020-10-29",
    "2021-02-02","2021-04-27","2021-07-27","2021-10-26",
    "2022-02-01","2022-04-26","2022-07-26","2022-10-25",
    "2023-02-02","2023-04-25","2023-07-25","2023-10-24",
    "2024-01-30","2024-04-25","2024-07-23","2024-10-29"
  ))
)

# helper: build earnings mask
build_earnings_mask <- function(dates_index, ticker) {
  mask <- rep(FALSE, length(dates_index))
  for (ed in earnings_dates[[ticker]]) {
    dists  <- abs(as.numeric(dates_index - ed))
    nearest <- which.min(dists)
    start  <- max(1, nearest - PRE_DAYS)
    end    <- min(length(dates_index), nearest + POST_DAYS)
    mask[start:end] <- TRUE
  }
  mask
}

# step 1 : download data 
cat("\n[1/7] Downloading price data from Yahoo Finance...\n")

price_list <- lapply(TICKERS, function(t) {
  cat("  Fetching", t, "...\n")
  tryCatch({
    getSymbols(t, src = "yahoo", from = START_DATE, to = END_DATE,
               auto.assign = FALSE, warnings = FALSE)
  }, error = function(e) {
    cat("  ERROR fetching", t, ":", conditionMessage(e), "\n")
    NULL
  })
})
names(price_list) <- TICKERS

# extract adjusted close, merge into one xts

adj_close <- do.call(merge, lapply(seq_along(TICKERS), function(i) {
  px <- price_list[[i]]
  if (is.null(px)) return(NULL)
  cl <- Ad(px)
  colnames(cl) <- TICKERS[i]
  cl
}))
adj_close <- na.omit(adj_close)

# log returns
log_returns <- diff(log(adj_close))
log_returns <- na.omit(log_returns)

trading_days <- nrow(log_returns)
cat("  Trading days:", trading_days, "\n")
cat("  Date range  :", format(start(log_returns)), "to", format(end(log_returns)), "\n")

# step 2: summary statistics 

cat("\n[2/7] Computing summary statistics...\n")

summary_stats <- lapply(TICKERS, function(t) {
  r <- as.numeric(log_returns[, t])
  list(
    ticker   = t,
    mean     = round(mean(r)  * 100, 4),
    std      = round(sd(r)    * 100, 4),
    skewness = round(skewness(r), 3),
    kurtosis = round(kurtosis(r) - 3, 3),   # excess kurtosis
    min      = round(min(r)   * 100, 3),
    max      = round(max(r)   * 100, 3),
    n        = length(r)
  )
})
names(summary_stats) <- TICKERS
cat("  Done.\n")

# step 3: rolling 30-day annualised volatility

cat("\n[3/7] Computing rolling volatility...\n")

roll_vol_list <- lapply(TICKERS, function(t) {
  r   <- log_returns[, t]
  rv  <- rollapply(r, width = 30, FUN = sd, fill = NA, align = "right") * sqrt(252) * 100
  rv  <- na.omit(rv)
  idx <- index(rv)
  step <- max(1L, floor(length(idx) / 150L))
  list(
    dates  = format(idx[seq(1, length(idx), step)]),
    values = round(as.numeric(rv[seq(1, length(idx), step)]), 4)
  )
})
names(roll_vol_list) <- TICKERS
cat("  Done.\n")

# step 4: normalised prices 

cat("\n[4/7] Building normalised price series...\n")

norm_prices <- lapply(TICKERS, function(t) {
  px   <- as.numeric(adj_close[, t])
  norm <- (px / px[1]) * 100
  idx  <- index(adj_close)
  step <- max(1L, floor(length(idx) / 150L))
  list(
    dates  = format(idx[seq(1, length(idx), step)]),
    values = round(norm[seq(1, length(idx), step)], 3)
  )
})
names(norm_prices) <- TICKERS

# return correlation matrix
corr_mat <- cor(as.data.frame(log_returns))
corr_list <- lapply(TICKERS, function(t) {
  lapply(as.list(round(corr_mat[t, ], 4)), function(x) x)
})
names(corr_list) <- TICKERS
cat("  Done.\n")

# step 5: hypothesis testing (welch's t-test)

cat("\n[5/7] Running Welch's t-tests...\n")

hyp_results <- lapply(TICKERS, function(t) {
  r    <- as.numeric(log_returns[, t])
  mask <- build_earnings_mask(index(log_returns), t)

  earn_vol   <- abs(r[mask])
  normal_vol <- abs(r[!mask])

  # One-tailed Welch t-test: H1: earn_vol > normal_vol
  tt <- t.test(earn_vol, normal_vol, alternative = "greater",
               var.equal = FALSE)

  cat(sprintf("  %s: earn=%.3f%% norm=%.3f%% t=%.3f p=%.4f %s\n",
    t,
    mean(earn_vol) * 100,
    mean(normal_vol) * 100,
    tt$statistic,
    tt$p.value,
    ifelse(tt$p.value < ALPHA, "*** REJECT H0", "")
  ))

  list(
    ticker       = t,
    earn_vol     = round(mean(earn_vol) * 100, 4),
    normal_vol   = round(mean(normal_vol) * 100, 4),
    delta        = round((mean(earn_vol) - mean(normal_vol)) * 100, 4),
    t_stat       = round(as.numeric(tt$statistic), 4),
    p_value      = round(tt$p.value, 5),
    significant  = tt$p.value < ALPHA,
    df           = round(tt$parameter, 1),
    earn_n       = sum(mask),
    normal_n     = sum(!mask),
    conf_lo      = round(tt$conf.int[1] * 100, 4),
    conf_hi      = round(tt$conf.int[2] * 100, 4)
  )
})
names(hyp_results) <- TICKERS

#step 6: garch (1,1) model

cat("\n[6/7] Fitting GARCH(1,1) models...\n")

garch_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

garch_params <- lapply(TICKERS, function(t) {
  r   <- as.numeric(log_returns[, t]) * 100   # rugarch expects % returns
  fit <- tryCatch(
    ugarchfit(spec = garch_spec, data = r, solver = "hybrid"),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    cat("  GARCH fit failed for", t, "\n")
    return(list(ticker = t, omega = NA, alpha = NA, beta = NA, ab_sum = NA,
                log_lik = NA, aic = NA, bic = NA))
  }
  p   <- coef(fit)
  cat(sprintf("  %s: omega=%.5f alpha=%.4f beta=%.4f alpha+beta=%.4f\n",
    t, p["omega"], p["alpha1"], p["beta1"], p["alpha1"] + p["beta1"]))
  list(
    ticker  = t,
    omega   = round(unname(p["omega"]),  6),
    alpha   = round(unname(p["alpha1"]), 4),
    beta    = round(unname(p["beta1"]),  4),
    ab_sum  = round(unname(p["alpha1"] + p["beta1"]), 4),
    log_lik = round(likelihood(fit), 2),
    aic     = round(infocriteria(fit)["Akaike",], 4),
    bic     = round(infocriteria(fit)["Bayes",], 4)
  )
})
names(garch_params) <- TICKERS

#  NFLX conditional volatility series 
cat("  Building NFLX conditional volatility series...\n")
r_nflx <- as.numeric(log_returns[, "NFLX"]) * 100
fit_nflx <- ugarchfit(spec = garch_spec, data = r_nflx, solver = "hybrid")
cond_vol <- as.numeric(sigma(fit_nflx))
cv_idx   <- index(log_returns)
step     <- max(1L, floor(length(cv_idx) / 150L))
nflx_cond_vol <- list(
  dates  = format(cv_idx[seq(1, length(cv_idx), step)]),
  values = round(cond_vol[seq(1, length(cv_idx), step)], 4)
)

# step 6b: rolling rmse evaluation
cat("  Evaluating rolling RMSE (10 windows per ticker)...\n")
TRAIN_WIN <- 1000L
HORIZON   <- 20L

rmse_results <- lapply(TICKERS, function(t) {
  r      <- as.numeric(log_returns[, t]) * 100
  n      <- length(r)
  errors <- numeric(0)
  eval_pts <- seq(TRAIN_WIN + 1L, n - HORIZON, by = HORIZON * 3L)
  eval_pts <- head(eval_pts, 10L)

  for (i in eval_pts) {
    train <- r[(i - TRAIN_WIN):(i - 1L)]
    test  <- r[i:(i + HORIZON - 1L)]
    tryCatch({
      fit   <- ugarchfit(spec = garch_spec, data = train, solver = "hybrid")
      fc    <- ugarchforecast(fit, n.ahead = HORIZON)
      fc_sd <- as.numeric(sigma(fc))
      realized <- abs(test)
      errors <- c(errors, sqrt(mean((fc_sd - realized)^2)))
    }, error = function(e) NULL)
  }
  list(ticker = t, rmse = round(if (length(errors)) mean(errors) else NA, 4))
})
names(rmse_results) <- TICKERS

# step 7: bayesian note 
cat("\n[7/7] Including Bayesian posterior results (cached from PyMC)...\n")
# full Bayesian GARCH via PyMC is computationally intensive.
# sesults below are from the paper (800 draws, 2 chains, NUTS, NFLX 500-day subset)
bayesian <- list(
  available   = FALSE,
  note        = "Full Bayesian GARCH run via PyMC (see app.py). Results below are from paper.",
  alpha = list(mean = 0.108, hpd_lo = 0.092, hpd_hi = 0.124),
  beta  = list(mean = 0.878, hpd_lo = 0.851, hpd_hi = 0.903),
  omega = list(mean = 0.0003, hpd_lo = 0.0001, hpd_hi = 0.0005),
  coverage_bayesian    = 93,
  coverage_frequentist = 86,
  sampler = list(draws = 800, tune = 400, chains = 2,
                 target_accept = 0.90, algorithm = "NUTS")
)

# assemble json 
cat("\nAssembling JSON output...\n")

output <- list(
  meta = list(
    generated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    r_version     = R.version$version.string,
    packages      = list(
      quantmod = as.character(packageVersion("quantmod")),
      rugarch  = as.character(packageVersion("rugarch")),
      moments  = as.character(packageVersion("moments"))
    ),
    start         = START_DATE,
    end           = END_DATE,
    tickers       = TICKERS,
    trading_days  = trading_days,
    alpha_level   = ALPHA,
    earnings_window = list(pre = PRE_DAYS, post = POST_DAYS)
  ),
  summary      = unname(summary_stats),
  hypothesis   = unname(hyp_results),
  garch_params = unname(garch_params),
  nflx_cond_vol = nflx_cond_vol,
  rolling_vol  = roll_vol_list,
  rmse         = unname(rmse_results),
  bayesian     = bayesian,
  norm_prices  = norm_prices,
  correlation  = corr_list
)

# write JSON
json_path <- file.path("outputs", "results.json")
write_json(output, path = json_path, auto_unbox = TRUE, digits = 6)
cat("Saved →", json_path, "\n")

# human-readable summary
cat("\n=== HYPOTHESIS TEST SUMMARY ===\n")
for (t in TICKERS) {
  h <- hyp_results[[t]]
  cat(sprintf("  %-5s  earn=%.3f%%  norm=%.3f%%  p=%.4f  %s\n",
    t, h$earn_vol, h$normal_vol, h$p_value,
    ifelse(h$significant, "[REJECT H0]", "[fail to reject]")))
}

cat("\n=== GARCH(1,1) SUMMARY ===\n")
for (t in TICKERS) {
  g <- garch_params[[t]]
  cat(sprintf("  %-5s  alpha=%.4f  beta=%.4f  alpha+beta=%.4f  AIC=%.4f\n",
    t, g$alpha, g$beta, g$ab_sum, g$aic))
}

cat("\n=== DONE ===\n")
cat("Results written to:", json_path, "\n")