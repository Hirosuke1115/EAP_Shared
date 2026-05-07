# == Step 1: Library Setup ============================================================

# Install remotes if needed (for GitHub installs)
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Clear bad GitHub token if present, then install OpenSourceAP.DownloadR
Sys.unsetenv("GITHUB_PAT")
remotes::install_github("tomz23/OpenSourceAP.DownloadR")

# Install CRAN packages if not already installed
packages <- c(
  # Data download & manipulation
  "tidyfinance",        # Stock prices & S&P 500 from Yahoo Finance
  "frenchdata",         # Ken French factor data
  "dplyr",
  "tidyr",
  "lubridate",
  "stringr",
  "tibble",
  "purrr",
  "zoo",                # Rolling window calculations
  
  # Volatility modelling
  "rugarch",            # GARCH(1,1) estimation
  
  # Regression & factor models
  "lmtest",             # Coefficient tests
  "sandwich"            # Newey-West standard errors
)

install.packages(setdiff(packages, rownames(installed.packages())))

# Load all packages
install.packages("moments")
install.packages("writexl")
library(remotes)
library(OpenSourceAP.DownloadR)
library(tidyfinance)
library(frenchdata)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(tibble)
library(purrr)
library(zoo)
library(rugarch)
library(lmtest)
library(sandwich)
library(tidyfinance)
library(frenchdata)
library(lubridate)
library(RSQLite)
library(dbplyr)
library(moments)
library(writexl)

# == Step 2: Data Download =============================================================

start_date <- as.Date("2015-10-01")
end_date   <- as.Date("2025-09-01")

# ── 2a: Stock Characteristics from Open Source Asset Pricing ───────────────
# ── Step 2a: Load Stock Characteristics ───────────────────────────────────

signal_path <- "/home/hchiashi/EAP/EAP_Project/"

signal_files <- list(
  "VarCF"               = "VarCF.csv",
  "Cash"                = "Cash.csv",
  "ShareIss1Y"          = "ShareIss1Y.csv",
  "ShareIss5Y"          = "ShareIss5Y.csv",
  "PM"                  = "PM.csv",
  "EP"                  = "EP.csv",
  "XFIN"                = "XFIN.csv",
  "ZScore"              = "ZScore.csv",
  "ForecastDispersion"  = "ForecastDispersion.csv"
)

# Read and merge all signals into one dataframe
if (!exists("ap_data")) {
  cat("ap_data not found in environment. Loading from CSVs...\n")
  ap_data <- signal_files |>
    purrr::imap(function(file, signal) {
      cat(sprintf("Loading signal %d / %d: %s\n",
                  which(names(signal_files) == signal),
                  length(signal_files), signal))
      read.csv(paste0(signal_path, file)) |>
        mutate(date = as.Date(paste0(yyyymm, "01"), format = "%Y%m%d")) |>
        filter(date >= start_date, date <= end_date) |>
        select(permno, date, all_of(signal))
    }) |>
    purrr::reduce(function(x, y) {
      cat(sprintf("Merging... current rows: %d\n", nrow(x)))
      full_join(x, y, by = c("permno", "date"))
    })
  cat("ap_data loaded. Rows:", nrow(ap_data), "\n")
} else {
  cat("ap_data already exists in environment. Skipping load.\n")
}

# ── 2b: S&P 500 Returns from Yahoo Finance ────────────────────────────────
sp500_raw <- download_data(
  type      = "stock_prices",
  symbols   = "^GSPC",
  start_date = start_date,
  end_date   = end_date
)

sp500 <- sp500_raw |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date) |>
  summarise(sp500_ret = last(adjusted_close) / first(adjusted_close) - 1, .groups = "drop")

# ── 2c: VIX from Yahoo Finance ────────────────────────────────────────────
vix_raw <- download_data(
  type       = "stock_prices",
  symbols    = "^VIX",
  start_date = start_date,
  end_date   = end_date
)

vix <- vix_raw |>
  mutate(date = floor_date(date, "month")) |>
  group_by(date) |>
  summarise(vix = last(adjusted_close), .groups = "drop") |>
  mutate(sigma_market = (vix / 100) / sqrt(12))

# ── 2d: Fama-French Factors from Ken French Library ───────────────────────
ff_factors_raw <- download_french_data("Fama/French 5 Factors (2x3)")

ff5 <- ff_factors_raw$subsets$data[[1]] |>
  mutate(date = floor_date(ym(date), "month")) |>
  filter(date >= start_date, date <= end_date) |>
  rename(
    mkt_rf = `Mkt-RF`,
    smb    = SMB,
    hml    = HML,
    rmw    = RMW,
    cma    = CMA,
    rf     = RF
  ) |>
  mutate(across(mkt_rf:rf, ~ . / 100))

# Momentum factor for FFC4
mom_raw <- download_french_data("Momentum Factor (Mom)")

mom <- mom_raw$subsets$data[[1]] |>
  mutate(date = floor_date(ym(date), "month")) |>
  filter(date >= start_date, date <= end_date) |>
  rename(mom = Mom) |>
  mutate(mom = mom / 100)


# ── 2e: Merge All Data ────────────────────────────────────────────────────
factors <- ff5 |>
  left_join(mom, by = "date") |>
  left_join(sp500, by = "date") |>
  left_join(vix, by = "date")

# ── Load CRSP Monthly Stock Data from SQLite ──────────────────────────────
db_path      <- "/home/shared/data/tidy_finance.sqlite"
tidy_finance <- dbConnect(SQLite(), db_path, extended_types = TRUE)

if (!exists("crsp_monthly")) {
  cat("Downloading CRSP data...\n")
  crsp_monthly <- tbl(tidy_finance, "crsp_monthly") |>
    select(permno, date, ret, ret_excess, prc, mktcap_lag) |>
    collect() |>
    mutate(
      date      = as.Date(date),
      log_price = log(abs(prc))        # prc can be negative in CRSP (bid/ask average)
    ) |>
    filter(date >= start_date, date <= end_date) |>
    select(permno, date, ret, ret_excess, log_price)
  cat("Done. Rows:", nrow(crsp_monthly), "\n")
} else {
  cat("crsp_monthly already exists. Skipping.\n")
}

# ── Merge Stock Returns with Characteristics ──────────────────────────────
if (!exists("stock_data")) {
  cat("Building stock_data...\n")
  stock_data <- crsp_monthly |>
    inner_join(ap_data, by = c("permno", "date"))
  cat("Done. Rows:", nrow(stock_data), "\n")
} else {
  cat("stock_data already exists. Skipping.\n")
}


# == Step 3: Construct Long-Short Portfolios ===================================

characteristics <- c(
  "VarCF", "Cash", "ShareIss1Y", "ShareIss5Y",
  "PM", "EP", "XFIN", "ZScore", "ForecastDispersion", "log_price"
)

# High signal = LOW return → long bottom, short top
negative_chars <- c(
  "VarCF", "ShareIss1Y", "ShareIss5Y",
  "XFIN", "ForecastDispersion", "Cash"
)

# High signal = HIGH return → long top, short bottom
# Cash, PM, EP stay in normal direction

# ── 3a: Sort into Deciles and Construct L-S Portfolio Returns ─────────────
if (!exists("ls_portfolios")) {
  cat("Constructing long-short portfolios...\n")
  
  ls_portfolios <- purrr::map(characteristics, function(char) {
    cat(sprintf("Processing: %s\n", char))
    
    stock_data |>
      filter(!is.na(.data[[char]]), !is.na(ret_excess)) |>
      group_by(date) |>
      mutate(decile = ntile(.data[[char]], 10)) |>
      ungroup() |>
      group_by(date, decile) |>
      summarise(
        port_ret = mean(ret_excess, na.rm = TRUE),
        n_firms  = n(),
        .groups  = "drop"
      ) |>
      filter(decile %in% c(1, 10)) |>
      pivot_wider(
        names_from  = decile,
        values_from = c(port_ret, n_firms)
      ) |>
      mutate(
        ls_ret = if (char %in% negative_chars) {
          port_ret_1 - port_ret_10
        } else {
          port_ret_10 - port_ret_1
        },
        characteristic = char
      ) |>
      select(date, characteristic, ls_ret, n_firms_1, n_firms_10)
  }) |>
    purrr::list_rbind()
  
  cat("Done. Rows:", nrow(ls_portfolios), "\n")
} else {
  cat("ls_portfolios already exists. Skipping.\n")
}

# ── 3b: Summary Statistics ────────────────────────────────────────────────
ls_summary <- ls_portfolios |>
  group_by(characteristic) |>
  summarise(
    mean_ret = mean(ls_ret, na.rm = TRUE) * 100,
    sd_ret   = sd(ls_ret, na.rm = TRUE) * 100,
    t_stat   = mean(ls_ret, na.rm = TRUE) / (sd(ls_ret, na.rm = TRUE) / sqrt(n())),
    n_months = n(),
    .groups  = "drop"
  )

print(ls_summary)

# ── 3c: Check Realized Betas of L-S Portfolios ────────────────────────────
realized_betas <- ls_portfolios |>
  left_join(factors |> select(date, sp500_ret), by = "date") |>
  group_by(characteristic) |>
  summarise(
    realized_beta = cov(ls_ret, sp500_ret, use = "pairwise.complete.obs") /
      var(sp500_ret, na.rm = TRUE),
    correlation   = cor(ls_ret, sp500_ret, use = "pairwise.complete.obs"),
    .groups       = "drop"
  )

print(realized_betas)


# == Step 4: GARCH(1,1) Volatility and FGK Implied Beta =======================

# GARCH(1,1) specification
garch_spec <- ugarchspec(
  variance.model     = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model         = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)

# Rolling window size
window_size <- 36

# ── 4a: Estimate GARCH(1,1) Volatility for Each Portfolio ─────────────────
if (!exists("garch_results")) {
  cat("Estimating GARCH(1,1) for each characteristic...\n")
  
  garch_results <- purrr::map(unique(ls_portfolios$characteristic), function(char) {
    cat(sprintf("\nProcessing GARCH: %s\n", char))
    
    port_data <- ls_portfolios |>
      filter(characteristic == char) |>
      arrange(date)
    
    port_ret <- port_data$ls_ret
    dates    <- port_data$date
    n        <- length(port_ret)
    sigma_port <- rep(NA, n)
    
    for (i in window_size:n) {
      window_ret <- port_ret[(i - window_size + 1):i]
      
      tryCatch({
        fit        <- ugarchfit(spec = garch_spec, data = window_ret, solver = "hybrid")
        sigma_port[i] <- tail(sigma(fit), 1)
      }, error = function(e) NULL)
      
      if (i %% 10 == 0 || i == n) {
        cat(sprintf("  Window %d / %d (%.0f%%)\n", i, n, 100 * i / n))
      }
    }
    
    tibble(date = dates, characteristic = char, sigma_port = sigma_port)
  }) |>
    purrr::list_rbind()
  
  cat("\nGARCH estimation complete.\n")
} else {
  cat("garch_results already exists. Skipping.\n")
}

# ── 4b: Rolling Correlation with S&P 500 ──────────────────────────────────
if (!exists("rho_data")) {
  cat("Estimating rolling correlations...\n")
  
  rho_data <- purrr::map(unique(ls_portfolios$characteristic), function(char) {
    cat(sprintf("Processing correlation: %s\n", char))
    
    port_data <- ls_portfolios |>
      filter(characteristic == char) |>
      arrange(date) |>
      left_join(factors |> select(date, sp500_ret, sigma_market), by = "date")
    
    n   <- nrow(port_data)
    rho <- rep(NA, n)
    
    for (i in window_size:n) {
      window <- port_data[(i - window_size + 1):i, ]
      if (sum(!is.na(window$ls_ret) & !is.na(window$sp500_ret)) >= 24) {
        rho[i] <- cor(window$ls_ret, window$sp500_ret,
                      use = "pairwise.complete.obs")
      }
    }
    
    port_data |>
      mutate(rho = rho) |>
      select(date, characteristic, rho)
  }) |>
    purrr::list_rbind()
  
  cat("Rolling correlation done.\n")
} else {
  cat("rho_data already exists. Skipping.\n")
}

# ── 4c: Compute FGK Implied Beta ──────────────────────────────────────────
beta_data <- garch_results |>
  left_join(rho_data, by = c("date", "characteristic")) |>
  left_join(factors |> select(date, sigma_market), by = "date") |>
  mutate(
    # FGK: implied beta = rho * (sigma_portfolio / sigma_market)
    implied_beta = rho * (sigma_port / sigma_market)
  ) |>
  group_by(characteristic) |>
  mutate(implied_beta_lag = lag(implied_beta)) |>  # lag to avoid look-ahead bias
  ungroup()

# ── 4d: Summary of Implied Betas ──────────────────────────────────────────
beta_summary <- beta_data |>
  group_by(characteristic) |>
  summarise(
    mean_beta = mean(implied_beta_lag, na.rm = TRUE),
    sd_beta   = sd(implied_beta_lag, na.rm = TRUE),
    min_beta  = min(implied_beta_lag, na.rm = TRUE),
    max_beta  = max(implied_beta_lag, na.rm = TRUE),
    .groups   = "drop"
  )

print(beta_summary)

# ── Sanity Check: GARCH vs Realized Volatility ────────────────────────────
realized_vol <- ls_portfolios |>
  group_by(characteristic) |>
  arrange(date) |>
  mutate(
    realized_vol = zoo::rollapply(
      ls_ret,
      width      = window_size,
      FUN        = sd,
      fill       = NA,
      align      = "right"
    )
  ) |>
  ungroup() |>
  select(date, characteristic, realized_vol)

# Merge with GARCH estimates
vol_comparison <- garch_results |>
  left_join(realized_vol, by = c("date", "characteristic")) |>
  filter(!is.na(sigma_port), !is.na(realized_vol))

# Summary comparison
vol_comparison |>
  group_by(characteristic) |>
  summarise(
    mean_garch    = mean(sigma_port, na.rm = TRUE),
    mean_realized = mean(realized_vol, na.rm = TRUE),
    correlation   = cor(sigma_port, realized_vol, use = "pairwise.complete.obs"),
    .groups       = "drop"
  ) |>
  print()

# == Step 5: Beta-Neutral Portfolio Construction ================================

# ── 5a: Merge L-S Returns with Lagged Implied Beta ────────────────────────
if (!exists("neutral_portfolios")) {
  cat("Constructing beta-neutral portfolios...\n")
  
  neutral_portfolios <- ls_portfolios |>
    left_join(
      beta_data |> select(date, characteristic, implied_beta_lag),
      by = c("date", "characteristic")
    ) |>
    left_join(
      factors |> select(date, sp500_ret, rf),
      by = "date"
    ) |>
    mutate(
      # Buy S&P 500 to neutralize negative beta exposure
      hedge_ret          = -implied_beta_lag * sp500_ret,
      neutral_ret        = ls_ret + hedge_ret,
      neutral_ret_excess = neutral_ret - rf
    )
  
  cat("Done. Rows:", nrow(neutral_portfolios), "\n")
} else {
  cat("neutral_portfolios already exists. Skipping.\n")
}

# ── 5b: Summary of Beta-Neutral Portfolio Returns ─────────────────────────
neutral_summary <- neutral_portfolios |>
  filter(!is.na(implied_beta_lag)) |>
  group_by(characteristic) |>
  summarise(
    mean_ret = mean(neutral_ret_excess, na.rm = TRUE) * 100,
    sd_ret   = sd(neutral_ret_excess, na.rm = TRUE) * 100,
    t_stat   = mean(neutral_ret_excess, na.rm = TRUE) /
      (sd(neutral_ret_excess, na.rm = TRUE) / sqrt(n())),
    n_months = n(),
    .groups  = "drop"
  )

cat("\nBeta-Neutral Portfolio Summary:\n")
print(neutral_summary)

# ── 5c: Verify Beta Neutrality ────────────────────────────────────────────
beta_check <- neutral_portfolios |>
  filter(!is.na(implied_beta_lag)) |>
  group_by(characteristic) |>
  summarise(
    residual_beta = cov(neutral_ret_excess, sp500_ret,
                        use = "pairwise.complete.obs") /
      var(sp500_ret, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nResidual betas after hedging (should be near zero):\n")
print(beta_check)

# == Step 6: Alpha Estimation ==================================================

# Helper: Newey-West alpha regression
nw_alpha <- function(ret, factors_df, formula_str, model_name, lag = 6) {
  df <- bind_cols(tibble(ret = ret), factors_df) |> drop_na()
  fit <- lm(as.formula(formula_str), data = df)
  ct  <- coeftest(fit, vcov. = NeweyWest(fit, lag = lag, 
                                         prewhite = FALSE, adjust = TRUE))
  tibble(
    model     = model_name,
    alpha     = ct["(Intercept)", "Estimate"],
    alpha_pct = ct["(Intercept)", "Estimate"] * 100,
    t_stat    = ct["(Intercept)", "t value"],
    p_value   = ct["(Intercept)", "Pr(>|t|)"]
  )
}

# ── 6a: Run CAPM, FF3, FFC4, FF5 for Each Characteristic ──────────────────
if (!exists("alpha_results")) {
  cat("Estimating alphas...\n")
  
  alpha_results <- purrr::map(unique(neutral_portfolios$characteristic), function(char) {
    cat(sprintf("Processing alpha: %s\n", char))
    
    port <- neutral_portfolios |>
      filter(characteristic == char, !is.na(implied_beta_lag)) |>
      left_join(factors, by = "date") |>
      drop_na(neutral_ret_excess, mkt_rf, smb, hml, rmw, cma, mom)
    
    ret <- port$neutral_ret_excess
    
    # CAPM
    capm <- nw_alpha(ret, port |> select(mkt_rf),
                     "ret ~ mkt_rf", "CAPM")
    
    # FF3
    ff3 <- nw_alpha(ret, port |> select(mkt_rf, smb, hml),
                    "ret ~ mkt_rf + smb + hml", "FF3")
    
    # FFC4
    ffc4 <- nw_alpha(ret, port |> select(mkt_rf, smb, hml, mom),
                     "ret ~ mkt_rf + smb + hml + mom", "FFC4")
    
    # FF5
    ff5_res <- nw_alpha(ret, port |> select(mkt_rf, smb, hml, rmw, cma),
                        "ret ~ mkt_rf + smb + hml + rmw + cma", "FF5")
    
    bind_rows(capm, ff3, ffc4, ff5_res) |>
      mutate(characteristic = char)
  }) |>
    purrr::list_rbind()
  
  cat("Alpha estimation complete.\n")
} else {
  cat("alpha_results already exists. Skipping.\n")
}

# ── 6b: Print Alpha Table ─────────────────────────────────────────────────
alpha_table <- alpha_results |>
  select(characteristic, model, alpha_pct, t_stat, p_value) |>
  mutate(
    alpha_pct = round(alpha_pct, 3),
    t_stat    = round(t_stat, 3),
    sig       = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    )
  ) |>
  select(-p_value)

print(alpha_table, n = 40)


# == Robustness Check: Value-Weighted Portfolios ================================

# pull market cap data from crsp_monthly for value-weighting
mktcap_data <- tbl(tidy_finance, "crsp_monthly") |>
  select(permno, date, mktcap_lag) |>
  collect() |>
  mutate(date = as.Date(date)) |>
  filter(date >= start_date, date <= end_date)

stock_data <- stock_data |>
  left_join(mktcap_data, by = c("permno", "date"))

# ── R1: Construct Value-Weighted L-S Portfolios ───────────────────────────
if (!exists("ls_portfolios_vw")) {
  cat("Constructing value-weighted long-short portfolios...\n")
  
  ls_portfolios_vw <- purrr::map(characteristics, function(char) {
    cat(sprintf("Processing VW: %s\n", char))
    
    stock_data |>
      filter(!is.na(.data[[char]]), !is.na(ret_excess), !is.na(mktcap_lag)) |>
      group_by(date) |>
      mutate(decile = ntile(.data[[char]], 10)) |>
      ungroup() |>
      group_by(date, decile) |>
      summarise(
        port_ret = weighted.mean(ret_excess, w = mktcap_lag, na.rm = TRUE),
        n_firms  = n(),
        .groups  = "drop"
      ) |>
      filter(decile %in% c(1, 10)) |>
      pivot_wider(
        names_from  = decile,
        values_from = c(port_ret, n_firms)
      ) |>
      mutate(
        ls_ret = if (char %in% negative_chars) {
          port_ret_1 - port_ret_10
        } else {
          port_ret_10 - port_ret_1
        },
        characteristic = char
      ) |>
      select(date, characteristic, ls_ret, n_firms_1, n_firms_10)
  }) |>
    purrr::list_rbind()
  
  cat("Done. Rows:", nrow(ls_portfolios_vw), "\n")
} else {
  cat("ls_portfolios_vw already exists. Skipping.\n")
}

# ── R2: Summary Statistics ────────────────────────────────────────────────
ls_summary_vw <- ls_portfolios_vw |>
  group_by(characteristic) |>
  summarise(
    mean_ret = mean(ls_ret, na.rm = TRUE) * 100,
    sd_ret   = sd(ls_ret, na.rm = TRUE) * 100,
    t_stat   = mean(ls_ret, na.rm = TRUE) /
      (sd(ls_ret, na.rm = TRUE) / sqrt(n())),
    n_months = n(),
    .groups  = "drop"
  )

cat("\nVW Portfolio Summary:\n")
print(ls_summary_vw)

# ── R3: GARCH on VW Portfolios ────────────────────────────────────────────
if (!exists("garch_results_vw")) {
  cat("Estimating GARCH(1,1) for VW portfolios...\n")
  
  garch_results_vw <- purrr::map(unique(ls_portfolios_vw$characteristic), function(char) {
    cat(sprintf("\nProcessing GARCH VW: %s\n", char))
    
    port_data <- ls_portfolios_vw |>
      filter(characteristic == char) |>
      arrange(date)
    
    port_ret   <- port_data$ls_ret
    dates      <- port_data$date
    n          <- length(port_ret)
    sigma_port <- rep(NA, n)
    
    for (i in window_size:n) {
      window_ret <- port_ret[(i - window_size + 1):i]
      tryCatch({
        fit           <- ugarchfit(spec = garch_spec, data = window_ret, solver = "hybrid")
        sigma_port[i] <- tail(sigma(fit), 1)
      }, error = function(e) NULL)
      
      if (i %% 10 == 0 || i == n) {
        cat(sprintf("  Window %d / %d (%.0f%%)\n", i, n, 100 * i / n))
      }
    }
    
    tibble(date = dates, characteristic = char, sigma_port = sigma_port)
  }) |>
    purrr::list_rbind()
  
  cat("\nGARCH VW estimation complete.\n")
} else {
  cat("garch_results_vw already exists. Skipping.\n")
}

# ── R4: Rolling Correlation for VW Portfolios ─────────────────────────────
if (!exists("rho_data_vw")) {
  cat("Estimating rolling correlations for VW portfolios...\n")
  
  rho_data_vw <- purrr::map(unique(ls_portfolios_vw$characteristic), function(char) {
    cat(sprintf("Processing correlation VW: %s\n", char))
    
    port_data <- ls_portfolios_vw |>
      filter(characteristic == char) |>
      arrange(date) |>
      left_join(factors |> select(date, sp500_ret), by = "date")
    
    n   <- nrow(port_data)
    rho <- rep(NA, n)
    
    for (i in window_size:n) {
      window <- port_data[(i - window_size + 1):i, ]
      if (sum(!is.na(window$ls_ret) & !is.na(window$sp500_ret)) >= 24) {
        rho[i] <- cor(window$ls_ret, window$sp500_ret,
                      use = "pairwise.complete.obs")
      }
    }
    
    port_data |>
      mutate(rho = rho) |>
      select(date, characteristic, rho)
  }) |>
    purrr::list_rbind()
  
  cat("Rolling correlation VW done.\n")
} else {
  cat("rho_data_vw already exists. Skipping.\n")
}

# ── R5: FGK Implied Beta for VW Portfolios ────────────────────────────────
beta_data_vw <- garch_results_vw |>
  left_join(rho_data_vw, by = c("date", "characteristic")) |>
  left_join(factors |> select(date, sigma_market), by = "date") |>
  mutate(implied_beta = rho * (sigma_port / sigma_market)) |>
  group_by(characteristic) |>
  mutate(implied_beta_lag = lag(implied_beta)) |>
  ungroup()

# ── R6: Beta-Neutral VW Portfolios ────────────────────────────────────────
if (!exists("neutral_portfolios_vw")) {
  neutral_portfolios_vw <- ls_portfolios_vw |>
    left_join(
      beta_data_vw |> select(date, characteristic, implied_beta_lag),
      by = c("date", "characteristic")
    ) |>
    left_join(factors |> select(date, sp500_ret, rf), by = "date") |>
    mutate(
      hedge_ret          = -implied_beta_lag * sp500_ret,
      neutral_ret        = ls_ret + hedge_ret,
      neutral_ret_excess = neutral_ret - rf
    )
  cat("VW beta-neutral portfolios done.\n")
} else {
  cat("neutral_portfolios_vw already exists. Skipping.\n")
}

# ── R7: Alpha Estimation for VW Portfolios ────────────────────────────────
if (!exists("alpha_results_vw")) {
  cat("Estimating VW alphas...\n")
  
  alpha_results_vw <- purrr::map(unique(neutral_portfolios_vw$characteristic), function(char) {
    cat(sprintf("Processing alpha VW: %s\n", char))
    
    port <- neutral_portfolios_vw |>
      filter(characteristic == char, !is.na(implied_beta_lag)) |>
      left_join(factors, by = "date") |>
      drop_na(neutral_ret_excess, mkt_rf, smb, hml, rmw, cma, mom)
    
    ret <- port$neutral_ret_excess
    
    bind_rows(
      nw_alpha(ret, port |> select(mkt_rf),
               "ret ~ mkt_rf", "CAPM"),
      nw_alpha(ret, port |> select(mkt_rf, smb, hml),
               "ret ~ mkt_rf + smb + hml", "FF3"),
      nw_alpha(ret, port |> select(mkt_rf, smb, hml, mom),
               "ret ~ mkt_rf + smb + hml + mom", "FFC4"),
      nw_alpha(ret, port |> select(mkt_rf, smb, hml, rmw, cma),
               "ret ~ mkt_rf + smb + hml + rmw + cma", "FF5")
    ) |>
      mutate(characteristic = char)
  }) |>
    purrr::list_rbind()
  
  cat("VW alpha estimation complete.\n")
} else {
  cat("alpha_results_vw already exists. Skipping.\n")
}

# ── R8: Compare EW vs VW Alpha Tables ─────────────────────────────────────
alpha_table_vw <- alpha_results_vw |>
  select(characteristic, model, alpha_pct, t_stat, p_value) |>
  mutate(
    alpha_pct = round(alpha_pct, 3),
    t_stat    = round(t_stat, 3),
    sig       = case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.10 ~ "*",
      TRUE           ~ ""
    )
  ) |>
  select(-p_value)

cat("\nVW Alpha Table:\n")
print(alpha_table_vw, n = 40)

cat("\nEW Alpha Table (for comparison):\n")
print(alpha_table, n = 40)

# == Table Construction and Export =============================================

# ── Table 1: Descriptive Statistics of Characteristics ────────────────────
table1 <- stock_data |>
  select(all_of(characteristics)) |>
  pivot_longer(everything(), names_to = "Characteristic", values_to = "value") |>
  group_by(Characteristic) |>
  summarise(
    Mean     = round(mean(value,             na.rm = TRUE), 4),
    Median   = round(median(value,           na.rm = TRUE), 4),
    SD       = round(sd(value,               na.rm = TRUE), 4),
    Skewness = round(skewness(value,         na.rm = TRUE), 4),
    Min      = round(min(value,              na.rm = TRUE), 4),
    Max      = round(max(value,              na.rm = TRUE), 4),
    N_Obs    = sum(!is.na(value)),
    .groups  = "drop"
  )

# ── Table 2: EW L-S Portfolio Returns ─────────────────────────────────────
table2 <- ls_summary |>
  rename(
    Characteristic = characteristic,
    Mean_Ret_pct   = mean_ret,
    SD_Ret_pct     = sd_ret,
    T_Stat         = t_stat,
    N_Months       = n_months
  )

# ── Table 3: VW L-S Portfolio Returns ─────────────────────────────────────
table3 <- ls_summary_vw |>
  rename(
    Characteristic = characteristic,
    Mean_Ret_pct   = mean_ret,
    SD_Ret_pct     = sd_ret,
    T_Stat         = t_stat,
    N_Months       = n_months
  )

# ── Table 4: FGK Implied Beta Summary ─────────────────────────────────────
table4 <- beta_summary |>
  rename(
    Characteristic = characteristic,
    Mean_Beta      = mean_beta,
    SD_Beta        = sd_beta,
    Min_Beta       = min_beta,
    Max_Beta       = max_beta
  ) |>
  mutate(across(where(is.numeric), ~ round(., 4)))

# ── Table 5: Beta Neutrality Check ────────────────────────────────────────
table5 <- realized_betas |>
  left_join(beta_check, by = "characteristic") |>
  rename(
    Characteristic  = characteristic,
    Beta_Before     = realized_beta,
    Correlation     = correlation,
    Residual_Beta   = residual_beta
  ) |>
  mutate(across(where(is.numeric), ~ round(., 4)))

# ── Table 6: EW Alpha Table ───────────────────────────────────────────────
table6 <- alpha_table |>
  rename(
    Characteristic = characteristic,
    Model          = model,
    Alpha_pct      = alpha_pct,
    T_Stat         = t_stat,
    Significance   = sig
  )

# ── Table 7: VW Alpha Table ───────────────────────────────────────────────
table7 <- alpha_table_vw |>
  rename(
    Characteristic = characteristic,
    Model          = model,
    Alpha_pct      = alpha_pct,
    T_Stat         = t_stat,
    Significance   = sig
  )

# ── Table 8: GARCH vs Realized Volatility ─────────────────────────────────
table8 <- vol_comparison |>
  group_by(characteristic) |>
  summarise(
    Mean_GARCH_Vol    = round(mean(sigma_port,    na.rm = TRUE), 4),
    Mean_Realized_Vol = round(mean(realized_vol,  na.rm = TRUE), 4),
    Correlation       = round(cor(sigma_port, realized_vol,
                                  use = "pairwise.complete.obs"), 4),
    .groups = "drop"
  ) |>
  rename(Characteristic = characteristic)

# ── Export to Excel ───────────────────────────────────────────────────────
dir.create("/home/hchiashi/EAP/EAP_Project/tables/", showWarnings = FALSE, recursive = TRUE)

write_xlsx(
  list(
    "1_Descriptive"     = table1,
    "2_EW_LS_Returns"   = table2,
    "3_VW_LS_Returns"   = table3,
    "4_Implied_Beta"    = table4,
    "5_Beta_Neutrality" = table5,
    "6_EW_Alpha"        = table6,
    "7_VW_Alpha"        = table7,
    "8_GARCH_vs_Real"   = table8
  ),
  path = "/home/hchiashi/EAP/EAP_Project/tables/results.xlsx"
)

cat("Excel file exported successfully.\n")