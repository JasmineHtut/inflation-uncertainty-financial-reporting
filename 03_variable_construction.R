# 03_variable_construction.R
# Purpose: Construct financial, market, lagged, and control variables.
#
# Input: df created by 02_data_cleaning.R
# Output: df with analysis variables.

library(dplyr)

df <- df %>%
  group_by(Symbol) %>%
  arrange(FYR, .by_group = TRUE) %>%
  mutate(
    SIZE = log(AT),
    AT_l1 = lag(AT),
    ROA = NI / AT,
    SALES_l1 = lag(SALES),
    dSALES = SALES - SALES_l1,
    dSALES_l1 = lag(dSALES),
    GROWTH = dSALES / SALES_l1,
    MarketCap = (SharePrice * ShareNo) / 1000,
    LEV = TotalLiability / AT,
    OperLev = PPE / AT,
    PROD = COGS + (Inventory - lag(Inventory)),
    DISEXP = SGA + RND,
    TA = NI - CFO,
    AR_l1 = lag(AR),
    TA_scaled = TA / AT_l1,
    dAR = AR - lag(AR),
    InvIntensity = Inventory / AT,
    LEV_l1 = lag(LEV),
    OperLev_l1 = lag(OperLev),
    InvIntensity_l1 = lag(InvIntensity),
    MTB = MarketCap / TotalEquity,
    BIG4 = ifelse(
      Auditor %in% c(
        "삼일회계법인",
        "삼정회계법인",
        "한영회계법인",
        "안진회계법인"
      ),
      1, 0
    )
  ) %>%
  ungroup()

# -------------------------------------------------------------------
# Sample restrictions for variables required by the main analysis
# -------------------------------------------------------------------

required_analysis <- c(
  "CFO", "AT_l1", "SALES", "SALES_l1", "dSALES", "dSALES_l1",
  "PROD", "DISEXP", "IND", "FYR", "AT", "AR", "AR_l1", "PPE",
  "TotalLiability", "TotalEquity", "NI", "MTB", "GROWTH"
)

df <- df %>%
  filter(if_all(all_of(required_analysis), ~ !is.na(.)))

# Keep industry-year cells with at least 15 observations.
df <- df %>%
  group_by(IND, FYR) %>%
  filter(n() >= 15) %>%
  ungroup()

# -------------------------------------------------------------------
# Winsorize variables used for scaling / sample construction
# -------------------------------------------------------------------

library(DescTools)

raw_scaling_vars <- c(
  "CFO", "PROD", "DISEXP", "SALES",
  "SALES_l1", "dSALES", "dSALES_l1", "AT_l1"
)

df[raw_scaling_vars] <- lapply(
  df[raw_scaling_vars],
  function(x) Winsorize(
    x,
    quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  )
)

# Re-check industry-year sample size after preprocessing.
df <- df %>%
  group_by(IND, FYR) %>%
  filter(n() >= 15) %>%
  ungroup()

# -------------------------------------------------------------------
# Winsorize analysis variables after they are constructed.
# -------------------------------------------------------------------

winsor_vars <- c(
  "OperLev_l1", "LEV", "LEV_l1", "InvIntensity_l1",
  "ROA", "SIZE", "MTB", "GROWTH"
)

df[winsor_vars] <- lapply(
  df[winsor_vars],
  function(x) Winsorize(
    x,
    quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  )
)

# Save an intermediate object if desired:
# saveRDS(df, "../data/analysis_panel.rds")

# The resulting object is used by 04_abnormal_activity.R
