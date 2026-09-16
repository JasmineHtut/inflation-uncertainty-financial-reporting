# 04_abnormal_activity.R
# Purpose: Estimate abnormal operating cash flow, production,
# discretionary expenses, R&D, and SG&A.
#
# Input: df created by 03_variable_construction.R
# Output: df with abnormal activity measures.

library(dplyr)

# -------------------------------------------------------------------
# 1. Estimate abnormal CFO, production, and discretionary expenses
#    within industry-year groups.
# -------------------------------------------------------------------

df <- df %>%
  group_by(IND, FYR) %>%
  mutate(
    abn_cfo = resid(
      lm(
        CFO / AT_l1 ~
          I(1 / AT_l1) +
          SALES / AT_l1 +
          dSALES / AT_l1,
        na.action = na.exclude
      )
    ),
    abn_prod = resid(
      lm(
        PROD / AT_l1 ~
          I(1 / AT_l1) +
          SALES / AT_l1 +
          dSALES / AT_l1 +
          dSALES_l1 / AT_l1,
        na.action = na.exclude
      )
    ),
    abn_disexp = resid(
      lm(
        DISEXP / AT_l1 ~
          I(1 / AT_l1) +
          SALES_l1 / AT_l1,
        na.action = na.exclude
      )
    )
  ) %>%
  ungroup()

# -------------------------------------------------------------------
# 2. Winsorize abnormal activity measures
# -------------------------------------------------------------------

winsor_abnormal <- c(
  "abn_cfo",
  "abn_prod",
  "abn_disexp"
)

df[winsor_abnormal] <- lapply(
  df[winsor_abnormal],
  function(x) Winsorize(
    x,
    quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  )
)

# -------------------------------------------------------------------
# 3. Estimate abnormal R&D and abnormal SG&A
# -------------------------------------------------------------------

df <- df %>%
  group_by(IND, FYR) %>%
  mutate(
    abn_rnd = resid(
      lm(
        RND / AT_l1 ~
          I(1 / AT_l1) +
          SALES_l1 / AT_l1,
        na.action = na.exclude
      )
    ),
    abn_sga = resid(
      lm(
        SGA / AT_l1 ~
          I(1 / AT_l1) +
          SALES / AT_l1,
        na.action = na.exclude
      )
    )
  ) %>%
  ungroup()

# The resulting object is used by 05_main_regression.R and
# 08_additional_analysis.R
