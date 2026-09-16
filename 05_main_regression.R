# 05_main_regression.R
# Purpose: Estimate the main fixed-effects regression models.
#
# Input: df created by 04_abnormal_activity.R
# Output: main_results.xlsx

library(fixest)
library(writexl)

# -------------------------------------------------------------------
# Main models
# -------------------------------------------------------------------

M1 <- feols(
  abn_cfo ~
    annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

M2 <- feols(
  abn_prod ~
    annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

M3 <- feols(
  abn_disexp ~
    annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

# -------------------------------------------------------------------
# Export main regression table
# -------------------------------------------------------------------

main_table <- etable(
  M1, M2, M3,
  cluster = "Symbol",
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)

write_xlsx(
  main_table,
  "../results/main_results.xlsx"
)
