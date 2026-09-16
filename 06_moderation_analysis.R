# 06_moderation_analysis.R
# Purpose: Test whether leverage, capital intensity, and inventory
# intensity moderate the relationship between economic uncertainty
# and abnormal operating activities.
#
# Input: df created by 04_abnormal_activity.R
# Output: Leverage_moderation_results.xlsx, Capital_Intensity_moderation_results.xlsx, Inventory_Intensisty_moderation_results.xlsx

library(fixest)
library(writexl)

# -------------------------------------------------------------------
# 1. Continuous interaction models
# -------------------------------------------------------------------

Ex_M1 <- feols(
  abn_cfo ~ annual_garch_unc * LEV_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M2 <- feols(
  abn_cfo ~ annual_garch_unc * CapIntensity_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M3 <- feols(
  abn_cfo ~ annual_garch_unc * InvIntensity_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M4 <- feols(
  abn_prod ~ annual_garch_unc * LEV_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M5 <- feols(
  abn_prod ~ annual_garch_unc * CapIntensity_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M6 <- feols(
  abn_prod ~ annual_garch_unc * InvIntensity_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M7 <- feols(
  abn_disexp ~ annual_garch_unc * LEV_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M8 <- feols(
  abn_disexp ~ annual_garch_unc * CapIntensity_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Ex_M9 <- feols(
  abn_disexp ~ annual_garch_unc * InvIntensity_l1 +
    annual_inf + SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

# -------------------------------------------------------------------
# 2. Export interaction tables
# -------------------------------------------------------------------

Table_leverage <- etable(
  Ex_M1, Ex_M4, Ex_M7,
  cluster = "Symbol",
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)

Table_capital_intensity <- etable(
  Ex_M2, Ex_M5, Ex_M8,
  cluster = "Symbol",
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)

Table_inventory_intensity <- etable(
  Ex_M3, Ex_M6, Ex_M9,
  cluster = "Symbol",
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)



write_xlsx(Table_leverage,"Leverage_moderation_results.xlsx")
write_xlsx(Table_capital_intensity,"Capital_Intensity_moderation_results.xlsx")
write_xlsx(Table_inventory_intensity,"Inventory_Intensisty_moderation_results.xlsx")
