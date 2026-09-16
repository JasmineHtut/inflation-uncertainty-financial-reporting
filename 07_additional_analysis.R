# 07_additional_analysis.R
# Purpose: Additional analyses using abnormal R&D, abnormal SG&A,
# industry R&D intensity, and discretionary accruals,
# Conduct alternative uncertainty and macroeconomic-control
#
# Input: df created by 04_abnormal_activity.R
# Output: additional_results.xlsx

library(dplyr)
library(fixest)
library(writexl)

# -------------------------------------------------------------------
# 1. Abnormal R&D and abnormal SG&A models
# -------------------------------------------------------------------

abn_rnd_model <- feols(
  abn_rnd ~ annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

abn_sga_model <- feols(
  abn_sga ~ annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

rnd_sga <- etable(
  abn_rnd_model, abn_sga_model,
  cluster = "Symbol",
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)

write_xlsx(rnd_sga,"Abnormal_RnD_SGA")

# 2. High-uncertainty indicator specification
# -------------------------------------------------------------------

threshold <- quantile(
  df$annual_garch_unc,
  0.75,
  na.rm = TRUE
)

df <- df %>%
  mutate(
    HIGH_UNC = ifelse(
      annual_garch_unc > threshold,
      1,
      0
    )
  )

M1_dummy <- feols(
  abn_cfo ~ HIGH_UNC + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

M2_dummy <- feols(
  abn_prod ~ HIGH_UNC + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

M3_dummy <- feols(
  abn_disexp ~ HIGH_UNC + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)
Table_dummy = etable(M1_dummy, M2_dummy, M3_dummy,
                     cluster = "Symbol",
                     se.below = TRUE,
                     fitstat = c("n","r2","wr2"))
write_xlsx(Table_dummy,"Dummy_variables.xlsx")

# -------------------------------------------------------------------
# 3. Split analysis by industry R&D intensity
# -------------------------------------------------------------------

df <- df %>%
  group_by(IND) %>%
  mutate(
    ind_rnd_median = median(RND / AT, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    HIGH_RND_IND = ifelse(
      (RND / AT) > ind_rnd_median,
      1,
      0
    )
  )

M_Split_rnd <- feols(
  abn_rnd ~ annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  split = ~ HIGH_RND_IND,
  cluster = ~ Symbol
)

M_Split_sga <- feols(
  abn_sga ~ annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  split = ~ HIGH_RND_IND,
  cluster = ~ Symbol
)

rnd_ind2 <- etable(
  M_Split_rnd, M_Split_sga,
  cluster = "Symbol",
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)

write_xlsx(rnd_ind2,"Industry_RnD_Split")

# -------------------------------------------------------------------
# 4. High-uncertainty indicator specification
# -------------------------------------------------------------------

threshold <- quantile(
  df$annual_garch_unc,
  0.75,
  na.rm = TRUE
)

df <- df %>%
  mutate(
    HIGH_UNC = ifelse(
      annual_garch_unc > threshold,
      1,
      0
    )
  )

M1_dummy <- feols(
  abn_cfo ~ HIGH_UNC + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

M2_dummy <- feols(
  abn_prod ~ HIGH_UNC + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

M3_dummy <- feols(
  abn_disexp ~ HIGH_UNC + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH |
    Symbol,
  data = df,
  cluster = ~ Symbol
)

Table_dummy = etable(M1_dummy, M2_dummy, M3_dummy,
                     cluster = "Symbol",
                     se.below = TRUE,
                     fitstat = c("n","r2","wr2"))
write_xlsx(Table_dummy,"dummy.xlsx")

# -------------------------------------------------------------------
# 5. Discretionary accruals using the Modified Jones-style model
# -------------------------------------------------------------------

mj_model <- feols(
  TA_scaled ~
    I(1 / AT_l1) +
    (dSALES - dAR) / AT_l1 +
    PPE / AT_l1 |
    IND + FYR,
  data = df
)

df$DA <- resid(mj_model)

df["DA"] <- lapply(
  df["DA"],
  function(x) Winsorize(
    x,
    quantile(x, probs = c(0.01, 0.99), na.rm = TRUE)
  )
)

DA_M1 <- feols(
  abs(DA) ~ annual_garch_unc + annual_inf +
    LEV + SIZE + ROA + MTB + BIG4 + GROWTH |
    Symbol + IND,
  data = df,
  cluster = ~ Symbol
)

DA_M2 <- feols(
  abs(DA) ~ annual_garch_unc * LEV_l1 +
    annual_inf + LEV + SIZE + ROA + MTB + BIG4 + GROWTH |
    Symbol + IND,
  data = df,
  cluster = ~ Symbol
)

DA_M3 <- feols(
  abs(DA) ~ annual_garch_unc * CapIntensity_l1 +
    annual_inf + LEV + SIZE + ROA + MTB + BIG4 + GROWTH |
    Symbol + IND,
  data = df,
  cluster = ~ Symbol
)

DA_M4 <- feols(
  abs(DA) ~ annual_garch_unc * InvIntensity_l1 +
    annual_inf + LEV + SIZE + ROA + MTB + BIG4 + GROWTH |
    Symbol + IND,
  data = df,
  cluster = ~ Symbol
)

DA_M <- etable(
  DA_M1, DA_M2, DA_M3, DA_M4,
  cluster = "Symbol",
  se.below = TRUE,
  fitstat = c("n", "r2", "wr2")
)

write_xlsx(DA_M,"Discretionary_Accruals")

# -------------------------------------------------------------------
# 6. Macroeconomic-control models
# -------------------------------------------------------------------

M1_Macro <- feols(
  abn_cfo ~ annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH +
    GDP_growth + IR_vol |
    Symbol + IND,
  data = df,
  cluster = ~ Symbol
)

M2_Macro <- feols(
  abn_prod ~ annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH +
    GDP_growth + IR_vol |
    Symbol + IND,
  data = df,
  cluster = ~ Symbol
)

M3_Macro <- feols(
  abn_disexp ~ annual_garch_unc + annual_inf +
    SIZE + LEV + ROA + MTB + BIG4 + GROWTH +
    GDP_growth + IR_vol |
    Symbol + IND,
  data = df,
  cluster = ~ Symbol
)

Table_Macro= etable(M1_Macro, M2_Macro, M3_Macro,
                    cluster = "Symbol",
                    se.below = TRUE,
                    fitstat = c("n","r2","wr2"))
write_xlsx(Table_Macro,"Macroeconomic_Controls.xlsx")
