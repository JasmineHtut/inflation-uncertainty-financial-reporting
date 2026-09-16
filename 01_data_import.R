# 01_data_import.R
# Economic Uncertainty and Corporate Financial Reporting Behavior
# Purpose: Import source datasets, standardize variable names, and merge datasets.
#
# Expected input files in ../data/:
#   - DATA.xlsx
#   - GDP growth.xlsx
#   - IR_annual.xlsx
#   - Unc_new_2.xlsx
#
# Note: Raw firm-level data are not included in the public portfolio repository.

library(readxl)
library(dplyr)

# -------------------------------------------------------------------
# 1. Import source datasets
# -------------------------------------------------------------------

GDP <- read_xlsx("../data/GDP growth.xlsx")
IR  <- read_xlsx("../data/IR_annual.xlsx")
UNC <- read_xlsx("../data/Unc_new_2.xlsx")
df  <- read_xlsx("../data/DATA.xlsx")

# -------------------------------------------------------------------
# 2. Standardize accounting variable names
# -------------------------------------------------------------------

df <- df %>%
  rename(
    FYR = `회계년`,
    AT = `총자산(천원)`,
    CASH = `현금및현금성자산(천원)`,
    FinancialAssets = `단기금융자산(천원)`,
    AR = `매출채권(천원)`,
    Inventory = `재고자산(천원)`,
    TotalLiability = `총부채(천원)`,
    TotalEquity = `총자본(천원)`,
    SGA = `판매비와관리비(천원)`,
    NI = `당기순이익(천원)`,
    PPE = `유형자산(천원)`,
    SALES = `매출액(천원)`,
    COGS = `매출원가(천원)`,
    CFO = `영업활동으로인한현금흐름(천원)`,
    RND = `연구개발비(천원)`,
    IND = `한국표준산업분류코드11차(중분류)`,
    SharePrice = `종가(원)`,
    Auditor = `감사인`,
    ShareNo = `상장주식수(주)`
  )

# -------------------------------------------------------------------
# 3. Standardize macroeconomic dataset key
# -------------------------------------------------------------------

names(IR)[names(IR) == "year"] <- "FYR"

class(df$FYR)
class(GDP$FYR)
class(IR$FYR)
class(UNC$FYR)

df$FYR  <- as.numeric(df$FYR)
GDP$FYR <- as.numeric(GDP$FYR)
IR$FYR  <- as.numeric(IR$FYR)
UNC$FYR <- as.numeric(UNC$FYR)

# -------------------------------------------------------------------
# 4. Merge annual macroeconomic and uncertainty measures
# -------------------------------------------------------------------

df <- df %>%
  left_join(GDP %>% select(FYR, GDP_growth), by = "FYR") %>%
  left_join(IR %>% select(FYR, IR_vol), by = "FYR") %>%
  left_join(
    UNC %>% select(FYR, annual_garch_unc, annual_sv_unc, annual_inf),
    by = "FYR"
  )

# The resulting object is used by 02_data_cleaning.R
