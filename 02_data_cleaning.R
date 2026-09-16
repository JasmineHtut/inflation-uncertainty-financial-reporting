# 02_data_cleaning.R
# Purpose: Clean the imported panel data and define the initial analysis sample.
#
# Input: df created by 01_data_import.R
# Output: df cleaned for variable construction.

library(dplyr)

# -------------------------------------------------------------------
# 1. Keep observations with the core raw variables required later
# -------------------------------------------------------------------

required_raw <- c(
  "Symbol", "FYR", "AT", "AR", "Inventory", "TotalLiability",
  "TotalEquity", "NI", "PPE", "SALES", "COGS", "CFO", "SGA",
  "RND", "IND", "SharePrice", "ShareNo", "Auditor"
)

df <- df %>%
  filter(if_all(all_of(required_raw), ~ !is.na(.)))

# -------------------------------------------------------------------
# 2. Order the panel by firm and fiscal year
# -------------------------------------------------------------------

df <- df %>%
  arrange(Symbol, FYR)

# -------------------------------------------------------------------
# 3. Remove exact duplicate firm-year observations
# -------------------------------------------------------------------
# Review duplicates before applying this step if the raw dataset can
# legitimately contain multiple observations per firm-year.

df <- df %>%
  distinct(Symbol, FYR, .keep_all = TRUE)

# The resulting object is used by 03_variable_construction.R
