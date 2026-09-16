# The Impact of Inflation Uncertainty on Real Activities: Evidence from Korea

**Graduate Thesis Research Project | R | Panel Data | Regression Analysis**

This repository presents the R-based analytical workflow used in a graduate thesis examining the relationship between inflation uncertainty and real activities.

## Research Questions

How would inflation uncertainty affect firm’s real operating activities in South Korea?”

This study examines abnormal sales, abnormal production activities and abnormal discretionary expenses under high inflation uncertainty to assess how firms adjust their real activities under inflation 
uncertainty. 

## Hypotheses

H1:  Inflation uncertainty is negatively associated with abnormal cash flow from operations. 
H2:  Inflation uncertainty is negatively associated with abnormal production costs. 
H3a: Inflation uncertainty is negatively associated with abnormal discretionary expenses 
H3b: Inflation uncertainty is positively associated with abnormal discretionary expenses 

## Project workflow

1. Data import and dataset integration
2. Data cleaning and sample preparation
3. Financial and control-variable construction
4. Abnormal activity estimation
5. Main fixed-effects regression
6. Moderation / interaction analysis
7. Robustness and alternative specifications
8. Additional analyses


## Methods demonstrated

- Panel-data preparation
- Data cleaning and preprocessing
- Financial variable construction
- Industry-year grouped regressions
- Residual-based abnormal activity measures
- Firm fixed-effects regression
- Industry and year fixed effects
- Clustered standard errors
- Interaction / moderation analysis
- Winsorization

## R packages

- `dplyr`
- `readxl`
- `DescTools`
- `fixest`
- `writexl`

## Repository structure

```text
economic-uncertainty-financial-reporting/
├── README.md
├── code/
│   ├── 01_data_import.R
│   ├── 02_data_cleaning.R
│   ├── 03_variable_construction.R
│   ├── 04_abnormal_activity.R
│   ├── 05_main_regression.R
```

## Running the analysis

Run the scripts sequentially from the project root:

```text
01_data_import.R
        ↓
02_data_cleaning.R
        ↓
03_variable_construction.R
        ↓
04_abnormal_activity.R
        ↓
05_main_regression.R

```

The scripts are intentionally separated by analytical stage so that the research workflow is easier to understand and review.

## Data availability

The original firm-level accounting dataset and related source files are not included in this public repository. The `data/` folder is reserved for locally available input files.

## Inflation Uncertainty measure 
Inflation uncertainty measured using GARCH method


