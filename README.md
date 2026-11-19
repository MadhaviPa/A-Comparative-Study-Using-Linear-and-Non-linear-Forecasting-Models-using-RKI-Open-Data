# A Comparative Study Using Linear and Non-linear Forecasting Models using RKI Open Data - SARS-CoV-2 Infektionen in Deutschland

**Case Studies – Project 3 | TU Dortmund University | Summer Semester 2026**

**Authors / project group:** Madhavi Panchangum, Lutz Arne Weiland, Tahereh Rahmani, and Yagiz Ünal.

## Overview

This repository documents a comparative study of **one-step-ahead forecasts of daily reported COVID-19 cases in Germany**. We assess whether epidemiological, demographic, weather, reporting, and calendar predictors improve on autoregressive forecasting, and compare linear approaches against regression trees and random forests. Additional seasonal autoregressive specifications are considered.

The project uses the Robert Koch Institute (RKI) SARS-CoV-2 data and temperature observations from the Deutscher Wetterdienst (DWD). The study period is **1 April 2020 – 31 March 2026**.

## Research objectives

1. Compare autoregressive (AR) and autoregressive-with-exogenous-covariates (ARX) models with regression trees and random forests.
2. Evaluate the contribution of weather, demographic, reporting-related, and calendar covariates.
3. Compare fixed-test, expanding-window, and rolling-window forecast designs where applicable.
4. Assess predictive accuracy using **root mean squared forecast error (RMSFE)** and **Diebold–Mariano tests**; explore seasonal AR/SARX specifications.

## Data and predictors

- **Target:** Daily reported confirmed COVID-19 cases in Germany, modeled as `log(cases + 1)` and evaluated after conversion to the case-count scale.
- **Inputs:** Lagged case counts, daily temperature, age- and gender-group shares, public holidays, Monday/start-of-week indicator, reporting delay, and symptom-onset share.
- **Sources:** [RKI dataset (Zenodo vintage used in supplied code)](https://zenodo.org/records/20791426) and [DWD Climate Data Center](https://opendata.dwd.de/climate_environment/CDC/).

## Main results (reported in the final report)

| Model class / best specification | Expanding-window RMSFE | Rolling-window RMSFE |
| --- | ---: | ---: |
| Random forest | 140.65 | **140.57** |
| Linear ARX | 163.70 | 158.47 |
| Regression tree | 207.62 | 203.54 |

The **random forest with Model 1 covariates and a rolling window** has the lowest reported RMSFE: **140.57 cases**. Reported DM tests favor random forests against the AR and linear comparison models, while differences between several covariate-augmented random forests under rolling estimation are not statistically significant. These figures are transcribed from the report, not recomputed by this repository build.

## Repository layout

```text
covid19-cases-forecasting-germany/
├── README.md
├── report/
│   └── final_report.pdf
├── code/
│   ├── 01_download_data.R
│   ├── 02_preprocessing.R
│   ├── 03_feature_engineering.R
│   ├── 04_linear_models.R
│   ├── 05_tree_random_forest.R
│   ├── 06_seasonal_models.R
│   ├── 07_evaluation.R
│   ├── original_project.Rmd
│   └── run_all.R
├── data/
│   ├── raw/
│   └── processed/
├── figures/
├── results/
├── packages.txt
└── .gitignore
```

**Note:** The modular scripts contain code chunks extracted from the original R Markdown document in their original execution order. The original `.Rmd` is provided as the most faithful source. They have **not** been rerun or tested end-to-end in the packaging environment.

## Code guide

| File | Purpose |
| --- | --- |
| `01_download_data.R` | Load dependencies and acquire RKI/DWD inputs |
| `02_preprocessing.R` | Construct daily cases, reporting and demographic covariates, and merge inputs |
| `03_feature_engineering.R` | Create lags, sample splits, model specifications, and time-series diagnostics |
| `04_linear_models.R` | AR and linear model forecasts (fixed, rolling, expanding) |
| `05_tree_random_forest.R` | Tree and random forest hyperparameter tuning and forecasts |
| `06_seasonal_models.R` | Seasonal model forecasts |
| `07_evaluation.R` | Forecast evaluation, plots and DM tests |

## How to reproduce

1. Install R and the packages in [`packages.txt`](packages.txt). In R, you can use `install.packages(readLines("packages.txt"))`.
2. Set your working directory to the project root.
3. Ensure you have internet access for the source-data downloads, or adapt the data download script for your archived local files.
4. Run `source("code/run_all.R")` **or** execute chunks in [`original_project.Rmd`](code/original_project.Rmd) in sequence.
5. Review the forecast tables and diagnostics. The large historical input files are intentionally excluded from GitHub.

**Reproducibility limitations:** The source R Markdown uses a particular historical RKI snapshot and external DWD files. Some download endpoints or input schemas can change, and the full pipeline may require adjustments. No input data or simulated forecasts have been fabricated to fill missing files.

## Figures and results

The `figures/` files are  the original analytical graphics. CSVs in `results/` are **transcriptions of published tables from the final report**, including AR benchmarks, linear models, regression trees, random forests, and selected DM tests. Refer to the report for precise interpretations and additional seasonal analyses.

## Report

Read the complete seminar report here: **[final_report.pdf](report/final_report.pdf)**.

## Acknowledgments

Project submitted as part of *Case Studies – Project 3*, TU Dortmund University, Summer Semester 2026. Supervisors listed in the report: Prof. Dr. Matei Demetrescu, Dr. Paul Navas, and MSc. Btissame El Mahtout.
