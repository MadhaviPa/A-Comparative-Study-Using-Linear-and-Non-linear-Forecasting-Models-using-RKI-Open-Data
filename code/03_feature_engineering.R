# Extracted in the original relative order from the uploaded R Markdown analysis.
# Run using code/run_all.R from the repository root; intermediate objects are shared.

# --- Original R Markdown chunk: create-lagged-data ---
model_data <- model_base %>%
  arrange(Meldedatum)

# ------------------------------------------------------------
# 1. Lagged cases: lag 1 to lag 7
# ------------------------------------------------------------
max_lag <- 10.

for (l in 1:max_lag) {
  model_data[[paste0("lag", l)]] <- dplyr::lag(model_data$log_cases, l)
}

# ------------------------------------------------------------
# 2. Identify age and gender variables
# ------------------------------------------------------------
age_vars <- names(model_data)[grepl("^AgeShare_", names(model_data))]
gender_vars <- names(model_data)[grepl("^GenderShare_", names(model_data))]

# ------------------------------------------------------------
# 3. Create lag 7 for all covariates
# ------------------------------------------------------------
covariates_to_lag7 <- c(
  "Temp",
  "StartOfWeek",
  "public_holiday",
  "reporting_delay_avg",
  "symptom_onset_share",
  age_vars,
  gender_vars
)

covariates_to_lag7 <- covariates_to_lag7[
  covariates_to_lag7 %in% names(model_data)
]

for (v in covariates_to_lag7) {
  model_data[[paste0(v, "_lag7")]] <- dplyr::lag(model_data[[v]], 7)
}

# ------------------------------------------------------------
# 4. Remove rows lost due to lags
# ------------------------------------------------------------
model_data <- model_data %>%
  tidyr::drop_na()

# --- Original R Markdown chunk: unnamed ---
# ------------------------------------------------------------
# 1. Define lag structures to be valiMeldedatumd
# ------------------------------------------------------------

lag_validation_specs <- list(
  "Lags 1-10" = 1:10,
  "Lags 1-7"  = 1:7,
  "Lag 7 only" = 7,
  "Lag 10 only" = 10
)

lag_summary_table <- tibble(
  Validation_Model = names(lag_validation_specs),
  Covariate_Lags_Used = sapply(lag_validation_specs, function(x) paste(x, collapse = ", ")),
  StartOfWeek_Lag = "No lag",
  Holiday_Lags = "holiday",
  Case_Lags = "log_cases_lag1 to log_cases_lag10"
)

# ------------------------------------------------------------
# 2. Create required lags
# ------------------------------------------------------------

model_data <- model_data %>%
  arrange(Meldedatum) %>%
  mutate(
    public_holiday_lag7 = lag(public_holiday, 7)
  )

# Case lags: always AR(7)
for (l in 1:10) {
  model_data[[paste0("log_cases_lag", l)]] <- lag(model_data$log_cases, l)
}

# Covariates to be lagged
temp_vars <- c("Temp")

age_vars <- names(model_data)[grepl("^AgeShare_", names(model_data))]
gender_vars <- names(model_data)[grepl("^GenderShare_", names(model_data))]

lagged_covariates <- c(temp_vars, age_vars, gender_vars)
lagged_covariates <- lagged_covariates[lagged_covariates %in% names(model_data)]

# Create lags 1-10 for temp, age shares, and gender shares
for (v in lagged_covariates) {
  for (l in 1:10) {
    model_data[[paste0(v, "_lag", l)]] <- lag(model_data[[v]], l)
  }
}

# ------------------------------------------------------------
# 3. Train-validation split
# ------------------------------------------------------------

linear_validation_data <- model_data %>%
  drop_na(log_cases, cases)

split_id <- floor(0.8 * nrow(linear_validation_data))

linear_train <- linear_validation_data[1:split_id, ]
linear_valid <- linear_validation_data[(split_id + 1):nrow(linear_validation_data), ]

# ------------------------------------------------------------
# 4. Function to run linear ARX validation
# ------------------------------------------------------------

run_linear_arx_validation <- function(lag_set_name, cov_lags) {
  
  case_lags <- paste0("log_cases_lag", 1:10)
  
  cov_lag_vars <- unlist(
    lapply(lagged_covariates, function(v) {
      paste0(v, "_lag", cov_lags)
    })
  )
  
  cov_lag_vars <- cov_lag_vars[cov_lag_vars %in% names(model_data)]
  
  fixed_covariates <- c(
    "StartOfWeek",
    "public_holiday",
    "public_holiday_lag7"
  )
  
  fixed_covariates <- fixed_covariates[fixed_covariates %in% names(model_data)]
  
  x_vars <- c(case_lags, cov_lag_vars, fixed_covariates)
  x_vars <- x_vars[x_vars %in% names(model_data)]
  
  train_df <- linear_train %>%
    select(Meldedatum, cases, log_cases, all_of(x_vars)) %>%
    drop_na()
  
  valid_df <- linear_valid %>%
    select(Meldedatum, cases, log_cases, all_of(x_vars)) %>%
    drop_na()
  
  formula_model <- as.formula(
    paste("log_cases ~", paste(x_vars, collapse = " + "))
  )
  
  fit <- lm(formula_model, data = train_df)
  
  pred_log <- predict(fit, newdata = valid_df)
  pred_cases <- expm1(pred_log)
  
  valid_df %>%
    mutate(
      Method = "OLS",
      Forecast_type = "Validation",
      Model = paste0("Linear ARX: ", lag_set_name),
      Forecast_log = as.numeric(pred_log),
      Forecast_cases = as.numeric(pred_cases),
      Actual_log = log_cases,
      Actual_cases = cases,
      Error_log = Actual_log - Forecast_log,
      Error_cases = Actual_cases - Forecast_cases
    )
}

# ------------------------------------------------------------
# 5. Run all four lag validation models
# ------------------------------------------------------------

linear_arx_validation_forecasts <- bind_rows(
  lapply(names(lag_validation_specs), function(spec_name) {
    run_linear_arx_validation(
      lag_set_name = spec_name,
      cov_lags = lag_validation_specs[[spec_name]]
    )
  })
)

# ------------------------------------------------------------
# 6. Validation accuracy table
# ------------------------------------------------------------

linear_arx_validation_accuracy <- linear_arx_validation_forecasts %>%
  group_by(Method, Forecast_type, Model) %>%
  summarise(
    MSFE_cases = mean(Error_cases^2, na.rm = TRUE),
    RMSFE_cases = sqrt(MSFE_cases),
    MSFE_log = mean(Error_log^2, na.rm = TRUE),
    RMSFE_log = sqrt(MSFE_log),
    .groups = "drop"
  ) %>%
  arrange(RMSFE_cases)

knitr::kable(
  linear_arx_validation_accuracy,
  digits = 4,
  caption = "Validation results for linear ARX models with alternative covariate lag structures"
)

# --- Original R Markdown chunk: train-test-split ---
n <- nrow(model_data)
test_start <- floor(0.80 * n) + 1

initial_train_data <- model_data[1:(test_start - 1), ]
test_data <- model_data[test_start:n, ]

rolling_window_size <- nrow(initial_train_data)

cat("Initial training rows:", nrow(initial_train_data), "\n")
cat("Test rows:", nrow(test_data), "\n")
cat("Rolling window size:", rolling_window_size, "\n")
cat("Test period:", as.character(min(test_data$Meldedatum)), "to", as.character(max(test_data$Meldedatum)), "\n")

sample_split_table <- tibble(
  Sample = c("Initial training", "Test"),
  Rows = c(nrow(initial_train_data), nrow(test_data)),
  Start = c(min(initial_train_data$Meldedatum), min(test_data$Meldedatum)),
  End = c(max(initial_train_data$Meldedatum), max(test_data$Meldedatum))
)

knitr::kable(sample_split_table, caption = "Sample split")

# --- Original R Markdown chunk: predictor-groups ---
case_lags_7 <- paste0("lag", 1:10)

age_lag7_vars <- names(model_data)[grepl("^AgeShare_.*_lag7$", names(model_data))]
gender_lag7_vars <- names(model_data)[grepl("^GenderShare_.*_lag7$", names(model_data))]

temp_vars <- c("Temp_lag7")
start_week_vars <- c("StartOfWeek_lag7")
holiday_vars <- c("public_holiday_lag7","public_holiday")
delay_vars <- c("reporting_delay_avg_lag7")
symptom_vars <- c("symptom_onset_share_lag7")

temp_vars <- temp_vars[temp_vars %in% names(model_data)]
start_week_vars <- start_week_vars[start_week_vars %in% names(model_data)]
holiday_vars <- holiday_vars[holiday_vars %in% names(model_data)]
delay_vars <- delay_vars[delay_vars %in% names(model_data)]
symptom_vars <- symptom_vars[symptom_vars %in% names(model_data)]

calendar_vars <- c(start_week_vars, holiday_vars)
demo_vars <- c(age_lag7_vars, gender_lag7_vars)
delay_onset_vars <- c(delay_vars, symptom_vars)

all_covariates <- c(
  temp_vars,
  calendar_vars,
  demo_vars,
  delay_onset_vars
)

# --- Original R Markdown chunk: model-specifications ---
model_specs <- list(
  list(
    name = "Lagged cases only",
    predictors = case_lags_7
  ),
  list(
    name = "Lagged cases + Temp + Age + Gender + StartOfWeek + Holiday",
    predictors = c(case_lags_7, temp_vars, demo_vars, calendar_vars)
  ),
  list(
    name = "Lagged cases + StartOfWeek + Holiday + Delay + Symptom onset",
    predictors = c(case_lags_7, calendar_vars, delay_onset_vars)
  ),
  list(
    name = "Lagged cases + All covariates",
    predictors = c(case_lags_7, all_covariates)
  )
)

# Keep only predictors that exist
model_specs <- lapply(model_specs, function(spec) {
  spec$predictors <- unique(spec$predictors[spec$predictors %in% names(model_data)])
  spec
})

model_spec_table <- bind_rows(lapply(model_specs, function(spec) {
  tibble(
    Model = spec$name,
    Number_of_predictors = length(spec$predictors),
    Predictors = paste(spec$predictors, collapse = " + ")
  )
}))

# --- Original R Markdown chunk: unnamed ---
# Model set 2:
# Baseline model with lagged cases only, plus one separate model for
# each individual covariate added to the lagged-case predictors.

individual_covariates <- unique(c(
  temp_vars,
  demo_vars,
  calendar_vars,
  delay_onset_vars,
  all_covariates
))

# Keep only covariates that exist in model_data
individual_covariates <- individual_covariates[
  individual_covariates %in% names(model_data)
]

model_specs_2 <- c(
  list(
    list(
      name = "Lagged cases only",
      predictors = case_lags_7
    )
  ),
  lapply(individual_covariates, function(covariate) {
    list(
      name = paste("Lagged cases +", covariate),
      predictors = c(case_lags_7, covariate)
    )
  })
)

# Keep only predictors that exist and remove duplicates
model_specs_2 <- lapply(model_specs_2, function(spec) {
  spec$predictors <- unique(
    spec$predictors[spec$predictors %in% names(model_data)]
  )
  spec
})

# Create summary table
model_spec_table_2 <- bind_rows(
  lapply(model_specs_2, function(spec) {
    tibble(
      Model = spec$name,
      Number_of_predictors = length(spec$predictors),
      Predictors = paste(spec$predictors, collapse = " + ")
    )
  })
)

model_spec_table_2

# --- Original R Markdown chunk: log-cases-diagnostics-ar-order, message=FALSE, warning=FALSE, fig.width=10, fig.height=5 ---
# Time-series plot of observed cases
p_cases_ts <- ggplot(model_data, aes(x = Meldedatum, y = cases)) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Daily reported COVID-19 cases",
    x = "Date",
    y = "Cases"
  ) +
  theme_minimal()

print(p_cases_ts)

# ACF and PACF of log cases using the training sample only
acf(initial_train_data$log_cases,
    lag.max = 40,
    na.action = na.pass,
    main = "ACF of log(cases + 1): initial training sample")

pacf(initial_train_data$log_cases,
     lag.max = 40,
     na.action = na.pass,
     main = "PACF of log(cases + 1): initial training sample")

# Fit AR(p) models for p = 0, ..., 10 and compare AIC/BIC.
# AR(0) is an intercept-only benchmark.
ar_order_results <- purrr::map_dfr(0:max_ar_p, function(p_order) {
  if (p_order == 0) {
    fit <- lm(log_cases ~ 1, data = initial_train_data)
  } else {
    lag_names <- paste0("lag", seq_len(p_order))
    lag_names <- lag_names[lag_names %in% names(initial_train_data)]

    if (length(lag_names) != p_order) {
      return(tibble(
        p = p_order,
        AIC = NA_real_,
        BIC = NA_real_,
        N = NA_integer_
      ))
    }

    fit <- lm(stats::reformulate(lag_names, response = "log_cases"), data = initial_train_data)
  }

  tibble(
    p = p_order,
    AIC = AIC(fit),
    BIC = BIC(fit),
    N = stats::nobs(fit)
  )
})

best_p_aic <- ar_order_results %>%
  filter(is.finite(AIC)) %>%
  slice_min(AIC, n = 1, with_ties = FALSE) %>%
  pull(p)

best_p_bic <- ar_order_results %>%
  filter(is.finite(BIC)) %>%
  slice_min(BIC, n = 1, with_ties = FALSE) %>%
  pull(p)

selected_ar_p <- best_p_bic
selected_ar_lags <- paste0("lag", seq_len(selected_ar_p))
selected_ar_lags <- selected_ar_lags[selected_ar_lags %in% names(model_data)]

knitr::kable(
  ar_order_results,
  digits = 3,
  caption = "AR lag-order selection for log(cases + 1), maximum lag 10"
)

cat("AIC-selected AR order:", best_p_aic, "\n")
cat("BIC-selected AR order:", best_p_bic, "\n")
cat("Preferred parsimonious order used for reporting:", selected_ar_p, "\n")

# Plot AIC and BIC across candidate AR orders
ar_ic_long <- ar_order_results %>%
  select(p, AIC, BIC) %>%
  pivot_longer(c(AIC, BIC), names_to = "Criterion", values_to = "Value")

p_ar_ic <- ggplot(ar_ic_long, aes(x = p, y = Value, linetype = Criterion)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 0:max_ar_p) +
  labs(
    title = "AIC and BIC for AR(p) models of log cases",
    subtitle = paste0("AIC selects p = ", best_p_aic,
                      "; BIC selects p = ", best_p_bic),
    x = "AR order p",
    y = "Information criterion"
  ) +
  theme_minimal()

print(p_ar_ic)
