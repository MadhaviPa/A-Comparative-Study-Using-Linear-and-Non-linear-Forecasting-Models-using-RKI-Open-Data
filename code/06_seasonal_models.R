# Extracted in the original relative order from the uploaded R Markdown analysis.
# Run using code/run_all.R from the repository root; intermediate objects are shared.

# --- Original R Markdown chunk: sarx-seasonal-lags ---
# ------------------------------------------------------------
# Seasonal AR terms for SARX
# SARX(p) uses lag1...lagp, lag365, and lag365+1...lag365+p.
# These are created on the full ordered model_data so that the test rows
# keep valid annual lag information.
# ------------------------------------------------------------
seasonal_period <- 365
sarx_p <- 10

model_data$lag365 <- dplyr::lag(model_data$log_cases, seasonal_period)

for (l in 1:sarx_p) {
  model_data[[paste0("lag", seasonal_period + l)]] <-
    dplyr::lag(model_data$log_cases, seasonal_period + l)
}

# Refresh the existing train-test objects after adding SARX variables.
initial_train_data <- model_data[1:(test_start - 1), ]
test_data <- model_data[test_start:nrow(model_data), ]
test_indices <- test_start:nrow(model_data)

sarx_base_vars <- c(
  paste0("lag", 1:sarx_p),
  "lag365",
  paste0("lag", seasonal_period + 1:sarx_p)
)

sarx_base_vars <- sarx_base_vars[sarx_base_vars %in% names(model_data)]

sarx_specs <- lapply(model_specs, function(spec) {
  list(
    name = spec$name,
    predictors = unique(c(sarx_base_vars, setdiff(spec$predictors, paste0("lag", 1:10))))
  )
})

sarx_specs <- lapply(sarx_specs, function(spec) {
  spec$predictors <- unique(spec$predictors[spec$predictors %in% names(model_data)])
  spec
})

sarx_spec_table <- bind_rows(lapply(sarx_specs, function(spec) {
  tibble(
    Model = spec$name,
    Number_of_predictors = length(spec$predictors),
    Predictors = paste(spec$predictors, collapse = " + ")
  )
}))

# --- Original R Markdown chunk: sarx-rolling-forecasts ---
rolling_sarx_forecasts <- list()

for (m in seq_along(sarx_specs)) {
  spec <- sarx_specs[[m]]
  formula_model <- make_formula(spec$predictors)
  forecast_rows <- list()

  for (j in seq_along(test_indices)) {
    test_i <- test_indices[j]
    train_start_i <- test_i - rolling_window_size
    train_end_i <- test_i - 1

    train_window <- model_data[train_start_i:train_end_i, ]
    test_row <- model_data[test_i, ]

    sarx_fit <- lm(formula_model, data = train_window, na.action = na.exclude)
    forecast_log <- as.numeric(predict(sarx_fit, newdata = test_row))

    forecast_rows[[j]] <- make_forecast_table(
      test_rows = test_row,
      model_name = spec$name,
      method_name = "SARX",
      forecast_type = "Rolling window",
      forecast_log = forecast_log
    )
  }

  rolling_sarx_forecasts[[m]] <- bind_rows(forecast_rows)
}

rolling_sarx_forecasts <- bind_rows(rolling_sarx_forecasts)
rolling_sarx_accuracy <- calculate_accuracy(rolling_sarx_forecasts)

knitr::kable(
  rolling_sarx_accuracy,
  digits = 3,
  caption = "Rolling-window SARX forecast accuracy"
)

# --- Original R Markdown chunk: sarx-expanding-forecasts ---
expanding_sarx_forecasts <- list()

for (m in seq_along(sarx_specs)) {
  spec <- sarx_specs[[m]]
  formula_model <- make_formula(spec$predictors)
  forecast_rows <- list()

  for (j in seq_along(test_indices)) {
    test_i <- test_indices[j]

    train_window <- model_data[1:(test_i - 1), ]
    test_row <- model_data[test_i, ]

    sarx_fit <- lm(formula_model, data = train_window, na.action = na.exclude)
    forecast_log <- as.numeric(predict(sarx_fit, newdata = test_row))

    forecast_rows[[j]] <- make_forecast_table(
      test_rows = test_row,
      model_name = spec$name,
      method_name = "SARX",
      forecast_type = "Expanding window",
      forecast_log = forecast_log
    )
  }

  expanding_sarx_forecasts[[m]] <- bind_rows(forecast_rows)
}

expanding_sarx_forecasts <- bind_rows(expanding_sarx_forecasts)
expanding_sarx_accuracy <- calculate_accuracy(expanding_sarx_forecasts)

knitr::kable(
  expanding_sarx_accuracy,
  digits = 3,
  caption = "Expanding-window SARX forecast accuracy"
)
