# Extracted in the original relative order from the uploaded R Markdown analysis.
# Run using code/run_all.R from the repository root; intermediate objects are shared.

# --- Original R Markdown chunk: helper-functions ---
make_formula <- function(x_vars) {
  as.formula(
    paste("log_cases ~", paste(x_vars, collapse = " + "))
  )
}

rmsfe <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}

make_forecast_table <- function(test_rows, model_name, method_name, forecast_type, forecast_log) {
  forecast_cases <- pmax(exp(forecast_log) - 1, 0)

  test_rows %>%
    select(Meldedatum, cases, log_cases) %>%
    mutate(
      Model = model_name,
      Method = method_name,
      Forecast_type = forecast_type,
      Forecast_log = as.numeric(forecast_log),
      Forecast_cases = as.numeric(forecast_cases),
      Error_log = log_cases - Forecast_log,
      Error_cases = cases - Forecast_cases
    )
}

calculate_accuracy <- function(forecast_df) {
  forecast_df %>%
    group_by(Method, Forecast_type, Model) %>%
    summarise(
      MSFE_log = mean(Error_log^2, na.rm = TRUE),
      RMSFE_log = sqrt(mean(Error_log^2, na.rm = TRUE)),
      MSFE_cases = mean(Error_cases^2, na.rm = TRUE),
      RMSFE_cases = sqrt(mean(Error_cases^2, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    arrange(RMSFE_cases)
}

# --- Original R Markdown chunk: ols-fixed-forecasts ---
# Combine model specification sets
all_model_specs <- c(
  lapply(model_specs, function(spec) {
    spec$spec_set <- "Model specs 1"
    spec
  }),
  lapply(model_specs_2, function(spec) {
    spec$spec_set <- "Model specs 2"
    spec
  })
)

fixed_ols_forecasts <- vector(
  mode = "list",
  length = length(all_model_specs)
)

for (i in seq_along(all_model_specs)) {
  spec <- all_model_specs[[i]]

  formula_model <- make_formula(spec$predictors)

  # Fit fixed OLS model using the initial training sample
  linear_model <- lm(
    formula = formula_model,
    data = initial_train_data
  )

  # Generate predictions for the fixed test sample
  linear_forecast_log <- as.numeric(
    predict(
      linear_model,
      newdata = test_data
    )
  )

  fixed_ols_forecasts[[i]] <- make_forecast_table(
    test_rows = test_data,
    model_name = spec$name,
    method_name = "Linear model",
    forecast_type = "Fixed test",
    forecast_log = linear_forecast_log
  ) |>
    mutate(
      Model_spec_set = spec$spec_set
    )
}

fixed_ols_forecasts <- bind_rows(fixed_ols_forecasts)

fixed_ols_accuracy <- calculate_accuracy(
  fixed_ols_forecasts
)

knitr::kable(
  fixed_ols_accuracy,
  digits = 3,
  caption = paste(
    "Fixed test accuracy: Linear models using",
    "model specifications 1 and 2"
  )
)

# --- Original R Markdown chunk: ols-rolling-forecasts ---
rolling_ols_forecasts <- list()
counter <- 1

test_indices <- test_start:nrow(model_data)

for (i in seq_along(all_model_specs)) {
  spec <- all_model_specs[[i]]
  formula_model <- make_formula(spec$predictors)
  
  for (method_name in c("Linear model")) {
    
    model_forecasts <- list()
    
    for (j in seq_along(test_indices)) {
      test_i <- test_indices[j]
      train_start_i <- test_i - rolling_window_size
      train_end_i <- test_i - 1
      
      train_window <- model_data[train_start_i:train_end_i, ]
      test_row <- model_data[test_i, ]
      
      model_fit <- lm(formula_model, data = train_window)
      forecast_log <- as.numeric(predict(model_fit, newdata = test_row))
      
      model_forecasts[[j]] <- make_forecast_table(
        test_rows = test_row,
        model_name = spec$name,
        method_name = method_name,
        forecast_type = "Rolling window",
        forecast_log = forecast_log
      )
    }
    
    rolling_ols_forecasts[[counter]] <- bind_rows(model_forecasts)
    counter <- counter + 1
  }
}

rolling_ols_forecasts <- bind_rows(rolling_ols_forecasts)
rolling_ols_accuracy <- calculate_accuracy(rolling_ols_forecasts)

knitr::kable(rolling_ols_accuracy, digits = 3, caption = "Rolling-window accuracy:  Linear models")

# --- Original R Markdown chunk: ols-expanding-forecasts ---
expanding_ols_forecasts <- list()
counter <- 1

test_indices <- test_start:nrow(model_data)

for (i in seq_along(all_model_specs)) {
  spec <- all_model_specs[[i]]
  formula_model <- make_formula(spec$predictors)

  for (method_name in c("Linear model")) {

    model_forecasts <- list()

    for (j in seq_along(test_indices)) {
      test_i <- test_indices[j]
      train_window <- model_data[1:(test_i - 1), ]
      test_row <- model_data[test_i, ]

      model_fit <- lm(formula_model, data = train_window)
      forecast_log <- as.numeric(predict(model_fit, newdata = test_row))

      model_forecasts[[j]] <- make_forecast_table(
        test_rows = test_row,
        model_name = spec$name,
        method_name = method_name,
        forecast_type = "Expanding window",
        forecast_log = forecast_log
      )
    }

    expanding_ols_forecasts[[counter]] <- bind_rows(model_forecasts)
    counter <- counter + 1
  }
}

expanding_ols_forecasts <- bind_rows(expanding_ols_forecasts)
expanding_ols_accuracy <- calculate_accuracy(expanding_ols_forecasts)

knitr::kable(expanding_ols_accuracy, digits = 3, caption = "Expanding-window accuracy: AR/ARX and Linear models")
