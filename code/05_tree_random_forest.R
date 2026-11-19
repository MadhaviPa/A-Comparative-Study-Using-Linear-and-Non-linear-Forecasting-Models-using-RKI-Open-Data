# Extracted in the original relative order from the uploaded R Markdown analysis.
# Run using code/run_all.R from the repository root; intermediate objects are shared.

# --- Original R Markdown chunk: ml-validation-split ---
n_initial <- nrow(initial_train_data)
validation_start <- floor(0.80 * n_initial) + 1

cv_train <- initial_train_data[1:(validation_start - 1), ]
cv_valid <- initial_train_data[validation_start:n_initial, ]

cat("ML tuning training rows:", nrow(cv_train), "\n")
cat("ML tuning validation rows:", nrow(cv_valid), "\n")

# --- Original R Markdown chunk: ml-grids ---
tree_grid <- expand.grid(
  minsplit = c(10, 20, 30, 50),
  maxdepth = c(3, 5, 7, 10),
  cp = c(0.001, 0.005, 0.01, 0.02),
  stringsAsFactors = FALSE
)

rf_trees <- c(100, 200, 300)

# --- Original R Markdown chunk: ml-tuning-functions ---
tune_tree <- function(predictors, cv_train, cv_valid) {
  formula_model <- make_formula(predictors)
  out <- list()

  for (i in 1:nrow(tree_grid)) {
    fit <- rpart::rpart(
      formula_model,
      data = cv_train,
      method = "anova",
      control = rpart::rpart.control(
        minsplit = tree_grid$minsplit[i],
        maxdepth = tree_grid$maxdepth[i],
        cp = tree_grid$cp[i]
      )
    )

    pred <- as.numeric(predict(fit, newdata = cv_valid))

    out[[i]] <- data.frame(
      minsplit = tree_grid$minsplit[i],
      maxdepth = tree_grid$maxdepth[i],
      cp = tree_grid$cp[i],
      Validation_RMSFE_log = rmsfe(cv_valid$log_cases, pred)
    )
  }

  bind_rows(out) %>%
    arrange(Validation_RMSFE_log) %>%
    slice(1)
}

tune_rf <- function(predictors, cv_train, cv_valid) {
  p <- length(predictors)

  if (p <= 10) {
    mtry_values <- c(2, 4, 6, 8, 10)
  } else {
    mtry_values <- unique(c(5, 10, 20, 30, 50, floor(sqrt(p)), floor(p / 3)))
  }

  mtry_values <- unique(pmin(p, pmax(1, mtry_values)))

  rf_grid <- expand.grid(
    mtry = mtry_values,
    min.node.size = c(1, 5, 10, 20),
    num.trees = rf_trees,
    stringsAsFactors = FALSE
  )

  formula_model <- make_formula(predictors)
  out <- list()

  for (i in 1:nrow(rf_grid)) {
    fit <- ranger::ranger(
      formula_model,
      data = cv_train,
      num.trees = rf_grid$num.trees[i],
      mtry = rf_grid$mtry[i],
      min.node.size = rf_grid$min.node.size[i],
      seed = 123
    )

    pred <- predict(fit, data = cv_valid)$predictions

    out[[i]] <- data.frame(
      mtry = rf_grid$mtry[i],
      min.node.size = rf_grid$min.node.size[i],
      num.trees = rf_grid$num.trees[i],
      Validation_RMSFE_log = rmsfe(cv_valid$log_cases, pred)
    )
  }

  bind_rows(out) %>%
    arrange(Validation_RMSFE_log) %>%
    slice(1)
}

# --- Original R Markdown chunk: ml-hyperparameter-selection ---
tree_best_list <- list()
rf_best_list <- list()

for (i in seq_along(model_specs)) {
  spec <- model_specs[[i]]

  best_tree <- tune_tree(spec$predictors, cv_train, cv_valid)
  tree_best_list[[i]] <- best_tree %>%
    mutate(Model = spec$name, Method = "Regression Tree")

  best_rf <- tune_rf(spec$predictors, cv_train, cv_valid)
  rf_best_list[[i]] <- best_rf %>%
    mutate(Model = spec$name, Method = "Random Forest")
}

tree_best_table <- bind_rows(tree_best_list)
rf_best_table <- bind_rows(rf_best_list)

knitr::kable(
  tree_best_table,
  digits = 4,
  caption = "Selected regression tree hyperparameters from validation sample"
)

knitr::kable(
  rf_best_table,
  digits = 4,
  caption = "Selected random forest hyperparameters from validation sample"
)

# --- Original R Markdown chunk: ml-rolling-forecasts ---
rolling_ml_forecasts <- list()
counter <- 1

test_indices <- test_start:nrow(model_data)

for (i in seq_along(model_specs)) {
  spec <- model_specs[[i]]
  formula_model <- make_formula(spec$predictors)

  best_tree <- tree_best_table %>% filter(Model == spec$name) %>% slice(1)
  best_rf <- rf_best_table %>% filter(Model == spec$name) %>% slice(1)

  # Regression Tree rolling forecasts
  tree_rows <- list()

  for (j in seq_along(test_indices)) {
    test_i <- test_indices[j]
    train_start_i <- test_i - rolling_window_size
    train_end_i <- test_i - 1

    train_window <- model_data[train_start_i:train_end_i, ]
    test_row <- model_data[test_i, ]

    tree_fit <- rpart::rpart(
      formula_model,
      data = train_window,
      method = "anova",
      control = rpart::rpart.control(
        minsplit = best_tree$minsplit,
        maxdepth = best_tree$maxdepth,
        cp = best_tree$cp
      )
    )

    forecast_log <- as.numeric(predict(tree_fit, newdata = test_row))

    tree_rows[[j]] <- make_forecast_table(
      test_rows = test_row,
      model_name = spec$name,
      method_name = "Regression Tree",
      forecast_type = "Rolling window",
      forecast_log = forecast_log
    )
  }

  rolling_ml_forecasts[[counter]] <- bind_rows(tree_rows)
  counter <- counter + 1

  # Random Forest rolling forecasts
  rf_rows <- list()

  for (j in seq_along(test_indices)) {
    test_i <- test_indices[j]
    train_start_i <- test_i - rolling_window_size
    train_end_i <- test_i - 1

    train_window <- model_data[train_start_i:train_end_i, ]
    test_row <- model_data[test_i, ]

    rf_fit <- ranger::ranger(
      formula_model,
      data = train_window,
      num.trees = best_rf$num.trees,
      mtry = best_rf$mtry,
      min.node.size = best_rf$min.node.size,
      seed = 123
    )

    forecast_log <- predict(rf_fit, data = test_row)$predictions

    rf_rows[[j]] <- make_forecast_table(
      test_rows = test_row,
      model_name = spec$name,
      method_name = "Random Forest",
      forecast_type = "Rolling window",
      forecast_log = forecast_log
    )
  }

  rolling_ml_forecasts[[counter]] <- bind_rows(rf_rows)
  counter <- counter + 1
}

rolling_ml_forecasts <- bind_rows(rolling_ml_forecasts)
rolling_ml_accuracy <- calculate_accuracy(rolling_ml_forecasts)

knitr::kable(rolling_ml_accuracy, digits = 3, caption = "Rolling-window accuracy: Regression Tree and Random Forest")

# --- Original R Markdown chunk: ml-expanding-forecasts ---
expanding_ml_forecasts <- list()
counter <- 1

test_indices <- test_start:nrow(model_data)

for (i in seq_along(model_specs)) {
  spec <- model_specs[[i]]
  formula_model <- make_formula(spec$predictors)

  best_tree <- tree_best_table %>% filter(Model == spec$name) %>% slice(1)
  best_rf <- rf_best_table %>% filter(Model == spec$name) %>% slice(1)

  # Regression Tree expanding forecasts
  tree_rows <- list()

  for (j in seq_along(test_indices)) {
    test_i <- test_indices[j]
    train_window <- model_data[1:(test_i - 1), ]
    test_row <- model_data[test_i, ]

    tree_fit <- rpart::rpart(
      formula_model,
      data = train_window,
      method = "anova",
      control = rpart::rpart.control(
        minsplit = best_tree$minsplit,
        maxdepth = best_tree$maxdepth,
        cp = best_tree$cp
      )
    )

    forecast_log <- as.numeric(predict(tree_fit, newdata = test_row))

    tree_rows[[j]] <- make_forecast_table(
      test_rows = test_row,
      model_name = spec$name,
      method_name = "Regression Tree",
      forecast_type = "Expanding window",
      forecast_log = forecast_log
    )
  }

  expanding_ml_forecasts[[counter]] <- bind_rows(tree_rows)
  counter <- counter + 1

  # Random Forest expanding forecasts
  rf_rows <- list()

  for (j in seq_along(test_indices)) {
    test_i <- test_indices[j]
    train_window <- model_data[1:(test_i - 1), ]
    test_row <- model_data[test_i, ]

    rf_fit <- ranger::ranger(
      formula_model,
      data = train_window,
      num.trees = best_rf$num.trees,
      mtry = best_rf$mtry,
      min.node.size = best_rf$min.node.size,
      seed = 123
    )

    forecast_log <- predict(rf_fit, data = test_row)$predictions

    rf_rows[[j]] <- make_forecast_table(
      test_rows = test_row,
      model_name = spec$name,
      method_name = "Random Forest",
      forecast_type = "Expanding window",
      forecast_log = forecast_log
    )
  }

  expanding_ml_forecasts[[counter]] <- bind_rows(rf_rows)
  counter <- counter + 1
}

expanding_ml_forecasts <- bind_rows(expanding_ml_forecasts)
expanding_ml_accuracy <- calculate_accuracy(expanding_ml_forecasts)

knitr::kable(expanding_ml_accuracy, digits = 3, caption = "Expanding-window accuracy: Regression Tree and Random Forest")
