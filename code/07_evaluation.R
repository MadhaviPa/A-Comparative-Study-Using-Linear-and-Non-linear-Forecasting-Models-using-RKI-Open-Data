# Extracted in the original relative order from the uploaded R Markdown analysis.
# Run using code/run_all.R from the repository root; intermediate objects are shared.

# --- Original R Markdown chunk: hyperparameter-tables ---
tree_best_hyperparameters <- bind_rows(tree_best_list) %>%
  select(Model, Method, minsplit, maxdepth, cp, Validation_RMSFE_log)

rf_best_hyperparameters <- bind_rows(rf_best_list) %>%
  select(Model, Method, mtry, min.node.size, num.trees, Validation_RMSFE_log)

knitr::kable(tree_best_hyperparameters, digits = 4, caption = "Selected regression tree hyperparameters")
knitr::kable(rf_best_hyperparameters, digits = 4, caption = "Selected random forest hyperparameters")

# --- Original R Markdown chunk: combine-forecasts-focused-results, message=FALSE, warning=FALSE ---
all_forecasts_combined <- bind_rows(
  fixed_ols_forecasts,
  rolling_ols_forecasts,
  expanding_ols_forecasts,
  rolling_ml_forecasts,
  expanding_ml_forecasts,
  rolling_sarx_forecasts,
  expanding_sarx_forecasts
) %>%
  mutate(
    Model_ID = paste(Method, Forecast_type, Model, sep = " | ")
  )

all_accuracy <- calculate_accuracy(all_forecasts_combined)

# Compact RMSFE table for all methods and forecast types.
rmsfe_summary <- all_accuracy %>%
  arrange(RMSFE_cases) %>%
  mutate(Rank = row_number()) %>%
  select(
    Rank, Method, Forecast_type, Model,
    RMSFE_cases, RMSFE_log, MSFE_cases, MSFE_log
  )

top10_models <- rmsfe_summary %>%
  slice_head(n = 10)

best_by_method_forecast <- rmsfe_summary %>%
  group_by(Method, Forecast_type) %>%
  slice_min(RMSFE_cases, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(RMSFE_cases)

knitr::kable(
  top10_models,
  digits = 3,
  caption = "Top 10 model-method-forecast combinations by RMSFE on original cases"
)

knitr::kable(
  best_by_method_forecast,
  digits = 3,
  caption = "Best model for each method and forecast type"
)

# --- Original R Markdown chunk: focused-rmsfe-tables, message=FALSE, warning=FALSE ---
# AR and ARX/linear model results on original scale.
ar_arx_rmsfe <- rmsfe_summary %>%
  filter(Method %in% c("AR / ARX", "Linear model")) %>%
  select(Method, Forecast_type, Model, RMSFE_cases, RMSFE_log) %>%
  arrange(Method, Forecast_type, RMSFE_cases)

# Machine-learning results on original scale.
ml_rmsfe <- rmsfe_summary %>%
  filter(Method %in% c("Regression Tree", "Random Forest")) %>%
  select(Method, Forecast_type, Model, RMSFE_cases, RMSFE_log) %>%
  arrange(Method, Forecast_type, RMSFE_cases)

# SARX results on original scale.
sarx_rmsfe <- rmsfe_summary %>%
  filter(Method == "SARX") %>%
  select(Method, Forecast_type, Model, RMSFE_cases, RMSFE_log) %>%
  arrange(Forecast_type, RMSFE_cases)

knitr::kable(ar_arx_rmsfe, digits = 3, caption = "AR/ARX and linear model RMSFE")
knitr::kable(ml_rmsfe, digits = 3, caption = "Regression tree and random forest RMSFE")
knitr::kable(sarx_rmsfe, digits = 3, caption = "SARX RMSFE")

# --- Original R Markdown chunk: focused-hyperparameter-tables, message=FALSE, warning=FALSE ---
tree_best_hyperparameters <- tree_best_table %>%
  select(Model, Method, minsplit, maxdepth, cp, Validation_RMSFE_log) %>%
  arrange(Validation_RMSFE_log)

rf_best_hyperparameters <- rf_best_table %>%
  select(Model, Method, mtry, min.node.size, num.trees, Validation_RMSFE_log) %>%
  arrange(Validation_RMSFE_log)

knitr::kable(
  tree_best_hyperparameters,
  digits = 4,
  caption = "Selected regression tree hyperparameters from validation RMSFE"
)

knitr::kable(
  rf_best_hyperparameters,
  digits = 4,
  caption = "Selected random forest hyperparameters from validation RMSFE"
)

# --- Original R Markdown chunk: individual-actual-vs-forecast-plots, message=FALSE, warning=FALSE, fig.width=10, fig.height=5 ---
plot_dir <- "project3_individual_actual_vs_forecast_plots"
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

safe_name <- function(x) {
  x %>%
    stringr::str_replace_all("[^A-Za-z0-9]+", "_") %>%
    stringr::str_replace_all("_+", "_") %>%
    stringr::str_replace_all("^_|_$", "")
}

make_actual_forecast_plot <- function(df_one, title_text) {
  ggplot(df_one, aes(x = Meldedatum)) +
    geom_line(aes(y = cases), linewidth = 0.45) +
    geom_line(aes(y = Forecast_cases), linewidth = 0.45, linetype = "dashed") +
    labs(
      title = title_text,
      subtitle = "Solid line = actual cases; dashed line = forecast cases",
      x = "Date",
      y = "Daily cases"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal()
}

plot_keys <- rmsfe_summary %>%
  select(Method, Forecast_type, Model, RMSFE_cases) %>%
  arrange(Method, Forecast_type, RMSFE_cases)

actual_forecast_files <- c()

for (i in seq_len(nrow(plot_keys))) {
  key <- plot_keys[i, ]

  df_one <- all_forecasts_combined %>%
    filter(
      Method == key$Method,
      Forecast_type == key$Forecast_type,
      Model == key$Model
    ) %>%
    arrange(Meldedatum)

  title_text <- paste0(
    key$Method, " | ", key$Forecast_type, " | ", key$Model,
    " | RMSFE cases = ", round(key$RMSFE_cases, 3)
  )

  p <- make_actual_forecast_plot(df_one, title_text)

  file_name <- file.path(
    plot_dir,
    paste0("actual_vs_forecast_", sprintf("%03d", i), "_", safe_name(title_text), ".png")
  )

  ggsave(file_name, p, width = 10, height = 5, dpi = 300)
  actual_forecast_files <- c(actual_forecast_files, file_name)

  print(p)
}

actual_forecast_file_table <- tibble(
  Plot_Number = seq_along(actual_forecast_files),
  File = actual_forecast_files
)

knitr::kable(
  actual_forecast_file_table,
  caption = "Saved actual-vs-forecast plots for every model and forecast method"
)

# --- Original R Markdown chunk: setup, message=FALSE, warning=FALSE ---
library(dplyr)
library(tidyr)
library(tibble)
library(readr)
library(knitr)
# install.packages("MCS") # run once if needed
library(MCS)

# --- Original R Markdown chunk: create-forecast-losses, message=FALSE, warning=FALSE ---
all_model_losses <- all_forecasts_combined %>%
  filter(
    !grepl("SARX", Method, ignore.case = TRUE),
    !grepl("SARX", Model, ignore.case = TRUE)
  ) %>%
  mutate(
    Absolute_Error_log = abs(Error_log),
    Squared_Error_log = Error_log^2,
    Absolute_Error_cases = abs(Error_cases),
    Squared_Error_cases = Error_cases^2
  )

# --- Original R Markdown chunk: summarize-model-losses, message=FALSE, warning=FALSE ---
loss_summary <- all_model_losses %>%
  group_by(Method, Forecast_type, Model) %>%
  summarise(
    N = sum(is.finite(Error_cases)),
    MAE_cases = mean(Absolute_Error_cases, na.rm = TRUE),
    MSFE_cases = mean(Squared_Error_cases, na.rm = TRUE),
    RMSFE_cases = sqrt(MSFE_cases),
    MAE_log = mean(Absolute_Error_log, na.rm = TRUE),
    MSFE_log = mean(Squared_Error_log, na.rm = TRUE),
    RMSFE_log = sqrt(MSFE_log),
    .groups = "drop"
  ) %>%
  arrange(RMSFE_cases)

knitr::kable(
  loss_summary %>% slice_head(n = 10),
  digits = 4,
  caption = "Top 10 models by loss functions on the original and log scales"
)

# --- Original R Markdown chunk: manual-dm-function, message=FALSE, warning=FALSE ---
manual_dm_test <- function(loss_difference, h = 1) {
  loss_difference <- loss_difference[is.finite(loss_difference)]
  n_obs <- length(loss_difference)

  if (
    n_obs < 5 ||
    !is.finite(stats::sd(loss_difference)) ||
    stats::sd(loss_difference) == 0
  ) {
    return(
      tibble(
        DM_stat = NA_real_,
        p_value = NA_real_,
        mean_loss_difference = mean(loss_difference, na.rm = TRUE),
        long_run_variance = NA_real_,
        T = n_obs
      )
    )
  }

  centered_difference <- loss_difference - mean(loss_difference)
  gamma_0 <- sum(centered_difference^2) / n_obs
  long_run_variance <- gamma_0

  if (h > 1) {
    for (lag_i in seq_len(h - 1)) {
      gamma_i <- sum(
        centered_difference[(lag_i + 1):n_obs] *
          centered_difference[1:(n_obs - lag_i)]
      ) / n_obs

      bartlett_weight <- 1 - lag_i / h
      long_run_variance <- long_run_variance +
        2 * bartlett_weight * gamma_i
    }
  }

  if (!is.finite(long_run_variance) || long_run_variance <= 0) {
    return(
      tibble(
        DM_stat = NA_real_,
        p_value = NA_real_,
        mean_loss_difference = mean(loss_difference),
        long_run_variance = long_run_variance,
        T = n_obs
      )
    )
  }

  dm_statistic <- mean(loss_difference) /
    sqrt(long_run_variance / n_obs)

  p_value <- 2 * (1 - stats::pnorm(abs(dm_statistic)))

  tibble(
    DM_stat = dm_statistic,
    p_value = p_value,
    mean_loss_difference = mean(loss_difference),
    long_run_variance = long_run_variance,
    T = n_obs
  )
}

# --- Original R Markdown chunk: pairwise-dm-function, message=FALSE, warning=FALSE ---
pairwise_dm_tests <- function(
  loss_data,
  comparison_keys,
  loss_col,
  h = 1,
  alpha = 0.05
) {
  output <- list()
  result_counter <- 1

  forecast_types <- unique(comparison_keys$Forecast_type)

  for (forecast_type_i in forecast_types) {
    keys_for_type <- comparison_keys %>%
      filter(Forecast_type == forecast_type_i)

    if (nrow(keys_for_type) < 2) next

    for (i in seq_len(nrow(keys_for_type) - 1)) {
      for (j in (i + 1):nrow(keys_for_type)) {
        model_1 <- keys_for_type[i, ]
        model_2 <- keys_for_type[j, ]

        model_1_losses <- loss_data %>%
          filter(
            Forecast_type == model_1$Forecast_type,
            Method == model_1$Method,
            Model == model_1$Model
          ) %>%
          select(Meldedatum, loss_1 = all_of(loss_col))

        model_2_losses <- loss_data %>%
          filter(
            Forecast_type == model_2$Forecast_type,
            Method == model_2$Method,
            Model == model_2$Model
          ) %>%
          select(Meldedatum, loss_2 = all_of(loss_col))

        joined_losses <- inner_join(
          model_1_losses,
          model_2_losses,
          by = "Meldedatum"
        ) %>%
          filter(is.finite(loss_1), is.finite(loss_2)) %>%
          mutate(loss_difference = loss_1 - loss_2)

        dm_result <- manual_dm_test(
          joined_losses$loss_difference,
          h = h
        ) %>%
          mutate(
            Forecast_type = forecast_type_i,
            Loss_function = loss_col,
            Model_1 = paste(model_1$Method, model_1$Model, sep = " | "),
            Model_2 = paste(model_2$Method, model_2$Model, sep = " | "),
            Interpretation = case_when(
              is.na(p_value) ~ "DM test unavailable",
              p_value < alpha & mean_loss_difference < 0 ~
                "Model 1 has significantly lower loss",
              p_value < alpha & mean_loss_difference > 0 ~
                "Model 2 has significantly lower loss",
              TRUE ~ "No significant difference"
            )
          )

        output[[result_counter]] <- dm_result
        result_counter <- result_counter + 1
      }
    }
  }

  bind_rows(output) %>%
    arrange(Forecast_type, p_value) %>%
    select(
      Forecast_type,
      Loss_function,
      Model_1,
      Model_2,
      T,
      mean_loss_difference,
      long_run_variance,
      DM_stat,
      p_value,
      Interpretation
    )
}

# --- Original R Markdown chunk: run-pairwise-dm-tests-all-models, message=FALSE, warning=FALSE ---
dm_keys_all_models <- all_model_losses %>%
  distinct(Method, Forecast_type, Model)

dm_tests_squared_cases_all <- pairwise_dm_tests(
  loss_data = all_model_losses,
  comparison_keys = dm_keys_all_models,
  loss_col = "Squared_Error_cases",
  h = 1,
  alpha = 0.05
)

dm_tests_absolute_cases_all <- pairwise_dm_tests(
  loss_data = all_model_losses,
  comparison_keys = dm_keys_all_models,
  loss_col = "Absolute_Error_cases",
  h = 1,
  alpha = 0.05
)

rolling_dm_tests_squared_all <- dm_tests_squared_cases_all %>%
  filter(Forecast_type == "Rolling window")

expanding_dm_tests_squared_all <- dm_tests_squared_cases_all %>%
  filter(Forecast_type == "Expanding window")

rolling_dm_tests_absolute_all <- dm_tests_absolute_cases_all %>%
  filter(Forecast_type == "Rolling window")

expanding_dm_tests_absolute_all <- dm_tests_absolute_cases_all %>%
  filter(Forecast_type == "Expanding window")

knitr::kable(
  rolling_dm_tests_squared_all,
  digits = 4,
  caption = "Pairwise DM tests across all rolling-window models using squared-error loss"
)

knitr::kable(
  expanding_dm_tests_squared_all,
  digits = 4,
  caption = "Pairwise DM tests across all expanding-window models using squared-error loss"
)

knitr::kable(
  rolling_dm_tests_absolute_all,
  digits = 4,
  caption = "Pairwise DM tests across all rolling-window models using absolute-error loss"
)

knitr::kable(
  expanding_dm_tests_absolute_all,
  digits = 4,
  caption = "Pairwise DM tests across all expanding-window models using absolute-error loss"
)

# --- Original R Markdown chunk: model-id-helper-functions, message=FALSE, warning=FALSE ---
create_model_id <- function(method, model) {
  paste(method, model, sep = " | ")
}

split_model_id <- function(model_id) {
  model_parts <- strsplit(
    model_id,
    " | ",
    fixed = TRUE
  )[[1]]

  list(
    Method = model_parts[1],
    Model = paste(model_parts[-1], collapse = " | ")
  )
}

# --- Original R Markdown chunk: dm-tournament-function, message=FALSE, warning=FALSE ---
run_dm_tournament <- function(
  loss_data,
  forecast_type,
  loss_col = "Squared_Error_cases",
  h = 1,
  alpha = 0.05
) {
  candidates <- loss_data %>%
    filter(Forecast_type == forecast_type) %>%
    distinct(Method, Model) %>%
    arrange(Method, Model) %>%
    mutate(Model_ID = create_model_id(Method, Model)) %>%
    pull(Model_ID)

  if (length(candidates) == 0) {
    return(list(rounds = tibble(), champion = tibble()))
  }

  all_rounds <- list()
  round_number <- 1

  while (length(candidates) > 1) {
    candidate_order <- loss_data %>%
      filter(Forecast_type == forecast_type) %>%
      mutate(Model_ID = create_model_id(Method, Model)) %>%
      filter(Model_ID %in% candidates) %>%
      group_by(Model_ID) %>%
      summarise(
        Mean_loss = mean(.data[[loss_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(Mean_loss) %>%
      pull(Model_ID)

    candidates <- candidate_order
    round_results <- list()
    winners <- character()
    pair_number <- 1
    pair_starts <- seq(1, length(candidates), by = 2)

    for (start_i in pair_starts) {
      model_1_id <- candidates[start_i]

      if (start_i == length(candidates)) {
        winners <- c(winners, model_1_id)

        round_results[[pair_number]] <- tibble(
          Forecast_type = forecast_type,
          Loss_function = loss_col,
          Round = round_number,
          Pair = pair_number,
          Model_1 = model_1_id,
          Model_2 = NA_character_,
          T = NA_integer_,
          Mean_loss_1 = NA_real_,
          Mean_loss_2 = NA_real_,
          mean_loss_difference = NA_real_,
          DM_stat = NA_real_,
          p_value = NA_real_,
          Winner = model_1_id,
          Decision = "Bye to next round"
        )

        pair_number <- pair_number + 1
        next
      }

      model_2_id <- candidates[start_i + 1]
      model_1_parts <- split_model_id(model_1_id)
      model_2_parts <- split_model_id(model_2_id)

      model_1_losses <- loss_data %>%
        filter(
          Forecast_type == forecast_type,
          Method == model_1_parts$Method,
          Model == model_1_parts$Model
        ) %>%
        select(Meldedatum, loss_1 = all_of(loss_col))

      model_2_losses <- loss_data %>%
        filter(
          Forecast_type == forecast_type,
          Method == model_2_parts$Method,
          Model == model_2_parts$Model
        ) %>%
        select(Meldedatum, loss_2 = all_of(loss_col))

      paired_losses <- inner_join(
        model_1_losses,
        model_2_losses,
        by = "Meldedatum"
      ) %>%
        filter(is.finite(loss_1), is.finite(loss_2)) %>%
        mutate(loss_difference = loss_1 - loss_2)

      dm_result <- manual_dm_test(
        paired_losses$loss_difference,
        h = h
      )

      mean_loss_1 <- mean(paired_losses$loss_1, na.rm = TRUE)
      mean_loss_2 <- mean(paired_losses$loss_2, na.rm = TRUE)

      winner <- if (mean_loss_1 <= mean_loss_2) {
        model_1_id
      } else {
        model_2_id
      }

      decision <- case_when(
        is.na(dm_result$p_value) ~
          "DM unavailable; lower mean loss advances",
        dm_result$p_value < alpha & dm_result$mean_loss_difference < 0 ~
          "Model 1 significantly better",
        dm_result$p_value < alpha & dm_result$mean_loss_difference > 0 ~
          "Model 2 significantly better",
        TRUE ~
          "No significant difference; lower mean loss advances"
      )

      winners <- c(winners, winner)

      round_results[[pair_number]] <- tibble(
        Forecast_type = forecast_type,
        Loss_function = loss_col,
        Round = round_number,
        Pair = pair_number,
        Model_1 = model_1_id,
        Model_2 = model_2_id,
        T = dm_result$T,
        Mean_loss_1 = mean_loss_1,
        Mean_loss_2 = mean_loss_2,
        mean_loss_difference = dm_result$mean_loss_difference,
        DM_stat = dm_result$DM_stat,
        p_value = dm_result$p_value,
        Winner = winner,
        Decision = decision
      )

      pair_number <- pair_number + 1
    }

    all_rounds[[round_number]] <- bind_rows(round_results)
    candidates <- unique(winners)
    round_number <- round_number + 1
  }

  champion_id <- candidates[1]
  champion_parts <- split_model_id(champion_id)

  champion <- loss_data %>%
    filter(
      Forecast_type == forecast_type,
      Method == champion_parts$Method,
      Model == champion_parts$Model
    ) %>%
    summarise(
      Forecast_type = forecast_type,
      Loss_function = loss_col,
      Champion = champion_id,
      Mean_loss = mean(.data[[loss_col]], na.rm = TRUE),
      RMSFE_cases = sqrt(mean(Squared_Error_cases, na.rm = TRUE)),
      N = sum(is.finite(.data[[loss_col]]))
    )

  list(
    rounds = bind_rows(all_rounds),
    champion = champion
  )
}

# --- Original R Markdown chunk: rolling-window-dm-tournament, message=FALSE, warning=FALSE ---
rolling_dm_tournament <- run_dm_tournament(
  loss_data = all_model_losses,
  forecast_type = "Rolling window",
  loss_col = "Squared_Error_cases",
  h = 1,
  alpha = 0.05
)

rolling_dm_rounds <- rolling_dm_tournament$rounds
rolling_dm_best_model <- rolling_dm_tournament$champion

knitr::kable(
  rolling_dm_rounds,
  digits = 4,
  caption = "Iterative pairwise DM tournament: rolling-window forecasts"
)

knitr::kable(
  rolling_dm_best_model,
  digits = 4,
  caption = "Final rolling-window model selected by the DM tournament"
)

# --- Original R Markdown chunk: expanding-window-dm-tournament, message=FALSE, warning=FALSE ---
expanding_dm_tournament <- run_dm_tournament(
  loss_data = all_model_losses,
  forecast_type = "Expanding window",
  loss_col = "Squared_Error_cases",
  h = 1,
  alpha = 0.05
)

expanding_dm_rounds <- expanding_dm_tournament$rounds
expanding_dm_best_model <- expanding_dm_tournament$champion

knitr::kable(
  expanding_dm_rounds,
  digits = 4,
  caption = "Iterative pairwise DM tournament: expanding-window forecasts"
)

knitr::kable(
  expanding_dm_best_model,
  digits = 4,
  caption = "Final expanding-window model selected by the DM tournament"
)

# --- Original R Markdown chunk: combine-dm-winners, message=FALSE, warning=FALSE ---
best_dm_models <- bind_rows(
  rolling_dm_best_model,
  expanding_dm_best_model
)

knitr::kable(
  best_dm_models,
  digits = 4,
  caption = "Final models selected for rolling and expanding windows"
)

# --- Original R Markdown chunk: unnamed ---
# ------------------------------------------------------------
# 1. Manual Diebold-Mariano test
# ------------------------------------------------------------

manual_dm_test <- function(loss_difference, h = 1) {

  loss_difference <- as.numeric(loss_difference)
  loss_difference <- loss_difference[is.finite(loss_difference)]

  n_obs <- length(loss_difference)

  if (n_obs < 5) {
    return(
      tibble::tibble(
        DM_stat = NA_real_,
        p_value = NA_real_,
        mean_loss_difference = ifelse(
          n_obs > 0,
          mean(loss_difference),
          NA_real_
        ),
        long_run_variance = NA_real_,
        T = n_obs
      )
    )
  }

  loss_sd <- stats::sd(loss_difference)

  if (!is.finite(loss_sd) || loss_sd <= 0) {
    return(
      tibble::tibble(
        DM_stat = NA_real_,
        p_value = NA_real_,
        mean_loss_difference = mean(loss_difference),
        long_run_variance = NA_real_,
        T = n_obs
      )
    )
  }

  mean_difference <- mean(loss_difference)
  centered_difference <- loss_difference - mean_difference

  gamma_0 <- sum(centered_difference^2) / n_obs
  long_run_variance <- gamma_0

  if (h > 1) {
    maximum_lag <- min(h - 1, n_obs - 1)

    for (lag_i in seq_len(maximum_lag)) {

      gamma_i <- sum(
        centered_difference[(lag_i + 1):n_obs] *
          centered_difference[1:(n_obs - lag_i)]
      ) / n_obs

      bartlett_weight <- 1 - lag_i / h

      long_run_variance <- long_run_variance +
        2 * bartlett_weight * gamma_i
    }
  }

  if (
    !is.finite(long_run_variance) ||
    long_run_variance <= 0
  ) {
    return(
      tibble::tibble(
        DM_stat = NA_real_,
        p_value = NA_real_,
        mean_loss_difference = mean_difference,
        long_run_variance = long_run_variance,
        T = n_obs
      )
    )
  }

  dm_statistic <- mean_difference /
    sqrt(long_run_variance / n_obs)

  original_p_value <- 2 * stats::pnorm(
    -abs(dm_statistic)
  )

  tibble::tibble(
    DM_stat = dm_statistic,
    p_value = original_p_value,
    mean_loss_difference = mean_difference,
    long_run_variance = long_run_variance,
    T = n_obs
  )
}


# ------------------------------------------------------------
# 2. Functions for model identifiers
# ------------------------------------------------------------

create_model_id <- function(method, model) {
  paste(method, model, sep = " | ")
}

split_model_id <- function(model_id) {

  model_parts <- strsplit(
    model_id,
    " | ",
    fixed = TRUE
  )[[1]]

  list(
    Method = model_parts[1],
    Model = paste(
      model_parts[-1],
      collapse = " | "
    )
  )
}


# ------------------------------------------------------------
# 3. HLN-corrected DM tournament function
# ------------------------------------------------------------

run_hln_dm_tournament <- function(
  loss_data,
  forecast_type,
  loss_col = "Squared_Error_cases",
  h = 1,
  alpha = 0.05
) {

  # Check required columns
  required_columns <- c(
    "Meldedatum",
    "Method",
    "Forecast_type",
    "Model",
    loss_col
  )

  missing_columns <- setdiff(
    required_columns,
    names(loss_data)
  )

  if (length(missing_columns) > 0) {
    stop(
      paste(
        "The following columns are missing:",
        paste(missing_columns, collapse = ", ")
      )
    )
  }

  if (!is.numeric(h) || length(h) != 1 || h < 1) {
    stop("h must be a positive number.")
  }

  if (
    !is.numeric(alpha) ||
    length(alpha) != 1 ||
    alpha <= 0 ||
    alpha >= 1
  ) {
    stop("alpha must be between 0 and 1.")
  }

  # Keep the selected forecasting design and valid losses
  tournament_data <- loss_data %>%
    dplyr::filter(
      .data$Forecast_type == forecast_type
    ) %>%
    dplyr::mutate(
      Meldedatum = as.Date(.data$Meldedatum),
      Model_ID = create_model_id(
        .data$Method,
        .data$Model
      )
    ) %>%
    dplyr::filter(
      !is.na(.data$Meldedatum),
      is.finite(.data[[loss_col]])
    ) %>%
    dplyr::group_by(
      .data$Meldedatum,
      .data$Method,
      .data$Model,
      .data$Model_ID
    ) %>%
    dplyr::summarise(
      Loss = mean(
        .data[[loss_col]],
        na.rm = TRUE
      ),
      .groups = "drop"
    )

  if (nrow(tournament_data) == 0) {
    stop(
      paste(
        "No valid observations were found for",
        forecast_type
      )
    )
  }

  # Identify candidate models
  candidates <- tournament_data %>%
    dplyr::distinct(.data$Model_ID) %>%
    dplyr::pull(.data$Model_ID)

  if (length(candidates) == 0) {
    stop(
      paste(
        "No models were found for",
        forecast_type
      )
    )
  }

  # If there is only one model, return it directly
  if (length(candidates) == 1) {

    champion_parts <- split_model_id(candidates[1])

    champion <- tournament_data %>%
      dplyr::filter(
        .data$Model_ID == candidates[1]
      ) %>%
      dplyr::summarise(
        Forecast_type = forecast_type,
        Loss_function = loss_col,
        Method = champion_parts$Method,
        Model = champion_parts$Model,
        Champion = candidates[1],
        Mean_loss = mean(.data$Loss),
        RMSFE_cases = sqrt(mean(.data$Loss)),
        N = dplyr::n()
      )

    return(
      list(
        rounds = tibble::tibble(),
        champion = champion
      )
    )
  }

  all_rounds <- list()
  round_number <- 1

  # Continue until one model remains
  while (length(candidates) > 1) {

    # Rank remaining models by average loss
    candidate_order <- tournament_data %>%
      dplyr::filter(
        .data$Model_ID %in% candidates
      ) %>%
      dplyr::group_by(.data$Model_ID) %>%
      dplyr::summarise(
        Mean_loss = mean(.data$Loss),
        .groups = "drop"
      ) %>%
      dplyr::arrange(
        .data$Mean_loss,
        .data$Model_ID
      ) %>%
      dplyr::pull(.data$Model_ID)

    candidates <- candidate_order

    round_results <- list()
    winners <- character(0)
    pair_number <- 1

    pair_starts <- seq(
      from = 1,
      to = length(candidates),
      by = 2
    )

    for (start_i in pair_starts) {

      model_1_id <- candidates[start_i]

      # Automatically advance an unmatched model
      if (start_i == length(candidates)) {

        winners <- c(
          winners,
          model_1_id
        )

        round_results[[pair_number]] <-
          tibble::tibble(
            Forecast_type = forecast_type,
            Loss_function = loss_col,
            Round = round_number,
            Pair = pair_number,
            Model_1 = model_1_id,
            Model_2 = NA_character_,
            T = NA_integer_,
            Mean_loss_1 = NA_real_,
            Mean_loss_2 = NA_real_,
            mean_loss_difference = NA_real_,
            DM_stat = NA_real_,
            Original_p_value = NA_real_,
            HLN_factor = NA_real_,
            DM_HLN = NA_real_,
            p_value_HLN = NA_real_,
            Winner = model_1_id,
            Decision = "Automatically advances"
          )

        pair_number <- pair_number + 1
        next
      }

      model_2_id <- candidates[start_i + 1]

      # Obtain losses for Model 1
      model_1_losses <- tournament_data %>%
        dplyr::filter(
          .data$Model_ID == model_1_id
        ) %>%
        dplyr::select(
          .data$Meldedatum,
          loss_1 = .data$Loss
        )

      # Obtain losses for Model 2
      model_2_losses <- tournament_data %>%
        dplyr::filter(
          .data$Model_ID == model_2_id
        ) %>%
        dplyr::select(
          .data$Meldedatum,
          loss_2 = .data$Loss
        )

      # Compare the models only on common dates
      paired_losses <- dplyr::inner_join(
        model_1_losses,
        model_2_losses,
        by = "Meldedatum"
      ) %>%
        dplyr::filter(
          is.finite(.data$loss_1),
          is.finite(.data$loss_2)
        ) %>%
        dplyr::mutate(
          loss_difference =
            .data$loss_1 - .data$loss_2
        )

      number_of_common_dates <- nrow(paired_losses)

      # Handle the case with no common observations
      if (number_of_common_dates == 0) {

        overall_mean_loss_1 <- tournament_data %>%
          dplyr::filter(
            .data$Model_ID == model_1_id
          ) %>%
          dplyr::summarise(
            value = mean(.data$Loss)
          ) %>%
          dplyr::pull(.data$value)

        overall_mean_loss_2 <- tournament_data %>%
          dplyr::filter(
            .data$Model_ID == model_2_id
          ) %>%
          dplyr::summarise(
            value = mean(.data$Loss)
          ) %>%
          dplyr::pull(.data$value)

        winner <- if (
          overall_mean_loss_1 <= overall_mean_loss_2
        ) {
          model_1_id
        } else {
          model_2_id
        }

        winners <- c(winners, winner)

        round_results[[pair_number]] <-
          tibble::tibble(
            Forecast_type = forecast_type,
            Loss_function = loss_col,
            Round = round_number,
            Pair = pair_number,
            Model_1 = model_1_id,
            Model_2 = model_2_id,
            T = 0L,
            Mean_loss_1 = overall_mean_loss_1,
            Mean_loss_2 = overall_mean_loss_2,
            mean_loss_difference =
              overall_mean_loss_1 -
              overall_mean_loss_2,
            DM_stat = NA_real_,
            Original_p_value = NA_real_,
            HLN_factor = NA_real_,
            DM_HLN = NA_real_,
            p_value_HLN = NA_real_,
            Winner = winner,
            Decision = paste(
              "No common forecast dates;",
              "lower overall mean loss advances"
            )
          )

        pair_number <- pair_number + 1
        next
      }

      mean_loss_1 <- mean(
        paired_losses$loss_1
      )

      mean_loss_2 <- mean(
        paired_losses$loss_2
      )

      mean_loss_difference <-
        mean_loss_1 - mean_loss_2

      # Run the original DM test
      dm_result <- manual_dm_test(
        loss_difference =
          paired_losses$loss_difference,
        h = h
      )

      T_value <- as.integer(dm_result$T[1])
      original_dm <- as.numeric(
        dm_result$DM_stat[1]
      )
      original_p_value <- as.numeric(
        dm_result$p_value[1]
      )

      # Default corrected values
      HLN_factor <- NA_real_
      DM_HLN <- NA_real_
      p_value_HLN <- NA_real_

      # Apply the HLN correction when possible
      correction_numerator <-
        T_value +
        1 -
        2 * h +
        (h * (h - 1)) / T_value

      if (
        is.finite(original_dm) &&
        T_value > 1 &&
        is.finite(correction_numerator) &&
        correction_numerator > 0
      ) {

        HLN_factor <- sqrt(
          correction_numerator / T_value
        )

        DM_HLN <- original_dm * HLN_factor

        p_value_HLN <- 2 * stats::pt(
          -abs(DM_HLN),
          df = T_value - 1
        )
      }

      # The lower mean loss determines the model that advances
      lower_loss_model <- if (
        mean_loss_1 <= mean_loss_2
      ) {
        model_1_id
      } else {
        model_2_id
      }

      # Use the corrected p-value for the statistical decision
      if (!is.finite(p_value_HLN)) {

        winner <- lower_loss_model

        decision <- paste(
          "HLN-corrected DM test unavailable;",
          "lower mean loss advances"
        )

      } else if (
        p_value_HLN < alpha &&
        mean_loss_difference < 0
      ) {

        winner <- model_1_id
        decision <- "Model 1 is significantly better"

      } else if (
        p_value_HLN < alpha &&
        mean_loss_difference > 0
      ) {

        winner <- model_2_id
        decision <- "Model 2 is significantly better"

      } else {

        winner <- lower_loss_model

        decision <- paste(
          "No significant difference;",
          "lower mean loss advances"
        )
      }

      winners <- c(
        winners,
        winner
      )

      round_results[[pair_number]] <-
        tibble::tibble(
          Forecast_type = forecast_type,
          Loss_function = loss_col,
          Round = round_number,
          Pair = pair_number,
          Model_1 = model_1_id,
          Model_2 = model_2_id,
          T = T_value,
          Mean_loss_1 = mean_loss_1,
          Mean_loss_2 = mean_loss_2,
          mean_loss_difference =
            mean_loss_difference,
          DM_stat = original_dm,
          Original_p_value =
            original_p_value,
          HLN_factor = HLN_factor,
          DM_HLN = DM_HLN,
          p_value_HLN = p_value_HLN,
          Winner = winner,
          Decision = decision
        )

      pair_number <- pair_number + 1
    }

    all_rounds[[round_number]] <-
      dplyr::bind_rows(round_results)

    candidates <- unique(winners)
    round_number <- round_number + 1
  }

  # Extract final champion
  champion_id <- candidates[1]
  champion_parts <- split_model_id(champion_id)

  champion <- tournament_data %>%
    dplyr::filter(
      .data$Model_ID == champion_id
    ) %>%
    dplyr::summarise(
      Forecast_type = forecast_type,
      Loss_function = loss_col,
      Method = champion_parts$Method,
      Model = champion_parts$Model,
      Champion = champion_id,
      Mean_loss = mean(.data$Loss),
      RMSFE_cases = sqrt(mean(.data$Loss)),
      N = dplyr::n()
    )

  list(
    rounds = dplyr::bind_rows(all_rounds),
    champion = champion
  )
}


# ------------------------------------------------------------
# 4. Run the rolling-window tournament
# ------------------------------------------------------------

rolling_hln_dm_tournament <- run_hln_dm_tournament(
  loss_data = all_model_losses,
  forecast_type = "Rolling window",
  loss_col = "Squared_Error_cases",
  h = 1,
  alpha = 0.05
)

rolling_dm_rounds_hln <-
  rolling_hln_dm_tournament$rounds

rolling_dm_best_model_hln <-
  rolling_hln_dm_tournament$champion


# ------------------------------------------------------------
# 5. Run the expanding-window tournament
# ------------------------------------------------------------

expanding_hln_dm_tournament <- run_hln_dm_tournament(
  loss_data = all_model_losses,
  forecast_type = "Expanding window",
  loss_col = "Squared_Error_cases",
  h = 1,
  alpha = 0.05
)

expanding_dm_rounds_hln <-
  expanding_hln_dm_tournament$rounds

expanding_dm_best_model_hln <-
  expanding_hln_dm_tournament$champion


# ------------------------------------------------------------
# 6. Display rolling-window comparisons
# ------------------------------------------------------------

knitr::kable(
  rolling_dm_rounds_hln %>%
    dplyr::select(
      .data$Round,
      .data$Pair,
      .data$Model_1,
      .data$Model_2,
      .data$T,
      .data$mean_loss_difference,
      .data$DM_HLN,
      .data$p_value_HLN,
      .data$Winner,
      .data$Decision
    ),
  digits = 4,
  caption = paste(
    "HLN-corrected Diebold--Mariano tournament:",
    "rolling-window forecasts"
  )
)


# ------------------------------------------------------------
# 7. Display expanding-window comparisons
# ------------------------------------------------------------

knitr::kable(
  expanding_dm_rounds_hln %>%
    dplyr::select(
      .data$Round,
      .data$Pair,
      .data$Model_1,
      .data$Model_2,
      .data$T,
      .data$mean_loss_difference,
      .data$DM_HLN,
      .data$p_value_HLN,
      .data$Winner,
      .data$Decision
    ),
  digits = 4,
  caption = paste(
    "HLN-corrected Diebold--Mariano tournament:",
    "expanding-window forecasts"
  )
)


# ------------------------------------------------------------
# 8. Combine and display the selected models
# ------------------------------------------------------------

best_dm_models_hln <- dplyr::bind_rows(
  expanding_dm_best_model_hln,
  rolling_dm_best_model_hln
) %>%
  dplyr::arrange(.data$Forecast_type)

knitr::kable(
  best_dm_models_hln,
  digits = 4,
  caption = paste(
    "Best models selected by the HLN-corrected",
    "Diebold--Mariano tournament"
  )
)
