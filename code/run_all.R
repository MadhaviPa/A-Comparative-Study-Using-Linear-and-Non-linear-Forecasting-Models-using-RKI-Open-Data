# Run this script from the repository root in R.
# Code is segmented from original_project.Rmd; no independent validation has been performed.
if (!file.exists("code/01_download_data.R")) stop("Set working directory to the repository root first.")
for (file in sprintf("code/%02d_%s.R", 1:7, c("download_data", "preprocessing", "feature_engineering", "linear_models", "tree_random_forest", "seasonal_models", "evaluation"))) {
  message("Running ", file)
  source(file, local = FALSE, echo = FALSE)
}
