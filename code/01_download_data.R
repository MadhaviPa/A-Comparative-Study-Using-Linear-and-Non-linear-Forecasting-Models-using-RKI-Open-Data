# Extracted in the original relative order from the uploaded R Markdown analysis.
# Run using code/run_all.R from the repository root; intermediate objects are shared.

# --- Original R Markdown chunk: setup, message=FALSE, warning=FALSE ---
library(tidyverse)
library(lubridate)
library(zoo)
library(readr)
library(stringr)
library(ggplot2)
library(scales)
library(knitr)
library(rpart)
library(rpart.plot)
library(ranger)
library(forecast)
library(data.table)

set.seed(123)

# --- Original R Markdown chunk: variable-construction-parameters, message=FALSE, warning=FALSE ---
set.seed(123)

start_date <- as.Date("2020-04-01")
end_date   <- as.Date("2026-03-31")
max_ar_p   <- 10

# ============================================================

# --- Original R Markdown chunk: vc-1-load-rki-covid-data, message=FALSE, warning=FALSE ---
# 1. Load RKI COVID data

url <- "https://zenodo.org/records/20791426/files/Aktuell_Deutschland_SarsCov2_Infektionen.csv?download=1"

download.file(
  url,
  destfile = "data/raw/rki_covid.csv",
  mode = "wb",
  method = "auto"
)

covid_file <- read_csv("data/raw/rki_covid.csv", show_col_types = FALSE)

covid <- covid_file %>%
  mutate(
    Meldedatum = as.Date(substr(Meldedatum, 1, 10)),
    Refdatum   = as.Date(substr(Refdatum, 1, 10)),
    AnzahlFall = as.numeric(AnzahlFall),
    IstErkrankungsbeginn = as.numeric(IstErkrankungsbeginn)
  ) %>%
  filter(
    Meldedatum >= start_date,
    Meldedatum <= end_date
  )

all_dates <- tibble(
  Meldedatum = seq(start_date, end_date, by = "day")
)

# --- Original R Markdown chunk: vc-7-temperature-covariate-from-dwd-daily-cli, message=FALSE, warning=FALSE ---
# 7. Temperature covariate from DWD daily climate data
# DWD KL variable: TMK = daily mean temperature

temp_url_hist   <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/historical/"
temp_url_recent <- "https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/daily/kl/recent/"

covid_start <- min(covid$Meldedatum, na.rm = TRUE)
covid_end   <- max(covid$Meldedatum, na.rm = TRUE)

# Get valid RKI Landkreis IDs
valid_landkreis_ids <- unique(as.integer(covid$IdLandkreis))
valid_landkreis_ids <- valid_landkreis_ids[!is.na(valid_landkreis_ids)]

# ------------------------------------------------------------
# Historical DWD ZIP file list
# ------------------------------------------------------------

zip_links_hist <- readLines(temp_url_hist, warn = FALSE)

zip_files_hist <- stringr::str_extract(
  zip_links_hist,
  "tageswerte_KL_[0-9]{5}_[0-9]{8}_[0-9]{8}_hist\\.zip"
)

zip_files_hist <- unique(na.omit(zip_files_hist))

zip_dt_hist <- data.table::data.table(
  zip_file = zip_files_hist
)

zip_dt_hist[, source := "historical"]
zip_dt_hist[, base_url := temp_url_hist]

zip_dt_hist[, STATIONS_ID := as.integer(
  stringr::str_extract(zip_file, "(?<=tageswerte_KL_)\\d{5}")
)]

zip_dt_hist[, start_date_dwd := as.Date(
  stringr::str_match(zip_file, "tageswerte_KL_[0-9]{5}_([0-9]{8})_")[, 2],
  format = "%Y%m%d"
)]

zip_dt_hist[, end_date_dwd := as.Date(
  stringr::str_match(zip_file, "_([0-9]{8})_hist\\.zip$")[, 2],
  format = "%Y%m%d"
)]

zip_dt_hist <- zip_dt_hist[
  start_date_dwd <= covid_end & end_date_dwd >= covid_start
]

# ------------------------------------------------------------
# Recent DWD ZIP file list
# ------------------------------------------------------------

zip_links_recent <- readLines(temp_url_recent, warn = FALSE)

zip_files_recent <- stringr::str_extract(
  zip_links_recent,
  "tageswerte_KL_[0-9]{5}_akt\\.zip"
)

zip_files_recent <- unique(na.omit(zip_files_recent))

zip_dt_recent <- data.table::data.table(
  zip_file = zip_files_recent
)

zip_dt_recent[, source := "recent"]
zip_dt_recent[, base_url := temp_url_recent]

zip_dt_recent[, STATIONS_ID := as.integer(
  stringr::str_extract(zip_file, "(?<=tageswerte_KL_)\\d{5}")
)]

zip_dt_recent[, start_date_dwd := as.Date(NA)]
zip_dt_recent[, end_date_dwd := as.Date(NA)]

# ------------------------------------------------------------
# Combine historical and recent files
# ------------------------------------------------------------

zip_dt <- data.table::rbindlist(
  list(zip_dt_hist, zip_dt_recent),
  fill = TRUE
)

# Keep only DWD station IDs that directly match RKI Landkreis IDs
zip_dt <- zip_dt[
  STATIONS_ID %in% valid_landkreis_ids
]

if (nrow(zip_dt) == 0) {
  stop("No DWD STATIONS_ID matched RKI IdLandkreis.")
}

# ------------------------------------------------------------
# Download and extract DWD temperature files
# ------------------------------------------------------------

temp_list <- vector("list", nrow(zip_dt))

for (i in seq_len(nrow(zip_dt))) {

  zip_path <- tempfile(fileext = ".zip")
  extract_dir <- tempfile(pattern = "dwd_")
  dir.create(extract_dir)

  one_temp <- tryCatch({

    download.file(
      url = paste0(zip_dt$base_url[i], zip_dt$zip_file[i]),
      destfile = zip_path,
      mode = "wb",
      quiet = TRUE
    )

    files_inside <- unzip(zip_path, list = TRUE)$Name

    product_file <- files_inside[
      grepl("^produkt_klima_tag_.*\\.txt$", files_inside)
    ][1]

    if (is.na(product_file)) {
      NULL
    } else {

      extracted_file <- unzip(
        zipfile = zip_path,
        files = product_file,
        exdir = extract_dir,
        overwrite = TRUE
      )

      temp_raw <- data.table::fread(
        extracted_file,
        sep = ";",
        na.strings = c("-999", "-999.0"),
        showProgress = FALSE
      )

      temp_raw[, .(
        STATIONS_ID = as.integer(STATIONS_ID),
        Meldedatum = as.Date(as.character(MESS_DATUM), format = "%Y%m%d"),
        temperature = as.numeric(TMK)
      )]
    }

  }, error = function(e) {
    NULL
  })

  temp_list[[i]] <- one_temp

  unlink(zip_path, force = TRUE)
  unlink(extract_dir, recursive = TRUE, force = TRUE)
}

# ------------------------------------------------------------
# Combine, clean duplicates, and compute Germany-level temperature
# ------------------------------------------------------------

temp_all <- data.table::rbindlist(
  temp_list,
  fill = TRUE
)

temp_all <- temp_all[
  Meldedatum >= covid_start & Meldedatum <= covid_end
]

temp_all_clean <- temp_all[
  ,
  .(
    temperature = mean(temperature, na.rm = TRUE)
  ),
  by = .(STATIONS_ID, Meldedatum)
]

temp_all_clean <- temp_all_clean[
  STATIONS_ID %in% valid_landkreis_ids
]

temp_germany <- temp_all_clean[
  ,
  .(
    temperature = mean(temperature, na.rm = TRUE),
    districts_used = uniqueN(STATIONS_ID)
  ),
  by = Meldedatum
]

temp_germany[, Meldedatum := as.Date(Meldedatum)]

temperature_daily <- as.data.frame(temp_germany) %>%
  mutate(
    Meldedatum = as.Date(Meldedatum),
    Temp = as.numeric(temperature),
    districts_used = as.integer(districts_used)
  ) %>%
  select(
    Meldedatum,
    Temp,
    districts_used
  ) %>%
  arrange(Meldedatum)

# Fill missing days
temperature_daily <- all_dates %>%
  left_join(temperature_daily, by = "Meldedatum") %>%
  arrange(Meldedatum) %>%
  mutate(
    Temp = zoo::na.approx(Temp, na.rm = FALSE),
    Temp = zoo::na.locf(Temp, na.rm = FALSE),
    Temp = zoo::na.locf(Temp, fromLast = TRUE, na.rm = FALSE),
    districts_used = replace_na(districts_used, 0L)
  )

temperature_summary <- temperature_daily %>%
  summarise(
    Start_date = min(Meldedatum, na.rm = TRUE),
    End_date = max(Meldedatum, na.rm = TRUE),
    Observations = n(),
    Missing_Temp = sum(is.na(Temp)),
    Mean_Temp = mean(Temp, na.rm = TRUE),
    Min_Temp = min(Temp, na.rm = TRUE),
    Max_Temp = max(Temp, na.rm = TRUE),
    Min_districts_used = min(districts_used, na.rm = TRUE),
    Mean_districts_used = mean(districts_used, na.rm = TRUE),
    Max_districts_used = max(districts_used, na.rm = TRUE)
  )

temperature_summary
