# Extracted in the original relative order from the uploaded R Markdown analysis.
# Run using code/run_all.R from the repository root; intermediate objects are shared.

# --- Original R Markdown chunk: vc-2-daily-cases, message=FALSE, warning=FALSE ---
# 2. Daily cases

daily_cases <- covid %>%
  group_by(Meldedatum) %>%
  summarise(
    cases = sum(AnzahlFall, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  right_join(all_dates, by = "Meldedatum") %>%
  arrange(Meldedatum) %>%
  mutate(
    cases = replace_na(cases, 0),
    log_cases = log(cases + 1)
  )

# --- Original R Markdown chunk: vc-3-reporting-delay-covariate, message=FALSE, warning=FALSE ---
# 3. Reporting delay covariate
# Reporting-delay limits used in Project 2 Appendix B
minimum_reporting_delay <- -30
maximum_reporting_delay <- 60

reporting_delay_daily <- covid %>%
  # Use only records for which Refdatum is the actual symptom-onset date.
  filter(IstErkrankungsbeginn == 1) %>%
  mutate(
    reporting_delay_raw = as.numeric(Meldedatum - Refdatum),
    reporting_delay_clipped = pmin(
      pmax(reporting_delay_raw, minimum_reporting_delay),
      maximum_reporting_delay
    )
  ) %>%
  group_by(Meldedatum) %>%
  summarise(
    reporting_delay_avg = if_else(
      any(!is.na(reporting_delay_clipped)),
      mean(reporting_delay_clipped, na.rm = TRUE),
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  right_join(all_dates, by = "Meldedatum") %>%
  arrange(Meldedatum)

# --- Original R Markdown chunk: vc-4-symptom-onset-share-covariate, message=FALSE, warning=FALSE ---
# 4. Symptom-onset share covariate
# Construction follows the professor's recommendation.
# Both the numerator and denominator are restricted to genuinely new cases
# identified by NeuerFall == 1.

symptom_onset_daily <- covid %>%
  mutate(
    known_onset = as.integer(IstErkrankungsbeginn == 1),
    is_new_case = as.integer(NeuerFall == 1),

    # Denominator: genuinely new cases reported on day t.
    new_case_count = if_else(
      is_new_case == 1,
      AnzahlFall,
      0
    ),

    # Numerator: genuinely new cases with a known symptom-onset date.
    new_onset_case_count = if_else(
      is_new_case == 1 & known_onset == 1,
      AnzahlFall,
      0
    )
  ) %>%
  group_by(Meldedatum) %>%
  summarise(
    symptom_onset_cases = sum(new_onset_case_count, na.rm = TRUE),
    total_new_cases = sum(new_case_count, na.rm = TRUE),
    symptom_onset_share = if_else(
      total_new_cases > 0,
      symptom_onset_cases / total_new_cases,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  right_join(all_dates, by = "Meldedatum") %>%
  arrange(Meldedatum)

# --- Original R Markdown chunk: vc-5-age-share-covariates, message=FALSE, warning=FALSE ---
# 5. Age-share covariates

age_daily <- covid %>%
  group_by(Meldedatum, Altersgruppe) %>%
  summarise(
    age_cases = sum(AnzahlFall, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Meldedatum) %>%
  mutate(
    total_age_cases = sum(age_cases, na.rm = TRUE),
    age_share = if_else(total_age_cases > 0, age_cases / total_age_cases, 0),
    age_name = paste0("AgeShare_", make.names(as.character(Altersgruppe)))
  ) %>%
  ungroup() %>%
  select(Meldedatum, age_name, age_share) %>%
  pivot_wider(
    names_from = age_name,
    values_from = age_share,
    values_fill = 0
  ) %>%
  right_join(all_dates, by = "Meldedatum") %>%
  mutate(
    across(starts_with("AgeShare_"), ~ replace_na(.x, 0))
  )

# --- Original R Markdown chunk: vc-6-gender-share-covariates, message=FALSE, warning=FALSE ---
# 6. Gender-share covariates

gender_daily <- covid %>%
  mutate(
    gender_value = as.character(Geschlecht)
  ) %>%
  group_by(Meldedatum, gender_value) %>%
  summarise(
    gender_cases = sum(AnzahlFall, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(Meldedatum) %>%
  mutate(
    total_gender_cases = sum(gender_cases, na.rm = TRUE),
    gender_share = if_else(total_gender_cases > 0, gender_cases / total_gender_cases, 0),
    gender_name = paste0("GenderShare_", make.names(gender_value))
  ) %>%
  ungroup() %>%
  select(Meldedatum, gender_name, gender_share) %>%
  pivot_wider(
    names_from = gender_name,
    values_from = gender_share,
    values_fill = 0
  ) %>%
  right_join(all_dates, by = "Meldedatum") %>%
  mutate(
    across(starts_with("GenderShare_"), ~ replace_na(.x, 0))
  )

# --- Original R Markdown chunk: vc-8-weekday-and-public-holiday-variables, message=FALSE, warning=FALSE ---
# 8. Weekday and public holiday variables

holiday_dates <- as.Date(c(
  "2020-01-01", "2020-04-10", "2020-04-13", "2020-05-01",
  "2020-05-21", "2020-06-01", "2020-10-03", "2020-12-25", "2020-12-26",
  "2021-01-01", "2021-04-02", "2021-04-05", "2021-05-01",
  "2021-05-13", "2021-05-24", "2021-10-03", "2021-12-25", "2021-12-26",
  "2022-01-01", "2022-04-15", "2022-04-18", "2022-05-01",
  "2022-05-26", "2022-06-06", "2022-10-03", "2022-12-25", "2022-12-26",
  "2023-01-01", "2023-04-07", "2023-04-10", "2023-05-01",
  "2023-05-18", "2023-05-29", "2023-10-03", "2023-12-25", "2023-12-26",
  "2024-01-01", "2024-03-29", "2024-04-01", "2024-05-01",
  "2024-05-09", "2024-05-20", "2024-10-03", "2024-12-25", "2024-12-26",
  "2025-01-01", "2025-04-18", "2025-04-21", "2025-05-01",
  "2025-05-29", "2025-06-09", "2025-10-03", "2025-12-25", "2025-12-26",
  "2026-01-01"
))

calendar_daily <- all_dates %>%
  mutate(
    Weekday = factor(
      lubridate::wday(Meldedatum, label = TRUE, week_start = 1),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    ),
    StartOfWeek = if_else(Weekday == "Mon", 1, 0),
    public_holiday = if_else(Meldedatum %in% holiday_dates, 1, 0)
  )

# --- Original R Markdown chunk: vc-9-merge-all-covariates, message=FALSE, warning=FALSE ---
# 9. Merge all covariates

model_base <- daily_cases %>%
  left_join(age_daily, by = "Meldedatum") %>%
  left_join(gender_daily, by = "Meldedatum") %>%
  left_join(reporting_delay_daily, by = "Meldedatum") %>%
  left_join(symptom_onset_daily, by = "Meldedatum") %>%
  left_join(temperature_daily, by = "Meldedatum") %>%
  left_join(calendar_daily, by = "Meldedatum") %>%
  arrange(Meldedatum) %>%
  mutate(
    across(starts_with("AgeShare_"), ~ replace_na(.x, 0)),
    across(starts_with("GenderShare_"), ~ replace_na(.x, 0)),
    reporting_delay_avg = replace_na(reporting_delay_avg, 0),
    symptom_onset_cases = replace_na(symptom_onset_cases, 0),
    symptom_onset_share = replace_na(symptom_onset_share, 0),
    Temp = zoo::na.approx(Temp, na.rm = FALSE),
    Temp = zoo::na.locf(Temp, na.rm = FALSE),
    Temp = zoo::na.locf(Temp, fromLast = TRUE, na.rm = FALSE),
    districts_used = replace_na(districts_used, 0L),
    StartOfWeek = replace_na(StartOfWeek, 0),
    public_holiday = replace_na(public_holiday, 0)
  )
