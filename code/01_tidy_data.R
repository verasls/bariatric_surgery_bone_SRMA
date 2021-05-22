# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(readxl)
library(lvmisc)
source(here("code", "funs.R"))

# Read and tidy data ------------------------------------------------------

data <- read_xlsx(here("data", "raw", "data_LV.xlsx")) %>%
  mutate(
    across(
      -c(
        author, year, study, title, study_design,
        surgery_type, outcomes, obs
      ),
      as.numeric
    ),
    id = paste0(author, " et al (", year, ")")
  ) %>%
  select(id, time_after_surgery, outcome = outcomes, where(is.numeric), - year)

# Filter only the data which is already reported as mean percentage change and
# compute the standard deviation where needed
data_percentage_change <- data %>%
  filter(!is.na(mean_percent_change)) %>%
  mutate(
    # Compute the standard deviation of the percentage change
    sd_percent_change = ifelse(
      # From the standard error of the mean
      !is.na(mean_percent_change) &
      !is.na(sem_percent_lower) &
      !is.na(sem_percent_upper),
      compute_sd(
        from = "se", n,
        mean_percent_change, sem_percent_lower, sem_percent_upper
      ),
      ifelse(
      # From the 95% confidence interval
        !is.na(mean_percent_change) &
        !is.na(`95_ci_percent_lower`) &
        !is.na(`95_ci_percent_upper`),
        compute_sd(
          from = "ci", n,
          mean_percent_change, `95_ci_percent_lower`, `95_ci_percent_upper`
        ),
        sd_percent_change
      )
    )
  ) %>%
  select(
    id, time_after_surgery, outcome, n, mean_percent_change, sd_percent_change
  )

# Filter only the data reported as absolute values and compute mean and
# standard deviation where needed
data_absolute <- data %>%
  filter(!is.na(mean) | !is.na(median)) %>%
  mutate(
    # Compute the mean from the median, 1st and 3rd quartiles
    mean = ifelse(
      is.na(mean) & !is.na(median),
      compute_mean(median, iqr_lower, iqr_upper),
      mean
    ),
    # Compute the standard deviation from the 1st and 3rd quartiles and
    # sample size
    sd = ifelse(
      is.na(sd) & !is.na(iqr_lower) & !is.na(iqr_upper),
      compute_sd(
        from = "iqr", n = n, mean = NA,
        x_lower = iqr_lower, x_upper = iqr_upper
      ),
      sd
    )
  ) %>%
  select(
    id, time_after_surgery, outcome, n, mean, sd
  )

# Compute the percentage change mean and standard deviation
# from the absolute values
data_percentage_change_tmp <- data_absolute %>%
  # Remove Schafer (2018) as it already has percent change values
  filter(id != "Schafer et al (2018)") %>%
  pivot_wider(
    names_from = time_after_surgery,
    values_from = c(n, mean, sd)
  ) %>%
  mutate(
    # Compute the mean percentage change
    mean_percent_change_12 = ((mean_12 - mean_0) / mean_0) * 100,
    mean_percent_change_24 = ((mean_24 - mean_0) / mean_0) * 100,
    # Compute the standard deviation from the mean percentage change
    sd_mean_change_12 = compute_sd_mean_change(sd_0, sd_12, 0.5),
    sd_mean_change_24 = compute_sd_mean_change(sd_0, sd_24, 0.5),
    sd_percent_change_12 = sd_mean_change_12 / sd_0,
    sd_percent_change_24 = sd_mean_change_24 / sd_0
  ) %>%
  select(
    id, outcome, n_12, n_24,
    mean_percent_change_12, mean_percent_change_24,
    sd_percent_change_12, sd_percent_change_24
  )

data_percentage_change_tmp_12 <- data_percentage_change_tmp %>%
  select(
    id, outcome,
    n = n_12,
    mean_percent_change = mean_percent_change_12,
    sd_percent_change = sd_percent_change_12
  ) %>%
  mutate(time_after_surgery = 12, .after = id)

data_percentage_change_tmp_24 <- data_percentage_change_tmp %>%
  select(
    id, outcome,
    n = n_24,
    mean_percent_change = mean_percent_change_24,
    sd_percent_change = sd_percent_change_24
  ) %>%
  mutate(time_after_surgery = 24, .after = id)

data_percentage_change_tmp_all <- rbind(
  data_percentage_change_tmp_12, data_percentage_change_tmp_24
)

# Merge all percentage change data
data_percentage_change <- data_percentage_change %>%
  rbind(data_percentage_change_tmp_all)

# Save the final data frame -----------------------------------------------

save(data_percentage_change, file = here("data", "data_percentage_change.rda"))
