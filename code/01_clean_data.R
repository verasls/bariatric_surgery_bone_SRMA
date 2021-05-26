# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
source(here("code", "funs.R"))

# Read and tidy data ------------------------------------------------------

data <- read_csv(here("data", "raw", "data_all.csv")) %>%
  mutate(id = paste0(author, " et al. (", year, ")"), .before = author) %>%
  select(id, time_after_surgery, outcome = outcomes, where(is.numeric), - year)

# Transformations to percentage change ------------------------------------

# Filter only the data which is already reported as mean percentage change and
# compute the standard deviation where needed
data_percentage_change <- data %>%
  filter(!is.na(mean_percent_change)) %>%
  mutate(
    # Compute the SEM percentage change as a single value
    sem_percent_change = ifelse(
      # When both lower and upper limits are available
      !is.na(sem_percent_lower) & !is.na(sem_percent_upper),
      ((mean_percent_change - sem_percent_lower) +
        (sem_percent_upper - mean_percent_change)) / 2,
      # When only the lower value is available
      ifelse(
        !is.na(sem_percent_lower) & is.na(sem_percent_upper),
        mean_percent_change - sem_percent_lower,
      # When only the upper value is available
        ifelse(
          is.na(sem_percent_lower) & !is.na(sem_percent_upper),
          sem_percent_upper - mean_percent_change,
          sem_percent_change
        )
      )
    ),
    # Compute the standard deviation of the percentage change
    sd_percent_change = ifelse(
      # From the standard error of the mean
      !is.na(mean_percent_change) &
      !is.na(sem_percent_change),
      sem_percent_change * sqrt(n),
      ifelse(
      # From the 95% confidence interval
        !is.na(mean_percent_change) &
        !is.na(`95_ci_percent_lower`) &
        !is.na(`95_ci_percent_upper`),
        compute_sd(
          from = "ci", n,
          mean_percent_change, `95_ci_percent_lower`, `95_ci_percent_upper`
        ),
        ifelse(
      # From the lower limit of the standard deviation (only Bredella 2017)
          !is.na(mean_percent_change) &
          !is.na(sd_percent_lower),
          mean_percent_change - sd_percent_lower,
          sd_percent_change
        )
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
  # Remove papers from Yu et al, as they already report data as
  # percentage change
  filter(id %!in% c("Yu et al. (2014)", "Yu et al. (2015)")) %>%
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
#
# Create a vector of the time points from all the studies and remove baseline
time_points <- sort(unique(data_absolute$time_after_surgery))[- 1]
# Create an empty list to accomodate the data
percentage_change_list <- vector("list", length(time_points))
# Compute the % change for each time point
for (i in seq_along(time_points)) {
  percentage_change_list[[i]] <- data_absolute %>%
    filter(time_after_surgery %in% c(0, time_points[i])) %>%
    pivot_wider(
      names_from = time_after_surgery,
      values_from = c(n, mean, sd)
    ) %>%
    na.omit() %>%
    mutate(
      # Compute the mean percentage change
      mean_percent_change = (.data[[paste0("mean_", time_points[i])]] - mean_0)
        / mean_0 * 100,
      # Compute the standard deviation from the mean percentage change
      sd_mean_change = compute_sd_mean_change(
        sd_0, .data[[paste0("mean_", time_points[i])]], 0.5
      ),
      sd_percent_change = sd_mean_change / sd_0
    ) %>%
    select(
      id, outcome, n = .data[[paste0("n_", time_points[i])]],
      mean_percent_change, sd_percent_change
    ) %>%
    mutate(time_after_surgery = time_points[i], .after = id)
}
data_percentage_change_cmp <- map_dfr(percentage_change_list, rbind)

# Schafer et al. (2018) reports body composition data as absolute change
# Need to transform to percentage change
schafer_2018_tmp <- data %>%
  filter(
    id == "Schafer et al. (2018)" &
    outcome %in% c("BMI", "body_mass", "fat_mass", "lean_mass")
  ) %>%
  # Unite absolute baseline values and absolute changes at follow-up
  # into single variables (for mean and sd) in order to pivot wider this
  # data.frame and compute the percentage change
  mutate(
    X = ifelse(is.na(mean), mean_change, mean),
    SD = ifelse(is.na(sd), sd_change, sd)
  ) %>%
  select(id, time_after_surgery, outcome, n, X, SD) %>%
  pivot_wider(
    names_from = time_after_surgery,
    values_from = c(n, X, SD)
  ) %>%
  # Compute the percentage change
  mutate(
    mean_percent_change_6 = (X_6 / X_0) * 100,
    mean_percent_change_12 = (X_12 / X_0) * 100,
    sd_percent_change_6 = SD_6 / SD_0,
    sd_percent_change_12 = SD_12 / SD_0
  ) %>%
  select(id, outcome, starts_with(c("mean_p", "sd_p")))
schafer_2018_6 <- schafer_2018_tmp %>%
  select(
    id, outcome,
    mean_percent_change = mean_percent_change_6,
    sd_percent_change = sd_percent_change_6
  ) %>%
  mutate(
    time_after_surgery = 6, .after = id
  )
schafer_2018_12 <- schafer_2018_tmp %>%
  select(
    id, outcome,
    mean_percent_change = mean_percent_change_12,
    sd_percent_change = sd_percent_change_12
  ) %>%
  mutate(
    time_after_surgery = 12, .after = id
  )
schafer_2018 <- rbind(schafer_2018_6, schafer_2018_12) %>%
  mutate(n = 45, .after = outcome)

# Merge all percentage change data
data_percentage_change <- data_percentage_change %>%
  rbind(data_percentage_change_cmp, schafer_2018) %>%
  arrange(id, time_after_surgery, outcome)

# Transformations to absolute change --------------------------------------

# 1st criterion to have absolute change data:
# Data already reported as absolute change
data_absolute_change_1 <- data %>%
  select(
    id, time_after_surgery, outcome, n,
    mean_absolute_change = mean_change,
    sd_absolute_change = sd_change
  ) %>%
  na.omit()

# 2nd criterion to have absolute change data:
# Data reported as absolute values at baseline and post surgery
data_absolute_change_2_tmp <- data %>%
  filter(
    !is.na(mean) | !is.na(sd) |
    !is.na(median) | !is.na(iqr_lower) | !is.na(iqr_upper)
  ) %>%
  mutate(
    mean = ifelse(
      is.na(mean), compute_mean(median, iqr_lower, iqr_upper), mean
    ),
    sd = ifelse(
      is.na(sd),
      compute_sd(
        from = "iqr", n, mean = NA, x_lower = iqr_lower, x_upper = iqr_upper
      ),
      sd
    )
  ) %>%
  select(id, time_after_surgery, outcome, n, mean, sd)
# Create a vector of the time points from all the studies and remove baseline
time_points <- sort(unique(data_absolute_change_2_tmp$time_after_surgery))[- 1]
# Create an empty list to accomodate the data
absolute_change_list <- vector("list", length(time_points))
# Compute the absolute change for each time point
for (i in seq_along(time_points)) {
  absolute_change_list[[i]] <- data_absolute_change_2_tmp %>%
    filter(time_after_surgery %in% c(0, time_points[i])) %>%
    pivot_wider(
      names_from = time_after_surgery,
      values_from = c(n, mean, sd)
    ) %>%
    na.omit() %>%
    mutate(
      # Compute the mean absolute change
      mean_absolute_change = .data[[paste0("mean_", time_points[i])]] - mean_0,
      sd_absolute_change = compute_sd_mean_change(
        sd_0, .data[[paste0("sd_", time_points[i])]], 0.5
      )
    ) %>%
    select(
      id, outcome, n = .data[[paste0("n_", time_points[i])]],
      mean_absolute_change, sd_absolute_change
    ) %>%
    mutate(time_after_surgery = time_points[i], .after = id)
}
data_absolute_change_2 <- map_dfr(absolute_change_list, rbind)

# Merge all absolute change data
data_absolute_change <- data_absolute_change_1 %>%
  rbind(data_absolute_change_2) %>%
  arrange(id, time_after_surgery, outcome)

# Save the final data frames ----------------------------------------------

save(data_percentage_change, file = here("data", "data_percentage_change.rda"))
save(data_absolute_change, file = here("data", "data_absolute_change.rda"))
