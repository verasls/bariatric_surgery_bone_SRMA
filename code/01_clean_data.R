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

# Merge all percentage change data
data_percentage_change <- data_percentage_change %>%
  rbind(data_percentage_change_cmp)

# Save the final data frame -----------------------------------------------

save(data_percentage_change, file = here("data", "data_percentage_change.rda"))
