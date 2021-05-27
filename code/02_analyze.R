# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(metafor)
library(ragg)

# Read and tidy data ------------------------------------------------------

load(here("data", "data_percentage_change.rda"))

# Create a variable to indicate which observations come from the same sample
studies <- unique(data_percentage_change$study)
data_percentage_change <- data_percentage_change %>%
  mutate(
    sample = as.numeric(as.factor(study)),
    sample = ifelse(
      study == "Yu et al. (2014)" |
      study == "Yu et al. (2015)",
      which(studies == "Lindeman et al. (2018)"),
      ifelse(
        study == "Shanbhogue et al. (2017)" |
        study == "Hansen et al. (2020)",
        which(studies == "Frederiksen et al. (2016)"),
        sample
      )
    ),
    .after = study
  )

# Separate the variables into smaller data.frames
# HR-pQCT variables
radius_vBMD <- data_percentage_change %>%
  filter(outcome == "radius_vBMD")
tibia_vBMD <- data_percentage_change %>%
  filter(outcome == "tibia_vBMD")

# Primary analysis --------------------------------------------------------

# Radius vBMD
#
# Calculate the effect size
radius_vBMD <- radius_vBMD %>%
  escalc(
    measure = "MN",
    mi = mean_percent_change,
    sdi = sd_percent_change,
    ni = n,
    data = .
  ) %>%
  as_tibble()

# Multilevel meta-analysis model
radius_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_vBMD
)

# Tibia vBMD
#
# Calculate the effect size
tibia_vBMD <- tibia_vBMD %>%
  escalc(
    measure = "MN",
    mi = mean_percent_change,
    sdi = sd_percent_change,
    ni = n,
    data = .
  ) %>%
  as_tibble()

# Multilevel meta-analysis model
tibia_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_vBMD
)

# Save the meta-analysis objects ------------------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  radius_vBMD, radius_vBMD_model,
  tibia_vBMD, tibia_vBMD_model,
  file = here("output", "ma_objects.rda")
)
