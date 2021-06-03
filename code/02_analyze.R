# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(metafor)

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
  filter(outcome == "radius_vBMD") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12",
      "Hansen et al. (2020) 24",
      "Lindeman et al. (2018) 24"
    )
  ) %>%
  select(-study_time)
tibia_vBMD <- data_percentage_change %>%
  filter(outcome == "tibia_vBMD") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12",
      "Hansen et al. (2020) 24",
      "Lindeman et al. (2018) 24"
    )
  ) %>%
  select(-study_time)
# QCT variables
LS_vBMD <- data_percentage_change %>%
  filter(outcome == "LS_vBMD")
TH_vBMD <- data_percentage_change %>%
  filter(outcome == "TH_vBMD")

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
  arrange(desc(yi)) %>%
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
  arrange(desc(yi)) %>%
  as_tibble()

# Multilevel meta-analysis model
tibia_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_vBMD
)

# Lumbar spine vBMD
#
# Calculate the effect size
LS_vBMD <- LS_vBMD %>%
  escalc(
    measure = "MN",
    mi = mean_percent_change,
    sdi = sd_percent_change,
    ni = n,
    data = .
  ) %>%
  arrange(desc(yi)) %>%
  as_tibble()

# Multilevel meta-analysis model
LS_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = LS_vBMD
)

# Total hip vBMD
#
# Calculate the effect size
TH_vBMD <- TH_vBMD %>%
  escalc(
    measure = "MN",
    mi = mean_percent_change,
    sdi = sd_percent_change,
    ni = n,
    data = .
  ) %>%
  arrange(desc(yi)) %>%
  as_tibble()

# Multilevel meta-analysis model
TH_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = TH_vBMD
)

# Meta-regression: time effect --------------------------------------------

# Radius vBMD
radius_vBMD_time_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  mods = ~ time_after_surgery,
  data = radius_vBMD
)

# Tibia vBMD
tibia_vBMD_time_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  mods = ~ time_after_surgery,
  data = tibia_vBMD
)

# Lumbar spine vBMD
LS_vBMD_time_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  mods = ~ time_after_surgery,
  data = LS_vBMD
)

# Total hip vBMD
TH_vBMD_time_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  mods = ~ time_after_surgery,
  data = TH_vBMD
)

# Save the meta-analysis objects ------------------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  radius_vBMD, radius_vBMD_model,
  tibia_vBMD, tibia_vBMD_model,
  LS_vBMD, LS_vBMD_model,
  TH_vBMD, TH_vBMD_model,
  file = here("output", "ma_objects.rda")
)
save(
  radius_vBMD_time_model,
  file = here("output", "meta_regression.rda")
)
