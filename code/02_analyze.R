# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(metafor)
library(dmetar)
source(here("code", "funs.R"))

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
# QCT variables
TH_vBMD <- data_percentage_change %>%
  filter(outcome == "TH_vBMD")
LS_vBMD <- data_percentage_change %>%
  filter(outcome == "LS_vBMD")
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

# Primary analysis --------------------------------------------------------

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

# Analyse heterogeneity ---------------------------------------------------

models <- list(
  TH_vBMD_model, LS_vBMD_model,
  radius_vBMD_model, tibia_vBMD_model
)
site <- c(
  "Total hip", "Lumbar spine",
  "Radius", "Tibia"
)
heterogeneity <- map2_dfr(models, site, variance_components)

# Sensitivity analysis: Lumbar spine
#
# Removing Ivaska et al. (2017)
LS_vBMD_s1 <- LS_vBMD %>%
  filter(study != "Ivaska et al. (2017)")
LS_vBMD_model_s1 <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = LS_vBMD_s1
)
# Removing Brzozowska et al. (2021) at 24 months
LS_vBMD_s2 <- LS_vBMD %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  filter(study_time != "Brzozowska et al. (2021) 24")
LS_vBMD_model_s2 <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = LS_vBMD_s2
)
# Removing both studies
LS_vBMD_s3 <- LS_vBMD %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  filter(
    study_time %!in% c("Brzozowska et al. (2021) 24", "Ivaska et al. (2017) 6")
  )
LS_vBMD_model_s3 <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = LS_vBMD_s3
)
# Analyse the differences in heterogeneity
models_s <- list(
  LS_vBMD_model_s1,
  LS_vBMD_model_s2,
  LS_vBMD_model_s3
)
site_s <- c(
  "Without Ivaska",
  "Without Brzozowska",
  "Without both"
)
heterogeneity_s <- map2_dfr(models_s, site_s, variance_components)

# Meta-regression: time effect --------------------------------------------

# Total hip vBMD
TH_vBMD_time_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  mods = ~ time_after_surgery,
  data = TH_vBMD
)

# Lumbar spine vBMD
LS_vBMD_time_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  mods = ~ time_after_surgery,
  data = LS_vBMD
)

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

# Save the meta-analysis objects ------------------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  TH_vBMD, TH_vBMD_model,
  LS_vBMD, LS_vBMD_model,
  radius_vBMD, radius_vBMD_model,
  tibia_vBMD, tibia_vBMD_model,
  file = here("output", "ma_objects.rda")
)
save(
  TH_vBMD_time_model,
  LS_vBMD_time_model,
  radius_vBMD_time_model,
  tibia_vBMD_time_model,
  file = here("output", "meta_regression.rda")
)
