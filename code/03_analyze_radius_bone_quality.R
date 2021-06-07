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
# Cortical bone variables
radius_CT_vBMD <- data_percentage_change %>%
  filter(outcome == "radius_CT_vBMD") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(study_time != "Shanbhogue et al. (2017) 12") %>%
  select(-study_time)
radius_CT_porosity <- data_percentage_change %>%
  filter(outcome == "radius_CT_porosity") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
radius_CT_thickness <- data_percentage_change %>%
  filter(outcome == "radius_CT_thickness") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
 filter(study_time != "Hansen et al. (2020) 24") %>%
  select(-study_time)
# Trabecular bone variables
radius_TB_vBMD <- data_percentage_change %>%
  filter(outcome == "radius_TB_vBMD") %>%
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
radius_TB_number <- data_percentage_change %>%
  filter(outcome == "radius_TB_number") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
radius_TB_separation <- data_percentage_change %>%
  filter(outcome == "radius_TB_separation") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
radius_TB_thickness <- data_percentage_change %>%
  filter(outcome == "radius_TB_thickness") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
# Other variables
radius_BVTV <- data_percentage_change %>%
  filter(outcome == "radius_BVTV")
radius_failure_load <- data_percentage_change %>%
  filter(outcome == "radius_failure_load") %>%
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

# Meta-analysis -----------------------------------------------------------

# Radius cortical vBMD
#
# Calculate the effect size
radius_CT_vBMD <- radius_CT_vBMD %>%
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
radius_CT_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_CT_vBMD
)

# Radius cortical porosity
#
# Calculate the effect size
radius_CT_porosity <- radius_CT_porosity %>%
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
radius_CT_porosity_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_CT_porosity
)

# Radius cortical thickness
#
# Calculate the effect size
radius_CT_thickness <- radius_CT_thickness %>%
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
radius_CT_thickness_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_CT_thickness
)

# Radius trabecular vBMD
#
# Calculate the effect size
radius_TB_vBMD <- radius_TB_vBMD %>%
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
radius_TB_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_TB_vBMD
)

# Radius trabecular number
#
# Calculate the effect size
radius_TB_number <- radius_TB_number %>%
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
radius_TB_number_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_TB_number
)

# Radius trabecular separation
#
# Calculate the effect size
radius_TB_separation <- radius_TB_separation %>%
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
radius_TB_separation_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_TB_separation
)

# Radius trabecular thickness
#
# Calculate the effect size
radius_TB_thickness <- radius_TB_thickness %>%
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
radius_TB_thickness_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_TB_thickness
)

# Radius BVTV
#
# Calculate the effect size
radius_BVTV <- radius_BVTV %>%
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
radius_BVTV_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_BVTV
)

# Radius failure load
#
# Calculate the effect size
radius_failure_load <- radius_failure_load %>%
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
radius_failure_load_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_failure_load
)
