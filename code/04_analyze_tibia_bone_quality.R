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
tibia_CT_vBMD <- data_percentage_change %>%
  filter(outcome == "tibia_CT_vBMD") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Lindeman et al. (2018) 24"
    )
  ) %>%
  select(-study_time)
tibia_CT_porosity <- data_percentage_change %>%
  filter(outcome == "tibia_CT_porosity") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
tibia_CT_thickness <- data_percentage_change %>%
  filter(outcome == "tibia_CT_thickness") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(study_time != "Hansen et al. (2020) 24") %>%
  select(-study_time)
# Trabecular bone variables
tibia_TB_vBMD <- data_percentage_change %>%
  filter(outcome == "tibia_TB_vBMD") %>%
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
tibia_TB_number <- data_percentage_change %>%
  filter(outcome == "tibia_TB_number") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
tibia_TB_separation <- data_percentage_change %>%
  filter(outcome == "tibia_TB_separation") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
tibia_TB_thickness <- data_percentage_change %>%
  filter(outcome == "tibia_TB_thickness") %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12", "Hansen et al. (2020) 24"
    )
  ) %>%
  select(-study_time)
# Other variables
tibia_BVTV <- data_percentage_change %>%
  filter(outcome == "tibia_BVTV")
tibia_failure_load <- data_percentage_change %>%
  filter(outcome == "tibia_failure_load") %>%
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

# Tibia cortical vBMD
#
# Calculate the effect size
tibia_CT_vBMD <- tibia_CT_vBMD %>%
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
tibia_CT_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_CT_vBMD
)

# Tibia cortical porosity
#
# Calculate the effect size
tibia_CT_porosity <- tibia_CT_porosity %>%
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
tibia_CT_porosity_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_CT_porosity
)

# Tibia cortical thickness
#
# Calculate the effect size
tibia_CT_thickness <- tibia_CT_thickness %>%
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
tibia_CT_thickness_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_CT_thickness
)

# Tibia trabecular vBMD
#
# Calculate the effect size
tibia_TB_vBMD <- tibia_TB_vBMD %>%
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
tibia_TB_vBMD_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_TB_vBMD
)

# Tibia trabecular number
#
# Calculate the effect size
tibia_TB_number <- tibia_TB_number %>%
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
tibia_TB_number_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_TB_number
)

# Tibia trabecular separation
#
# Calculate the effect size
tibia_TB_separation <- tibia_TB_separation %>%
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
tibia_TB_separation_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_TB_separation
)

# Tibia trabecular thickness
#
# Calculate the effect size
tibia_TB_thickness <- tibia_TB_thickness %>%
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
tibia_TB_thickness_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_TB_thickness
)

# Tibia BVTV
#
# Calculate the effect size
tibia_BVTV <- tibia_BVTV %>%
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
tibia_BVTV_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_BVTV
)

# tibia failure load
#
# Calculate the effect size
tibia_failure_load <- tibia_failure_load %>%
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
tibia_failure_load_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = tibia_failure_load
)
