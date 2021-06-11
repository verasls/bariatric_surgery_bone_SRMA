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
TH_DXA_QCT <- data_percentage_change %>%
  filter(outcome %in% c("TH_vBMD", "TH_aBMD")) %>%
  pivot_wider(
    names_from = outcome,
    values_from = c(n, mean_percent_change, sd_percent_change)
  ) %>%
  na.omit() %>%
  select(
    study, sample, time_after_surgery,
    n_DXA = n_TH_aBMD,
    n_QCT = n_TH_vBMD,
    mean_percent_change_DXA = mean_percent_change_TH_aBMD,
    mean_percent_change_QCT = mean_percent_change_TH_vBMD,
    sd_percent_change_DXA = sd_percent_change_TH_aBMD,
    sd_percent_change_QCT = sd_percent_change_TH_vBMD
  )
LS_DXA_QCT <- data_percentage_change %>%
  filter(outcome %in% c("LS_vBMD", "LS_aBMD")) %>%
  pivot_wider(
    names_from = outcome,
    values_from = c(n, mean_percent_change, sd_percent_change)
  ) %>%
  na.omit() %>%
  select(
    study, sample, time_after_surgery,
    n_DXA = n_LS_aBMD,
    n_QCT = n_LS_vBMD,
    mean_percent_change_DXA = mean_percent_change_LS_aBMD,
    mean_percent_change_QCT = mean_percent_change_LS_vBMD,
    sd_percent_change_DXA = sd_percent_change_LS_aBMD,
    sd_percent_change_QCT = sd_percent_change_LS_vBMD
  )
radius_DXA_QCT <- data_percentage_change %>%
  filter(outcome %in% c("radius_vBMD", "TR_aBMD")) %>%
  mutate(study_time = paste(study, time_after_surgery)) %>%
  # Remove some time points of some studies due to sample sobreposition
  filter(
    study_time %!in% c(
      "Shanbhogue et al. (2017) 12",
      "Hansen et al. (2020) 24",
      "Lindeman et al. (2018) 24"
    )
  ) %>%
  select(-study_time) %>%
  pivot_wider(
    names_from = outcome,
    values_from = c(n, mean_percent_change, sd_percent_change)
  ) %>%
  na.omit() %>%
  select(
    study, sample, time_after_surgery,
    n_DXA = n_TR_aBMD,
    n_QCT = n_radius_vBMD,
    mean_percent_change_DXA = mean_percent_change_TR_aBMD,
    mean_percent_change_QCT = mean_percent_change_radius_vBMD,
    sd_percent_change_DXA = sd_percent_change_TR_aBMD,
    sd_percent_change_QCT = sd_percent_change_radius_vBMD
  )

# Meta-analysis -----------------------------------------------------------

# Total hip
#
# Compare DXA vs. QCT
# Calculate the effect size
TH_DXA_QCT <- TH_DXA_QCT %>%
  mutate(site = "Total hip") %>%
  escalc(
    measure = "MD",
    m1i = mean_percent_change_DXA,
    sd1i = sd_percent_change_DXA,
    n1i = n_DXA,
    m2i = mean_percent_change_QCT,
    sd2i = sd_percent_change_QCT,
    n2i = n_QCT,
    data = .
  ) %>%
  arrange(desc(yi)) %>%
  as_tibble()
# Multilevel meta-analysis model
TH_DXA_QCT_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = TH_DXA_QCT
)

# Lumbar spine
#
# Calculate the effect size
LS_DXA_QCT <- LS_DXA_QCT %>%
  mutate(site = "Lumbar spine") %>%
  escalc(
    measure = "MD",
    m1i = mean_percent_change_DXA,
    sd1i = sd_percent_change_DXA,
    n1i = n_DXA,
    m2i = mean_percent_change_QCT,
    sd2i = sd_percent_change_QCT,
    n2i = n_QCT,
    data = .
  ) %>%
  arrange(desc(yi)) %>%
  as_tibble()
# Multilevel meta-analysis model
LS_DXA_QCT_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = LS_DXA_QCT
)

# Radius
#
# Calculate the effect size
radius_DXA_QCT <- radius_DXA_QCT %>%
  mutate(site = "Radius") %>%
  escalc(
    measure = "MD",
    m1i = mean_percent_change_DXA,
    sd1i = sd_percent_change_DXA,
    n1i = n_DXA,
    m2i = mean_percent_change_QCT,
    sd2i = sd_percent_change_QCT,
    n2i = n_QCT,
    data = .
  ) %>%
  arrange(desc(yi)) %>%
  as_tibble()
# Multilevel meta-analysis model
radius_DXA_QCT_model <- rma.mv(
  yi, vi,
  random = ~ 1 | sample / study,
  data = radius_DXA_QCT
)

# Save the meta-analysis objects ------------------------------------------

if (!dir.exists(here("output"))) {
  dir.create(here("output"))
}
save(
  TH_DXA_QCT_model, LS_DXA_QCT_model, radius_DXA_QCT_model
  file = here("output", "ma_DXA_QCT_objects.rda")
)




models <- list(TH_DXA_QCT_model, LS_DXA_QCT_model, radius_DXA_QCT_model)
sites <- c("Total hip", "Lumbar spine", "Radius")
plot_df <- map2_dfr(models, sites, get_model_estimate)


agg_png(
  here("figures", "DXA_QCT.png"),
  width = 12,
  height = 5,
  units = "cm",
  res = 300,
  scaling = 0.8
)
ggplot(plot_df) +
  geom_point(
    aes(x = mean, y = site)
  ) +
  geom_errorbarh(
    aes(xmin = ci_lower, xmax = ci_upper, y = site, height = 0.1)
  ) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  scale_x_continuous(
    limits = c(-15, 15),
    breaks = seq(-15, 15, 5)
  ) +
  theme_classic() +
  theme(
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 12)
  ) +
  xlab("Higher decrease DXA                     Higher decrease QCT")
dev.off()
