# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(ragg)
source(here("code", "funs.R"))

# Read and tidy data ------------------------------------------------------

load(here("output", "ma_DXA_QCT_objects.rda"))

models <- list(TH_DXA_QCT_model, LS_DXA_QCT_model, radius_DXA_QCT_model)
sites <- c("Total hip", "Lumbar spine", "Radius")
plot_df <- map2_dfr(models, sites, get_model_estimate) %>%
  mutate(
    site = factor(site, levels = c("Radius", "Lumbar spine", "Total hip"))
  )

# Make plot ---------------------------------------------------------------

DXA_QCT_plot <- ggplot(plot_df) +
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

# Save plot ---------------------------------------------------------------

agg_png(
  here("figures", "DXA_QCT.png"),
  width = 12,
  height = 5,
  units = "cm",
  res = 300,
  scaling = 0.8
)
DXA_QCT_plot
dev.off()
