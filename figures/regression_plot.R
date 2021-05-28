# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(metafor)
library(ragg)
source(here("code", "funs.R"))

# Read data ---------------------------------------------------------------

load(here("output", "ma_objects.rda"))

# Radius vBMD meta-regression (time effect) plot --------------------------

agg_tiff(
  here("figures", "regplot_radius.tiff"),
  width = 15,
  height = 15,
  units = "cm",
  res = 200
)
regplot(
  radius_vBMD_time_model,
  shade = "gray95",
  xlab = "Time after surgery (months)",
  ylab = "Mean of radius vBMD change post-RYGB (%)",
  xlim = c(0, 90),
  bty = "l"
)
axis(side = 1, at = seq(0, 90, 10))
dev.off()

# Tibia vBMD meta-regression (time effect) plot ---------------------------

agg_tiff(
  here("figures", "regplot_tibia.tiff"),
  width = 15,
  height = 15,
  units = "cm",
  res = 200
)
regplot(
  tibia_vBMD_time_model,
  shade = "gray95",
  xlab = "Time after surgery (months)",
  ylab = "Mean of tibia vBMD change post-RYGB (%)",
  xlim = c(0, 90),
  bty = "l"
)
axis(side = 1, at = seq(0, 90, 10))
dev.off()

# Lumbar spine vBMD meta-regression (time effect) plot --------------------

agg_tiff(
  here("figures", "regplot_LS.tiff"),
  width = 15,
  height = 15,
  units = "cm",
  res = 200
)
regplot(
  LS_vBMD_time_model,
  shade = "gray95",
  xlab = "Time after surgery (months)",
  ylab = "Mean of lumbar spine vBMD change post-RYGB (%)",
  xlim = c(0, 30),
  bty = "l"
)
axis(side = 1, at = seq(0, 30, 10))
dev.off()

# Total hip vBMD meta-regression (time effect) plot -----------------------

agg_tiff(
  here("figures", "regplot_TH.tiff"),
  width = 15,
  height = 15,
  units = "cm",
  res = 200
)
regplot(
  TH_vBMD_time_model,
  shade = "gray95",
  xlab = "Time after surgery (months)",
  ylab = "Mean of total hip vBMD change post-RYGB (%)",
  xlim = c(0, 30),
  bty = "l"
)
axis(side = 1, at = seq(0, 30, 10))
dev.off()
