# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(metafor)
library(ragg)
library(ggplotify)
library(cowplot)
source(here("code", "funs.R"))

# Read data ---------------------------------------------------------------

load(here("output", "ma_objects.rda"))
load(here("output", "meta_regression_objects.rda"))

# Obs ---------------------------------------------------------------------

# The code for generating each regression plot was wrapped in a function in
# order to allow it to be transformed into a ggplot object with {ggplotify}.
# This transformation was done to achieve a better control in the plot grid
# using {cowplot}.

# Total hip vBMD meta-regression (time effect) plot -----------------------

TH_vBMD_time_model_r2 <- round(
  r2(TH_vBMD_model, TH_vBMD_time_model), 2
)
make_TH_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    TH_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of total hip vBMD \n change post-RYGB (%)",
    xlim = c(0, 40),
    ylim = c(-15, 6),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 40, 10))
  # Plot regression equation text
  reg_equation(TH_vBMD_time_model, "time", 40, 5.4)
  # Plot p and R^2 values
  text(
    40, 3.9, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) == .698 * ";" ~ R^2 == "0.00")
  )
}

# Lumbar spine vBMD meta-regression (time effect) plot --------------------

LS_vBMD_time_model_r2 <- round(
  r2(LS_vBMD_model, LS_vBMD_time_model), 2
)
make_LS_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    LS_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of lumbar spine vBMD \n change post-RYGB (%)",
    xlim = c(0, 30),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 30, 10))
  # Plot regression equation text
  reg_equation(LS_vBMD_time_model, "time", 30, 5)
  # Plot p and R^2 values
  text(
    30, 3.7, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) == .009 * ";" ~ R^2 == "0.25")
  )
}

# Radius vBMD meta-regression (time effect) plot --------------------------

radius_vBMD_time_model_r2 <- round(
  r2(radius_vBMD_model, radius_vBMD_time_model), 2
)
make_radius_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius vBMD \n change post-RYGB (%)",
    xlim = c(0, 90),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_vBMD_time_model, "time", 90, 2)
  # Plot p and R^2 values
  text(
    90, 0, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == .84)
  )
}

# Tibia vBMD meta-regression (time effect) plot ---------------------------

tibia_vBMD_time_model_r2 <- round(
  r2(tibia_vBMD_model, tibia_vBMD_time_model), 2
)
make_tibia_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia vBMD \n change post-RYGB (%)",
    xlim = c(0, 90),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_vBMD_time_model, "time", 90, 0)
  # Plot p and R^2 values
  text(
    90, -1.5, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == .97)
  )
}

# Combine and save plots --------------------------------------------------

p1 <- as.ggplot(~ make_TH_regplot())
p2 <- as.ggplot(~ make_LS_regplot())
p3 <- as.ggplot(~ make_radius_regplot())
p4 <- as.ggplot(~ make_tibia_regplot())
agg_png(
  here("figures", "regplots.png"),
  width = 23,
  height = 20,
  units = "cm",
  res = 300
)
plot_grid(p1, p2, p3, p4, ncol = 2)
dev.off()
