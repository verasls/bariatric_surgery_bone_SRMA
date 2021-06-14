# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(metafor)
library(ragg)
library(ggplotify)
library(cowplot)
source(here("code", "funs.R"))

# Read data ---------------------------------------------------------------

load(here("output", "ma_radius_objects.rda"))
load(here("output", "meta_regression_radius_objects.rda"))

# Obs ---------------------------------------------------------------------

# The code for generating each regression plot was wrapped in a function in
# order to allow it to be transformed into a ggplot object with {ggplotify}.
# This transformation was done to achieve a better control in the plot grid
# using {cowplot}.

# Radius cortical vBMD meta-regression (time effect) plot -----------------

radius_CT_vBMD_time_model_r2 <- round(
  r2(radius_CT_vBMD_model, radius_CT_vBMD_time_model), 2
)
make_radius_CT_vBMD_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_CT_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius cortical vBMD \n change post-RYGB (%)",
    xlim = c(0, 70),
    ylim = c(-10, 2),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 70, 10))
  # Plot regression equation text
  reg_equation(radius_CT_vBMD_time_model, "time", 70, 1.6)
  # Plot p and R^2 values
  text(
    70, 0.7, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.64")
  )
}

# Radius cortical porosity meta-regression (time effect) plot -------------

radius_CT_porosity_time_model_r2 <- round(
  r2(radius_CT_porosity_model, radius_CT_porosity_time_model), 2
)
make_radius_CT_porosity_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_CT_porosity_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius cortical porosity \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-5, 270),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_CT_porosity_time_model, "time", 90, 252)
  # Plot p and R^2 values
  text(
    90, 233, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.00")
  )
}

# Radius cortical thickness meta-regression (time effect) plot ------------

radius_CT_thickness_time_model_r2 <- round(
  r2(radius_CT_thickness_model, radius_CT_thickness_time_model), 2
)
make_radius_CT_thickness_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_CT_thickness_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius cortical thickness \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-20, 5),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_CT_thickness_time_model, "time", 90, 4.5)
  # Plot p and R^2 values
  text(
    90, 3, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.00")
  )
}

# Radius trabecular vBMD meta-regression (time effect) plot ---------------

radius_TB_vBMD_time_model_r2 <- round(
  r2(radius_TB_vBMD_model, radius_TB_vBMD_time_model), 2
)
make_radius_TB_vBMD_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_TB_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius trabecular vBMD \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-40, 0),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_TB_vBMD_time_model, "time", 90, 0)
  # Plot p and R^2 values
  text(
    90, -2.8, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.99")
  )
}

# Radius trabecular number meta-regression (time effect) plot -------------

radius_TB_number_time_model_r2 <- round(
  r2(radius_TB_number_model, radius_TB_number_time_model), 2
)
make_radius_TB_number_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_TB_number_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius trabecular number \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-25, 0),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_TB_number_time_model, "time", 90, 0)
  # Plot p and R^2 values
  text(
    90, -2, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.74")
  )
}

# Radius trabecular separation meta-regression (time effect) plot ---------

radius_TB_separation_time_model_r2 <- round(
  r2(radius_TB_separation_model, radius_TB_separation_time_model), 2
)
make_radius_TB_separation_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_TB_separation_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius trabecular separation \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-5, 40),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_TB_separation_time_model, "time", 0, 40, pos = 4)
  # Plot p and R^2 values
  text(
    0, 36, font = 1, pos = 4, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.66")
  )
}

# Radius trabecular thickness meta-regression (time effect) plot ----------

radius_TB_thickness_time_model_r2 <- round(
  r2(radius_TB_thickness_model, radius_TB_thickness_time_model), 2
)
make_radius_TB_thickness_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_TB_thickness_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius trabecular thickness \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-15, 7),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_TB_thickness_time_model, "time", 90, 6.4)
  # Plot p and R^2 values
  text(
    90, 4.9, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.79")
  )
}

# Radius BVTV meta-regression (time effect) plot --------------------------

radius_BVTV_time_model_r2 <- round(
  r2(radius_BVTV_model, radius_BVTV_time_model), 2
)
make_radius_BVTV_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_BVTV_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius BVTV \n change post-RYGB (%)",
    xlim = c(0, 13),
    ylim = c(-15, 20),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 12, 2))
  # Plot regression equation text
  reg_equation(radius_BVTV_time_model, "time", 12, 20)
  # Plot p and R^2 values
  text(
    12, 17.5, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) == .955 * ";" ~ R^2 == "0.55")
  )
}

# Radius failure load meta-regression (time effect) plot ------------------

radius_failure_load_time_model_r2 <- round(
  r2(radius_failure_load_model, radius_failure_load_time_model), 2
)
make_radius_failure_load_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    radius_failure_load_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of radius failure load \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-25, 15),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(radius_failure_load_time_model, "time", 90, 11)
  # Plot p and R^2 values
  text(
    90, 8, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.00")
  )
}

# Combine and save plots --------------------------------------------------

p1 <- as.ggplot(~ make_radius_CT_vBMD_regplot())
p2 <- as.ggplot(~ make_radius_CT_porosity_regplot())
p3 <- as.ggplot(~ make_radius_CT_thickness_regplot())
p4 <- as.ggplot(~ make_radius_TB_vBMD_regplot())
p5 <- as.ggplot(~ make_radius_TB_number_regplot())
p6 <- as.ggplot(~ make_radius_TB_separation_regplot())
p7 <- as.ggplot(~ make_radius_TB_thickness_regplot())
p8 <- as.ggplot(~ make_radius_BVTV_regplot())
p9 <- as.ggplot(~ make_radius_failure_load_regplot())
agg_png(
  here("figures", "regplots_radius_bone_quality.png"),
  width = 35,
  height = 30,
  units = "cm",
  res = 300
)
plot_grid(
  p1, p2, p3, p4, p5, p6, p7, p8, p9,
  ncol = 3
)
dev.off()
