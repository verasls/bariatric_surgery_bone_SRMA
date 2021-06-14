# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(metafor)
library(ragg)
library(ggplotify)
library(cowplot)
source(here("code", "funs.R"))

# Read data ---------------------------------------------------------------

load(here("output", "ma_tibia_objects.rda"))
load(here("output", "meta_regression_tibia_objects.rda"))

# Obs ---------------------------------------------------------------------

# The code for generating each regression plot was wrapped in a function in
# order to allow it to be transformed into a ggplot object with {ggplotify}.
# This transformation was done to achieve a better control in the plot grid
# using {cowplot}.

# Tibia cortical vBMD meta-regression (time effect) plot ------------------

tibia_CT_vBMD_time_model_r2 <- round(
  r2(tibia_CT_vBMD_model, tibia_CT_vBMD_time_model), 2
)
make_tibia_CT_vBMD_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_CT_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia cortical vBMD \n change post-RYGB (%)",
    xlim = c(0, 70),
    ylim = c(-10, 2),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 70, 10))
  # Plot regression equation text
  reg_equation(tibia_CT_vBMD_time_model, "time", 70, 1.6)
  # Plot p and R^2 values
  text(
    70, 0.7, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.84")
  )
}

# Tibia cortical porosity meta-regression (time effect) plot --------------

tibia_CT_porosity_time_model_r2 <- round(
  r2(tibia_CT_porosity_model, tibia_CT_porosity_time_model), 2
)
make_tibia_CT_porosity_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_CT_porosity_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia cortical porosity \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-5, 110),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_CT_porosity_time_model, "time", 90, 100)
  # Plot p and R^2 values
  text(
    90, 93, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) == .522 * ";" ~ R^2 == "0.00")
  )
}

# Tibia cortical thickness meta-regression (time effect) plot -------------

tibia_CT_thickness_time_model_r2 <- round(
  r2(tibia_CT_thickness_model, tibia_CT_thickness_time_model), 2
)
make_tibia_CT_thickness_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_CT_thickness_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia cortical thickness \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-15, 3),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_CT_thickness_time_model, "time", 90, 2.8)
  # Plot p and R^2 values
  text(
    90, 1.5, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.27")
  )
}

# Tibia trabecular vBMD meta-regression (time effect) plot ----------------

tibia_TB_vBMD_time_model_r2 <- round(
  r2(tibia_TB_vBMD_model, tibia_TB_vBMD_time_model), 2
)
make_tibia_TB_vBMD_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_TB_vBMD_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia trabecular vBMD \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-23, 0),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_TB_vBMD_time_model, "time", 90, -1)
  # Plot p and R^2 values
  text(
    90, -2.8, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.95")
  )
}

# Tibia trabecular number meta-regression (time effect) plot --------------

tibia_TB_number_time_model_r2 <- round(
  r2(tibia_TB_number_model, tibia_TB_number_time_model), 2
)
make_tibia_TB_number_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_TB_number_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia trabecular number \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-15, 4),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_TB_number_time_model, "time", 90, 3)
  # Plot p and R^2 values
  text(
    90, 1.4, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.21")
  )
}

# Tibia trabecular separation meta-regression (time effect) plot ----------

tibia_TB_separation_time_model_r2 <- round(
  r2(tibia_TB_separation_model, tibia_TB_separation_time_model), 2
)
make_tibia_TB_separation_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_TB_separation_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia trabecular separation \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-5, 25),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_TB_separation_time_model, "time", 0, 24, pos = 4)
  # Plot p and R^2 values
  text(
    0, 21.5, font = 1, pos = 4, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.00")
  )
}

# Tibia trabecular thickness meta-regression (time effect) plot -----------

tibia_TB_thickness_time_model_r2 <- round(
  r2(tibia_TB_thickness_model, tibia_TB_thickness_time_model), 2
)
make_tibia_TB_thickness_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_TB_thickness_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia trabecular thickness \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-15, 7),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_TB_thickness_time_model, "time", 90, 6.4)
  # Plot p and R^2 values
  text(
    90, 4.9, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) == .002 * ";" ~ R^2 == "0.55")
  )
}

# Tibia BVTV meta-regression (time effect) plot ---------------------------

tibia_BVTV_time_model_r2 <- round(
  r2(tibia_BVTV_model, tibia_BVTV_time_model), 2
)
make_tibia_BVTV_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_BVTV_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia BVTV \n change post-RYGB (%)",
    xlim = c(0, 13),
    ylim = c(-4, 4),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 12, 2))
  # Plot regression equation text
  reg_equation(tibia_BVTV_time_model, "time", 12, 3.5)
  # Plot p and R^2 values
  text(
    12, 2.8, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) == .396 * ";" ~ R^2 == "0.00")
  )
}

# Tibia failure load meta-regression (time effect) plot -------------------

tibia_failure_load_time_model_r2 <- round(
  r2(tibia_failure_load_model, tibia_failure_load_time_model), 2
)
make_tibia_failure_load_regplot <- function() {
  par(mar = c(2, 3, 1, 1))
  regplot(
    tibia_failure_load_time_model,
    shade = "gray95",
    xlab = "Time after surgery (months)",
    ylab = "Mean of tibia failure load \n change post-RYGB (%)",
    xlim = c(0, 90),
    ylim = c(-25, 15),
    bty = "l"
  )
  axis(side = 1, at = seq(0, 90, 10))
  # Plot regression equation text
  reg_equation(tibia_failure_load_time_model, "time", 90, 12)
  # Plot p and R^2 values
  text(
    90, 9, font = 1, pos = 2, cex = 0.9,
    bquote(italic(p) < .001 * ";" ~ R^2 == "0.00")
  )
}

# Combine and save plots --------------------------------------------------

p1 <- as.ggplot(~ make_tibia_CT_vBMD_regplot())
p2 <- as.ggplot(~ make_tibia_CT_porosity_regplot())
p3 <- as.ggplot(~ make_tibia_CT_thickness_regplot())
p4 <- as.ggplot(~ make_tibia_TB_vBMD_regplot())
p5 <- as.ggplot(~ make_tibia_TB_number_regplot())
p6 <- as.ggplot(~ make_tibia_TB_separation_regplot())
p7 <- as.ggplot(~ make_tibia_TB_thickness_regplot())
p8 <- as.ggplot(~ make_tibia_BVTV_regplot())
p9 <- as.ggplot(~ make_tibia_failure_load_regplot())
agg_png(
  here("figures", "regplots_tibia_bone_quality.png"),
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
