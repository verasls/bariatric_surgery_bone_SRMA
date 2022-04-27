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

# Obs ---------------------------------------------------------------------

# The code for generating each forest plot was wrapped in a function in order
# to allow it to be transformed into a ggplot object with {ggplotify}. This
# transformation was done to achieve a better control in the plot grid using
# {cowplot}.

# Total hip vBMD forest plot -------------------------------------------

make_TH_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    TH_vBMD_model,
    slab = TH_vBMD$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 59),
    top = 4
  )
  # X axis label
  text(11, -5.4, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 7.2, font = 2, pos = 4, cex = 1,
    "Total hip volumetric bone mineral density"
  )
  # Follow-up time column
  text(-26, 6, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(TH_vBMD, -27)
  # Sample size column
  text(-22, 6, font = 2, pos = 2, cex = 1, "n")
  n_text(TH_vBMD, -22)
  # Weight column
  text(34, 6, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(TH_vBMD_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(i2(TH_vBMD, TH_vBMD_model), digits = 0, format = "f")
          ), "%, ",
        italic(.("p")), " = ",
        .(formatC(TH_vBMD_model$QEp, digits = 3, format = "f"))
      )
    )
  )
  # Overall effect text
  text(
    -69.9, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(TH_vBMD_model$zval, digits = 2, format = "f")), ", ",
        italic(.("p")), " = ",
        .(formatC(TH_vBMD_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Lumbar spine vBMD forest plot -------------------------------------------

make_LS_forestplot <- function() {
  par(mar = c(2.5, 2, 0.5, 2))
  forest(
    LS_vBMD_model,
    slab = LS_vBMD$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 59),
    top = 4
  )
  # X axis label
  text(15, -5.2, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 13.2, font = 2, pos = 4, cex = 1,
    "Lumbar spine volumetric bone mineral density"
  )
  # Follow-up time column
  text(-26, 12, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(LS_vBMD, -27)
  # Sample size column
  text(-22, 12, font = 2, pos = 2, cex = 1, "n")
  n_text(LS_vBMD, -22)
  # Weight column
  text(34, 12, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(LS_vBMD_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(i2(LS_vBMD, LS_vBMD_model), digits = 0, format = "f")
          ), "%, ",
        italic(.("p")), " < 0.001"
      )
    )
  )
  # Overall effect text
  text(
    -69.9, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(LS_vBMD_model$zval, digits = 2, format = "f")), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Radius vBMD forest plot -------------------------------------------------

make_radius_forestplot <- function() {
  par(mar = c(2, 2, 6.5, 2))
  forest(
    radius_vBMD_model,
    slab = radius_vBMD$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-65, 30),
    top = 0.5
  )
  # X axis label
  text(2, -5.2, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -65.5, 14.5, font = 2, pos = 4, cex = 1,
    "Radius volumetric bone mineral density"
  )
  # Follow-up time column
  text(-31, 13, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_vBMD, -32)
  # Sample size column
  text(-27, 13, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_vBMD, -27)
  # Weight column
  text(14, 13, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_vBMD_model, 14)
  text(14, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -64.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(i2(radius_vBMD, radius_vBMD_model), digits = 0, format = "f")
          ), "%, ",
        italic(.("p")), " < 0.001"
      )
    )
  )
  # Overall effect text
  text(
    -64.9, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(radius_vBMD_model$zval, digits = 2, format = "f")), ", ",
        italic(.("p")), " = ",
        .(formatC(radius_vBMD_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia vBMD forest plot --------------------------------------------------

make_tibia_forestplot <- function() {
  par(mar = c(2.5, 2, 7, 2))
  forest(
    tibia_vBMD_model,
    slab = tibia_vBMD$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-65, 30),
    top = 0.5
  )
  # X axis label
  text(4, -5.2, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -65.5, 13.5, font = 2, pos = 4, cex = 1,
    "Tibia volumetric bone mineral density"
  )
  # Follow-up time column
  text(-31, 12, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_vBMD, -32)
  # Sample size column
  text(-27, 12, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_vBMD, -27)
  # Weight column
  text(14, 12, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_vBMD_model, 14)
  text(14, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -64.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(i2(tibia_vBMD, tibia_vBMD_model), digits = 0, format = "f")
          ), "%, ",
        italic(.("p")), " < 0.001"
      )
    )
  )
  # Overall effect text
  text(
    -64.9, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(tibia_vBMD_model$zval, digits = 2, format = "f")), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Combine and save plots --------------------------------------------------

p1 <- as.ggplot(~ make_TH_forestplot())
p2 <- as.ggplot(~ make_LS_forestplot())
p3 <- as.ggplot(~ make_radius_forestplot())
p4 <- as.ggplot(~ make_tibia_forestplot())
agg_tiff(
  here("figures", "fig2.tiff"),
  width = 25,
  height = 30,
  units = "cm",
  res = 300
)
plot_grid(p1, p2, p3, p4, ncol = 1, rel_heights = c(0.6, 0.95, 1, 1))
dev.off()
