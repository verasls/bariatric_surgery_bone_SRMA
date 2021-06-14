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

# Obs ---------------------------------------------------------------------

# The code for generating each forest plot was wrapped in a function in order
# to allow it to be transformed into a ggplot object with {ggplotify}. This
# transformation was done to achieve a better control in the plot grid using
# {cowplot}.

# Radius cortical vBMD forest plot -------------------------------------------

make_radius_CT_vBMD_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_CT_vBMD_model,
    slab = radius_CT_vBMD$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 59),
    top = 4
  )
  # X axis label
  text(15, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 17, font = 2, pos = 4, cex = 1,
    "Radius cortical volumetric bone mineral density"
  )
  # Follow-up time column
  text(-26, 16, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_CT_vBMD, -27)
  # Sample size column
  text(-22, 16, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_CT_vBMD, -22)
  # Weight column
  text(34, 16, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_CT_vBMD_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_CT_vBMD, radius_CT_vBMD_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
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
        .(formatC(radius_CT_vBMD_model$zval, digits = 2, format = "f")), ", ",
        italic(.("p")), " = ",
        .(formatC(radius_CT_vBMD_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Radius cortical porosity ------------------------------------------------

make_radius_CT_porosity_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_CT_porosity_model,
    slab = radius_CT_porosity$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-1000, 1200),
    top = 4
  )
  # X axis label
  text(450, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -1010, 13, font = 2, pos = 4, cex = 1,
    "Radius cortical porosity"
  )
  # Follow-up time column
  text(-400, 12, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_CT_porosity, -410)
  # Sample size column
  text(-320, 12, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_CT_porosity, -320)
  # Weight column
  text(780, 12, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_CT_porosity_model, 780)
  text(780, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -995, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_CT_porosity, radius_CT_porosity_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
  # Overall effect text
  text(
    -995, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(
            radius_CT_porosity_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(radius_CT_porosity_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Radius cortical thickness -----------------------------------------------

make_radius_CT_thickness_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_CT_thickness_model,
    slab = radius_CT_thickness$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 59),
    top = 4
  )
  # X axis label
  text(8, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 15, font = 2, pos = 4, cex = 1,
    "Radius cortical thickness"
  )
  # Follow-up time column
  text(-26, 14, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_CT_thickness, -27)
  # Sample size column
  text(-22, 14, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_CT_thickness, -22)
  # Weight column
  text(34, 14, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_CT_thickness_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_CT_thickness, radius_CT_thickness_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
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
        .(formatC(
            radius_CT_thickness_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Radius trabecular volumetric bone mineral density -----------------------

make_radius_TB_vBMD_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_TB_vBMD_model,
    slab = radius_TB_vBMD$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-150, 100),
    top = 4
  )
  # X axis label
  text(20, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -152, 17, font = 2, pos = 4, cex = 1,
    "Radius trabecular volumetric bone mineral density"
  )
  # Follow-up time column
  text(-60, 16, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_TB_vBMD, -63)
  # Sample size column
  text(-53, 16, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_TB_vBMD, -53)
  # Weight column
  text(52, 16, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_TB_vBMD_model, 52)
  text(52, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -149, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_TB_vBMD, radius_TB_vBMD_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
  # Overall effect text
  text(
    -149, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(
            radius_TB_vBMD_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Radius trabecular number ------------------------------------------------

make_radius_TB_number_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_TB_number_model,
    slab = radius_TB_number$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-80, 59),
    top = 4
  )
  # X axis label
  text(7, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -81, 16, font = 2, pos = 4, cex = 1,
    "Radius trabecular number"
  )
  # Follow-up time column
  text(-36, 15, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_TB_number, -37)
  # Sample size column
  text(-31, 15, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_TB_number, -31)
  # Weight column
  text(34, 15, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_TB_number_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -79.5, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_TB_number, radius_TB_number_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
  # Overall effect text
  text(
    -79.5, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(
            radius_TB_number_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Radius trabecular separation --------------------------------------------

make_radius_TB_separation_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_TB_separation_model,
    slab = radius_TB_separation$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 90),
    top = 4
  )
  # X axis label
  text(40, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 13, font = 2, pos = 4, cex = 1,
    "Radius trabecular separation"
  )
  # Follow-up time column
  text(-23, 12, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_TB_separation, -24)
  # Sample size column
  text(-18, 12, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_TB_separation, -18)
  # Weight column
  text(63, 12, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_TB_separation_model, 63)
  text(63, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_TB_separation, radius_TB_separation_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
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
        .(formatC(
            radius_TB_separation_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Radius trabecular thickness --------------------------------------------

make_radius_TB_thickness_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_TB_thickness_model,
    slab = radius_TB_thickness$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 59),
    top = 4
  )
  # X axis label
  text(11, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 16, font = 2, pos = 4, cex = 1,
    "Radius trabecular thickness"
  )
  # Follow-up time column
  text(-26, 15, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_TB_thickness, -27)
  # Sample size column
  text(-22, 15, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_TB_thickness, -22)
  # Weight column
  text(34, 15, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_TB_thickness_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_TB_thickness, radius_TB_thickness_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
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
        .(formatC(
            radius_TB_thickness_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Radius trabecular bone volume fraction ----------------------------------

make_radius_BVTV_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_BVTV_model,
    slab = radius_BVTV$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 59),
    top = 4
  )
  # X axis label
  text(11, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 6, font = 2, pos = 4, cex = 1,
    "Radius trabecular bone volume fraction"
  )
  # Follow-up time column
  text(-26, 5, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_BVTV, -27)
  # Sample size column
  text(-22, 5, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_BVTV, -22)
  # Weight column
  text(34, 5, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_BVTV_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_BVTV, radius_BVTV_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
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
        .(formatC(
            radius_BVTV_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Radius trabecular failure load ------------------------------------------

make_radius_failure_load_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    radius_failure_load_model,
    slab = radius_failure_load$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-80, 59),
    top = 4
  )
  # X axis label
  text(11, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -80.5, 14, font = 2, pos = 4, cex = 1,
    "Radius trabecular failure load"
  )
  # Follow-up time column
  text(-36, 13, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(radius_failure_load, -37)
  # Sample size column
  text(-32, 13, font = 2, pos = 2, cex = 1, "n")
  n_text(radius_failure_load, -32)
  # Weight column
  text(34, 13, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(radius_failure_load_model, 34)
  text(34, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -79.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(radius_failure_load, radius_failure_load_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
  # Overall effect text
  text(
    -79.9, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(
            radius_failure_load_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Combine and save plots --------------------------------------------------

p1 <- as.ggplot(~ make_radius_CT_vBMD_forestplot())
p2 <- as.ggplot(~ make_radius_CT_porosity_forestplot())
p3 <- as.ggplot(~ make_radius_CT_thickness_forestplot())
p4 <- as.ggplot(~ make_radius_TB_vBMD_forestplot())
p5 <- as.ggplot(~ make_radius_TB_number_forestplot())
p6 <- as.ggplot(~ make_radius_TB_separation_forestplot())
p7 <- as.ggplot(~ make_radius_TB_thickness_forestplot())
p8 <- as.ggplot(~ make_radius_BVTV_forestplot())
p9 <- as.ggplot(~ make_radius_failure_load_forestplot())
agg_png(
  here("figures", "forest_plots_radius_bone_quality.png"),
  width = 25,
  height = 90,
  units = "cm",
  res = 300
)
plot_grid(
  p1, p2, p3, p4, p5, p6, p7, p8, p9,
  ncol = 1,
  rel_heights = c(1, 0.9, 0.9, 1, 1, 0.9, 1, 0.5, 1)
)
dev.off()
