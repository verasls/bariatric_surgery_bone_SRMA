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

# Obs ---------------------------------------------------------------------

# The code for generating each forest plot was wrapped in a function in order
# to allow it to be transformed into a ggplot object with {ggplotify}. This
# transformation was done to achieve a better control in the plot grid using
# {cowplot}.

# Tibia cortical vBMD forest plot -------------------------------------------

make_tibia_CT_vBMD_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_CT_vBMD_model,
    slab = tibia_CT_vBMD$study,
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
    -70.5, 15, font = 2, pos = 4, cex = 1,
    "Tibia cortical volumetric bone mineral density"
  )
  # Follow-up time column
  text(-26, 14, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_CT_vBMD, -27)
  # Sample size column
  text(-22, 14, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_CT_vBMD, -22)
  # Weight column
  text(34, 14, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_CT_vBMD_model, 34)
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
            i2(tibia_CT_vBMD, tibia_CT_vBMD_model),
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
        .(formatC(tibia_CT_vBMD_model$zval, digits = 2, format = "f")), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_CT_vBMD_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia cortical porosity ------------------------------------------------

make_tibia_CT_porosity_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_CT_porosity_model,
    slab = tibia_CT_porosity$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-1000, 1200),
    top = 4
  )
  # X axis label
  text(320, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -1010, 12, font = 2, pos = 4, cex = 1,
    "Tibia cortical porosity"
  )
  # Follow-up time column
  text(-330, 11, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_CT_porosity, -340)
  # Sample size column
  text(-270, 11, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_CT_porosity, -270)
  # Weight column
  text(780, 11, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_CT_porosity_model, 780)
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
            i2(tibia_CT_porosity, tibia_CT_porosity_model),
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
            tibia_CT_porosity_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_CT_porosity_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia cortical thickness -----------------------------------------------

make_tibia_CT_thickness_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_CT_thickness_model,
    slab = tibia_CT_thickness$study,
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
    -70.5, 14, font = 2, pos = 4, cex = 1,
    "Tibia cortical thickness"
  )
  # Follow-up time column
  text(-26, 13, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_CT_thickness, -27)
  # Sample size column
  text(-22, 13, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_CT_thickness, -22)
  # Weight column
  text(39, 13, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_CT_thickness_model, 39)
  text(39, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -69.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(tibia_CT_thickness, tibia_CT_thickness_model),
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
            tibia_CT_thickness_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " < 0.001 "
      )
    )
  )
}

# Tibia trabecular volumetric bone mineral density -----------------------

make_tibia_TB_vBMD_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_TB_vBMD_model,
    slab = tibia_TB_vBMD$study,
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
    -152, 16, font = 2, pos = 4, cex = 1,
    "Tibia trabecular volumetric bone mineral density"
  )
  # Follow-up time column
  text(-60, 15, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_TB_vBMD, -63)
  # Sample size column
  text(-53, 15, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_TB_vBMD, -53)
  # Weight column
  text(52, 15, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_TB_vBMD_model, 52)
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
            i2(tibia_TB_vBMD, tibia_TB_vBMD_model),
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
            tibia_TB_vBMD_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_TB_vBMD_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia trabecular number ------------------------------------------------

make_tibia_TB_number_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_TB_number_model,
    slab = tibia_TB_number$study,
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
    -81, 15, font = 2, pos = 4, cex = 1,
    "Tibia trabecular number"
  )
  # Follow-up time column
  text(-36, 14, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_TB_number, -37)
  # Sample size column
  text(-31, 14, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_TB_number, -31)
  # Weight column
  text(34, 14, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_TB_number_model, 34)
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
            i2(tibia_TB_number, tibia_TB_number_model),
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
            tibia_TB_number_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_TB_number_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia trabecular separation --------------------------------------------

make_tibia_TB_separation_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_TB_separation_model,
    slab = tibia_TB_separation$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 90),
    top = 4
  )
  # X axis label
  text(30, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 12, font = 2, pos = 4, cex = 1,
    "Tibia trabecular separation"
  )
  # Follow-up time column
  text(-23, 11, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_TB_separation, -24)
  # Sample size column
  text(-18, 11, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_TB_separation, -18)
  # Weight column
  text(63, 11, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_TB_separation_model, 63)
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
            i2(tibia_TB_separation, tibia_TB_separation_model),
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
            tibia_TB_separation_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_TB_separation_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia trabecular thickness --------------------------------------------

make_tibia_TB_thickness_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_TB_thickness_model,
    slab = tibia_TB_thickness$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-70, 59),
    top = 4
  )
  # X axis label
  text(13, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -70.5, 15, font = 2, pos = 4, cex = 1,
    "Tibia trabecular thickness"
  )
  # Follow-up time column
  text(-26, 14, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_TB_thickness, -27)
  # Sample size column
  text(-22, 14, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_TB_thickness, -22)
  # Weight column
  text(34, 14, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_TB_thickness_model, 34)
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
            i2(tibia_TB_thickness, tibia_TB_thickness_model),
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
            tibia_TB_thickness_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_TB_thickness_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia trabecular bone volume fraction ----------------------------------

make_tibia_BVTV_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_BVTV_model,
    slab = tibia_BVTV$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-15, 10),
    top = 4
  )
  # X axis label
  text(2, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -15.1, 5, font = 2, pos = 4, cex = 1,
    "Tibia trabecular bone volume fraction"
  )
  # Follow-up time column
  text(-6, 4, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_BVTV, -6.5)
  # Sample size column
  text(-5, 4, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_BVTV, -5)
  # Weight column
  text(6, 4, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_BVTV_model, 6)
  text(6, -1, font = 1, pos = 2, cex = 1, "100.0%")
  # Heterogeneity text
  text(
    -14.9, -2.1, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Heterogeneity: ",
        I^2, " = ",
        .(
          formatC(
            i2(tibia_BVTV, tibia_BVTV_model),
            digits = 0, format = "f"
          )
          ), "%, ",
        italic(.("p")), " = ",
        .(formatC(tibia_BVTV_model$QEp, digits = 3, format = "f"))
      )
    )
  )
  # Overall effect text
  text(
    -14.9, -3.0, pos = 4, cex = 0.85,
    bquote(
      paste(
        "Test for overall effect: ",
        "z = ",
        .(formatC(
            tibia_BVTV_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_BVTV_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Tibia failure load ------------------------------------------------------

make_tibia_failure_load_forestplot <- function() {
  par(mar = c(3.5, 2, 1, 2))
  forest(
    tibia_failure_load_model,
    slab = tibia_failure_load$study,
    header = TRUE,
    digits = 1,
    xlab = "",
    mlab = "Random effects model",
    xlim = c(-80, 59),
    top = 4
  )
  # X axis label
  text(15, -5, font = 1, pos = 2, cex = 1, "Mean percentage change")
  # Plot title
  text(
    -80.5, 13, font = 2, pos = 4, cex = 1,
    "Tibia failure load"
  )
  # Follow-up time column
  text(-36, 12, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
  followup_text(tibia_failure_load, -37)
  # Sample size column
  text(-32, 12, font = 2, pos = 2, cex = 1, "n")
  n_text(tibia_failure_load, -32)
  # Weight column
  text(34, 12, font = 2, pos = 2, cex = 1, "Weight")
  weights_text(tibia_failure_load_model, 34)
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
            i2(tibia_failure_load, tibia_failure_load_model),
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
            tibia_failure_load_model$zval,
            digits = 2, format = "f"
          )), ", ",
        italic(.("p")), " = ",
        .(formatC(tibia_failure_load_model$pval, digits = 3, format = "f"))
      )
    )
  )
}

# Combine and save plots --------------------------------------------------

p1 <- as.ggplot(~ make_tibia_CT_vBMD_forestplot())
p2 <- as.ggplot(~ make_tibia_CT_porosity_forestplot())
p3 <- as.ggplot(~ make_tibia_CT_thickness_forestplot())
p4 <- as.ggplot(~ make_tibia_TB_vBMD_forestplot())
p5 <- as.ggplot(~ make_tibia_TB_number_forestplot())
p6 <- as.ggplot(~ make_tibia_TB_separation_forestplot())
p7 <- as.ggplot(~ make_tibia_TB_thickness_forestplot())
p8 <- as.ggplot(~ make_tibia_BVTV_forestplot())
p9 <- as.ggplot(~ make_tibia_failure_load_forestplot())
agg_tiff(
  here("figures", "fig5.tiff"),
  width = 25,
  height = 90,
  units = "cm",
  res = 300
)
plot_grid(
  p1, p2, p3, p4, p5, p6, p7, p8, p9,
  ncol = 1,
  rel_heights = c(1, 0.9, 0.9, 1, 1, 0.9, 1, 0.4, 1)
)
dev.off()
