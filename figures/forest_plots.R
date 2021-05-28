# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(metafor)
library(ragg)
source(here("code", "funs.R"))

# Read data ---------------------------------------------------------------

load(here("output", "ma_objects.rda"))

# Radius vBMD forest plot -------------------------------------------------

agg_tiff(
  here("figures", "forest_plot_radius_vBMD.tiff"),
  width = 25,
  height = 15,
  units = "cm",
  res = 200
)
forest(
  radius_vBMD_model,
  slab = radius_vBMD$study,
  header = TRUE,
  digits = 1,
  xlab = "Mean percentage change",
  mlab = "Random effects model",
  xlim = c(-65, 30),
  top = 4
)
# Plot title
text(
  -65.5, 17.5, font = 2, pos = 4, cex = 1,
  "Radius volumetric bone mineral density"
)
# Follow-up time column
text(-31, 16, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
followup_text(radius_vBMD, -32)
# Sample size column
text(-27, 16, font = 2, pos = 2, cex = 1, "n")
n_text(radius_vBMD, -27)
# Weight column
text(14, 16, font = 2, pos = 2, cex = 1, "Weight")
weights_text(radius_vBMD_model, 14)
text(14, -1, font = 1, pos = 2, cex = 1, "100%")
# Heterogeneity text
text(
  -64.9, -1.65, pos = 4, cex = 0.7,
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
  -64.9, -2.15, pos = 4, cex = 0.7,
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
dev.off()

# Tibia vBMD forest plot --------------------------------------------------

agg_tiff(
  here("figures", "forest_plot_tibia_vBMD.tiff"),
  width = 25,
  height = 15,
  units = "cm",
  res = 200
)
forest(
  tibia_vBMD_model,
  slab = tibia_vBMD$study,
  header = TRUE,
  digits = 1,
  xlab = "Mean percentage change",
  mlab = "Random effects model",
  xlim = c(-65, 30),
  top = 4
)
# Plot title
text(
  -65.5, 16.5, font = 2, pos = 4, cex = 1,
  "Tibia volumetric bone mineral density"
)
# Follow-up time column
text(-31, 15, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
followup_text(tibia_vBMD, -32)
# Sample size column
text(-27, 15, font = 2, pos = 2, cex = 1, "n")
n_text(tibia_vBMD, -27)
# Weight column
text(14, 15, font = 2, pos = 2, cex = 1, "Weight")
weights_text(tibia_vBMD_model, 14)
text(14, -1, font = 1, pos = 2, cex = 1, "100%")
# Heterogeneity text
text(
  -64.9, -1.65, pos = 4, cex = 0.7,
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
  -64.9, -2.15, pos = 4, cex = 0.7,
  bquote(
    paste(
      "Test for overall effect: ",
      "z = ",
      .(formatC(tibia_vBMD_model$zval, digits = 2, format = "f")), ", ",
      italic(.("p")), " < 0.001 "
    )
  )
)
dev.off()

# Lumbar spine vBMD forest plot -------------------------------------------

agg_tiff(
  here("figures", "forest_plot_LS_vBMD.tiff"),
  width = 25,
  height = 15,
  units = "cm",
  res = 200
)
forest(
  LS_vBMD_model,
  slab = LS_vBMD$study,
  header = TRUE,
  digits = 1,
  xlab = "Mean percentage change",
  mlab = "Random effects model",
  xlim = c(-70, 59),
  top = 4
)
# Plot title
text(
  -70.5, 13.2, font = 2, pos = 4, cex = 1,
  "Lumbar spine volumetric bone mineral density"
)
# Follow-up time column
text(-31, 12, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
followup_text(LS_vBMD, -32)
# Sample size column
text(-27, 12, font = 2, pos = 2, cex = 1, "n")
n_text(LS_vBMD, -27)
# Weight column
text(34, 12, font = 2, pos = 2, cex = 1, "Weight")
weights_text(LS_vBMD_model, 34)
text(34, -1, font = 1, pos = 2, cex = 1, "100%")
# Heterogeneity text
text(
  -69.9, -1.5, pos = 4, cex = 0.7,
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
  -69.9, -2.0, pos = 4, cex = 0.7,
  bquote(
    paste(
      "Test for overall effect: ",
      "z = ",
      .(formatC(LS_vBMD_model$zval, digits = 2, format = "f")), ", ",
      italic(.("p")), " < 0.001 "
    )
  )
)
dev.off()

# Total hip vBMD forest plot -------------------------------------------

agg_tiff(
  here("figures", "forest_plot_TH_vBMD.tiff"),
  width = 25,
  height = 12,
  units = "cm",
  res = 200
)
forest(
  TH_vBMD_model,
  slab = TH_vBMD$study,
  header = TRUE,
  digits = 1,
  xlab = "Mean percentage change",
  mlab = "Random effects model",
  xlim = c(-70, 59),
  top = 4
)
# Plot title
text(
  -70.5, 6, font = 2, pos = 4, cex = 1,
  "Total hip volumetric bone mineral density"
)
# Follow-up time column
text(-31, 5, font = 2, pos = 2, cex = 1, "Follow-up (mo.)")
followup_text(TH_vBMD, -32)
# Sample size column
text(-27, 5, font = 2, pos = 2, cex = 1, "n")
n_text(TH_vBMD, -27)
# Weight column
text(34, 5, font = 2, pos = 2, cex = 1, "Weight")
weights_text(TH_vBMD_model, 34)
text(34, -1, font = 1, pos = 2, cex = 1, "100%")
# Heterogeneity text
text(
  -69.9, -1.38, pos = 4, cex = 0.7,
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
  -69.9, -1.7, pos = 4, cex = 0.7,
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
dev.off()
