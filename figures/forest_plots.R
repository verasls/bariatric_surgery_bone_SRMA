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
  # showweights = TRUE,
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
text(12.5, 16, font = 2, pos = 2, cex = 1, "Weight")
weights_text(radius_vBMD_model, 12.5)
text(12.5, -1, font = 1, pos = 2, cex = 1, "100.00%")
# Heterogeneity text
text(
  -64.9, -1.65, pos = 4, cex = 0.7,
  bquote(
    paste(
      "Heterogeneity: ",
      I^2, " = ",
      .(
        formatC(i2(radius_vBMD, radius_vBMD_model), digits = 1, format = "f")
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
