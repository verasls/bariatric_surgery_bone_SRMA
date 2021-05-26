# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
library(lvmisc)
library(metafor)
library(ragg)

# Read and tidy data ------------------------------------------------------

load(here("data", "data_percentage_change.rda"))

R_vBMD <- data_percentage_change %>%
  filter(outcome == "radius_vBMD") %>%
  filter(time_after_surgery %in% c(12, 24, 48, 9))
R_vBMD <- R_vBMD[c(1:7, 9), ]

# Meta-analysis -----------------------------------------------------------

# Calculate effect size
R_vBMD_e_ess <- escalc(
  measure = "MN",
  mi = mean_percent_change,
  sdi = sd_percent_change,
  ni = n,
  data = R_vBMD,
  slab = id
)

# Run model
R_vBMD_model <- rma(yi, vi, data = R_vBMD_es)

# Forest plot
agg_tiff(
  here("figures", "test.tiff"),
  width = 25,
  height = 15,
  units = "cm",
  res = 200
)
forest(
  R_vBMD_model,
  header = TRUE,
  showweights = TRUE,
  xlab = "Mean percentage change",
  mlab = "Random effects model"
)
text(18, 10, font = 2, cex = 1, "Weight (%)")
# Heterogeneity text
text(
  -45.3, -1.5, pos = 4, cex = 0.75,
  bquote(
    paste(
      "Heterogeneity: ",
      I^2, " = ",
      .(formatC(R_vBMD_model$I2, digits = 2, format = "f")), "%, ",
      tau^2, " = ",
      .(formatC(R_vBMD_model$tau2, digits = 2, format = "f")), ", ",
      italic(.("p")), " < 0.001"
    )
  )
)
# Overall effect text
text(
  -45.3, -1.9, pos = 4, cex = 0.75,
  bquote(
    paste(
      "Test for overall effect: ",
      "z = ",
      .(formatC(R_vBMD_model$zval, digits = 2, format = "f")), ", ",
      italic(.("p")), " = ",
      .(formatC(R_vBMD_model$pval, digits = 3, format = "f"))
    )
  )
)
dev.off()
