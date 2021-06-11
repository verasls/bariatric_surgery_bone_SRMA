# Load packages and functions ---------------------------------------------

library(here)
library(tidyverse)
source(here("code", "funs.R"))

# Read data ---------------------------------------------------------------

load(here("output", "ma_radius_objects.rda"))
load(here("output", "ma_tibia_objects.rda"))

# Build table -------------------------------------------------------------

variables <- c(
  "Cortical volumetric bone mineral density",
  "Cortical porosity",
  "Cortical thickness",
  "Trabecular volumetric bone mineral density",
  "Trabecular number",
  "Trabecular separation",
  "Trabecular thickness",
  "Trabecular bone volume fraction",
  "Failure load"
)
radius_models <- list(
  radius_CT_vBMD_model,
  radius_CT_porosity_model,
  radius_CT_thickness_model,
  radius_TB_vBMD_model,
  radius_TB_number_model,
  radius_TB_separation_model,
  radius_TB_thickness_model,
  radius_BVTV_model,
  radius_failure_load_model
)
tibia_models <- list(
  tibia_CT_vBMD_model,
  tibia_CT_porosity_model,
  tibia_CT_thickness_model,
  tibia_TB_vBMD_model,
  tibia_TB_number_model,
  tibia_TB_separation_model,
  tibia_TB_thickness_model,
  tibia_BVTV_model,
  tibia_failure_load_model
)
radius_values <- map_chr(radius_models, get_model_estimate_tb)
tibia_values <- map_chr(tibia_models, get_model_estimate_tb)

bone_quality_table <- data.frame(
  Variables = variables,
  Radius = radius_values,
  Tibia = tibia_values
)

# Save as csv -------------------------------------------------------------

write_csv(bone_quality_table, here("tables", "bone_quality_table.csv"))
