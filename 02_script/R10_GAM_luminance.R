# ==========================================================
# 20_GAM_luminance.R
# Fit GAM for dorsoventral luminance profile
# ==========================================================

# ----------------------------------------------------------
# Load processed data
# ----------------------------------------------------------

profile_line <- readRDS(
  "01_data/processed/profile_line.rds"
)

# ----------------------------------------------------------
# Fit GAM
# ----------------------------------------------------------

model_lumi <- bam(
  luminance ~
    species +
    s(dv_pos, by = species, k = 10) +
    ti(dv_pos, ac_ratio, by = species, k = c(10, 5)) +
    s(image_name, bs = "re") +
    s(site, bs = "re"),
  data = profile_line,
  method = "fREML",
  discrete = TRUE
)

# ----------------------------------------------------------
# Model summary
# ----------------------------------------------------------

summary(model_lumi)

# ----------------------------------------------------------
# Save model
# ----------------------------------------------------------

saveRDS(
  model_lumi,
  "12_lateral_data_processed/models/model_luminance.rds"
)