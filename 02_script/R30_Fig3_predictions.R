# ==========================================================
# 30_Fig3_prediction.R
# Prediction data for Figure 3 (Dolly Varden)
# ==========================================================


# ----------------------------------------------------------
# Load data and models
# ----------------------------------------------------------

df <- readRDS(
  "01_data/processed/df_processed.rds"
)

DV_models <- readRDS(
  "13_DV_LMM_LM/models/DV_models.rds"
)

DV_LM_spawn <- DV_models$LM_spawn


# ----------------------------------------------------------
# Data preparation
# ----------------------------------------------------------

df <- df |>
  dplyr::mutate(
    fl_z = as.numeric(fl_z),
    width_z = as.numeric(width_z),
    resid_log_Charr = as.numeric(resid_log_Charr),
    sex = factor(sex),
    SPorUNSP = factor(SPorUNSP),
    ac_ratio = ac_area / fish_area
  )


# Data for LMM prediction

df_dv <- df |>
  dplyr::filter(
    species == "DV",
    site != "Oct"
  )


# Data for spawning prediction

df_spawn <- df |>
  dplyr::filter(
    species == "DV"
  )


# ----------------------------------------------------------
# Model for Figure 3 prediction
# ----------------------------------------------------------

DV_LMM_ac_ratio <- lme4::lmer(
  log(ac_ratio) ~
    fl_z * width_z +
    resid_log_Charr +
    sex +
    (1 | site),
  data = df_dv
)


# ==========================================================
# Prediction
# ==========================================================


# ----------------------------------------------------------
# Body size
# ----------------------------------------------------------

pred_fl <- ggeffects::ggpredict(
  DV_LMM_ac_ratio,
  terms = "fl_z [all]",
  condition = c(
    fish_area = 1,
    width_z = 0,
    resid_log_Charr = 0,
    sex = "Female"
  ),
  back_transform = FALSE
) |>
  as.data.frame()



# ----------------------------------------------------------
# River width
# ----------------------------------------------------------

width_vals <- quantile(
  df_dv$width_z,
  probs = c(0.1, 0.9),
  na.rm = TRUE
)


pred_width <- ggeffects::ggpredict(
  DV_LMM_ac_ratio,
  terms = c(
    "fl_z [all]",
    paste0(
      "width_z [",
      round(width_vals[1], 2),
      ",",
      round(width_vals[2], 2),
      "]"
    )
  ),
  condition = c(
    fish_area = 1,
    resid_log_Charr = 0,
    sex = "Female"
  ),
  back_transform = FALSE
) |>
  as.data.frame()


pred_width$width_val <-
  as.numeric(
    as.character(pred_width$group)
  )



# ----------------------------------------------------------
# Body condition
# ----------------------------------------------------------

condition_vals <- quantile(
  df_dv$resid_log_Charr,
  probs = c(0.1, 0.9),
  na.rm = TRUE
)


pred_condition <- ggeffects::ggpredict(
  DV_LMM_ac_ratio,
  terms = c(
    "fl_z [all]",
    paste0(
      "resid_log_Charr [",
      round(condition_vals[1], 2),
      ",",
      round(condition_vals[2], 2),
      "]"
    )
  ),
  condition = c(
    fish_area = 1,
    width_z = 0,
    sex = "Female"
  ),
  back_transform = FALSE
) |>
  as.data.frame()


pred_condition$condition_val <-
  as.numeric(
    as.character(pred_condition$group)
  )



# ----------------------------------------------------------
# Sex
# ----------------------------------------------------------

pred_sex <- ggeffects::ggpredict(
  DV_LMM_ac_ratio,
  terms = c(
    "fl_z [all]",
    "sex"
  ),
  condition = c(
    fish_area = 1,
    width_z = 0,
    resid_log_Charr = 0
  ),
  back_transform = FALSE
) |>
  as.data.frame() |>
  dplyr::filter(
    group != "Unidentified"
  )



# ----------------------------------------------------------
# Spawning status
# ----------------------------------------------------------
# ----------------------------------------------------------
# Data for spawning prediction
# ----------------------------------------------------------

df_spawn <- df |>
  dplyr::filter(
    species == "DV",
    !is.na(SPorUNSP)
  )



# ----------------------------------------------------------
# Spawning model (log(ac_ratio))
# ----------------------------------------------------------

DV_LM_spawn_ratio <- lm(
  log(ac_ratio) ~
    fl_z +
    resid_log_Charr +
    sex +
    SPorUNSP,
  data = df_spawn
)



# ----------------------------------------------------------
# Spawning status prediction
# ----------------------------------------------------------

pred_spawn <- ggeffects::ggpredict(
  DV_LM_spawn_ratio,
  terms = c(
    "fl_z [all]",
    "SPorUNSP"
  ),
  condition = c(
    resid_log_Charr = 0,
    sex = "Female"
  ),
  back_transform = FALSE
) |>
  as.data.frame() |>
  dplyr::filter(
    !is.na(group)
  )


# ==========================================================
# Save prediction data
# ==========================================================

Fig3_prediction <- list(

  # prediction
  pred_fl = pred_fl,
  pred_width = pred_width,
  pred_condition = pred_condition,
  pred_sex = pred_sex,
  pred_spawn = pred_spawn,

  # raw data
  df_dv = df_dv,
  df_spawn = df_spawn,

  # parameters
  width_vals = width_vals,
  condition_vals = condition_vals

)


dir.create(
  "results",
  showWarnings = FALSE
)


saveRDS(
  Fig3_prediction,
  "13_DV_LMM_LM/results/Fig3_prediction.rds"
)


message(
  "Figure 3 prediction data saved."
)