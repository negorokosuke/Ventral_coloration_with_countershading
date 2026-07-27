# ==========================================================
# 32_Fig4_predictions.R
# Prediction data for Figure 4 (White-spotted charr)
#
# Statistical model:
# log(ac_area) ~ predictors + offset(log(fish_area))
#
# Visualization:
# log(ac_area / fish_area)
# ==========================================================


# ----------------------------------------------------------
# Load data and models
# ----------------------------------------------------------

df <- readRDS(
  "01_data/processed/df_processed.rds"
)

WSC_models <- readRDS(
  "14_WSC_GAMM_GAM/models/WSC_models.rds"
)

WSC_GAMM <- WSC_models$GAMM

WSC_GAM_spawn <- WSC_models$GAM_spawn



# ----------------------------------------------------------
# Data for plotting
# ----------------------------------------------------------

df_wsc <- df |>
  dplyr::filter(
    species == "WSC",
    site != "Oct"
  ) |>
  dplyr::mutate(
    ac_ratio = ac_area / fish_area,
    log_ac_ratio = log(ac_ratio)
  )


df_spawn <- df |>
  dplyr::filter(
    species == "WSC",
    !is.na(SPorUNSP)
  ) |>
  dplyr::mutate(
    ac_ratio = ac_area / fish_area,
    log_ac_ratio = log(ac_ratio)
  )


site_ref <- levels(df_wsc$site)[1]



# ----------------------------------------------------------
# Prediction grid
# ----------------------------------------------------------

fl_seq <- seq(
  min(df_wsc$fl_z, na.rm = TRUE),
  max(df_wsc$fl_z, na.rm = TRUE),
  length.out = 200
)


width_vals <- quantile(
  df_wsc$width_z,
  c(0.10, 0.90),
  na.rm = TRUE
)


condition_vals <- quantile(
  df_wsc$resid_log_Charr,
  c(0.10, 0.90),
  na.rm = TRUE
)



# ==========================================================
# Body size
# ==========================================================

new_fl <- data.frame(
  fl_z = fl_seq,
  width_z = 0,
  resid_log_Charr = 0,
  sex = "Female",
  fish_area = 1,
  site = factor(
    site_ref,
    levels = levels(df_wsc$site)
  )
)


pred <- predict(
  WSC_GAMM,
  newdata = new_fl,
  se.fit = TRUE,
  exclude = "s(site)"
)


pred_fl <- new_fl |>
  mutate(

    x = fl_z,

    # log(ac_ratio)
    log_ac_ratio = pred$fit,

    conf.low =
      pred$fit - 1.96 * pred$se.fit,

    conf.high =
      pred$fit + 1.96 * pred$se.fit

  )



# ==========================================================
# River width
# ==========================================================

new_width <- expand.grid(
  fl_z = fl_seq,
  width_z = width_vals,
  resid_log_Charr = 0,
  sex = "Female",
  fish_area = 1,
  site = factor(
    site_ref,
    levels = levels(df_wsc$site)
  )
)


pred <- predict(
  WSC_GAMM,
  newdata = new_width,
  se.fit = TRUE,
  exclude = "s(site)"
)


pred_width <- new_width |>
  mutate(

    x = fl_z,

    log_ac_ratio = pred$fit,

    conf.low =
      pred$fit - 1.96 * pred$se.fit,

    conf.high =
      pred$fit + 1.96 * pred$se.fit,

    width_val = width_z,

    group = factor(width_z)

  )



# ==========================================================
# Body condition
# ==========================================================

new_condition <- expand.grid(
  fl_z = fl_seq,
  width_z = 0,
  resid_log_Charr = condition_vals,
  sex = "Female",
  fish_area = 1,
  site = factor(
    site_ref,
    levels = levels(df_wsc$site)
  )
)


pred <- predict(
  WSC_GAMM,
  newdata = new_condition,
  se.fit = TRUE,
  exclude = "s(site)"
)


pred_condition <- new_condition |>
  mutate(

    x = fl_z,

    log_ac_ratio = pred$fit,

    conf.low =
      pred$fit - 1.96 * pred$se.fit,

    conf.high =
      pred$fit + 1.96 * pred$se.fit,

    condition_val = resid_log_Charr,

    group = factor(resid_log_Charr)

  )



# ==========================================================
# Sex
# ==========================================================

new_sex <- expand.grid(
  fl_z = fl_seq,
  width_z = 0,
  resid_log_Charr = 0,
  sex = c("Female", "Male"),
  fish_area = 1,
  site = factor(
    site_ref,
    levels = levels(df_wsc$site)
  )
)


pred <- predict(
  WSC_GAMM,
  newdata = new_sex,
  se.fit = TRUE,
  exclude = "s(site)"
)


pred_sex <- new_sex |>
  mutate(

    x = fl_z,

    log_ac_ratio = pred$fit,

    conf.low =
      pred$fit - 1.96 * pred$se.fit,

    conf.high =
      pred$fit + 1.96 * pred$se.fit,

    group = sex

  )



# ==========================================================
# Spawning season
# ==========================================================

pred_spawn <- ggpredict(
  WSC_GAM_spawn,
  terms = c(
    "fl_z [all]",
    "SPorUNSP"
  ),
  condition = c(
    resid_log_Charr = 0,
    sex = "Female",
    fish_area = 1
  ),
  back_transform = FALSE
) |>
  as.data.frame() |>
  dplyr::filter(
    !is.na(group)
  ) |>
  dplyr::mutate(

    log_ac_ratio = predicted,

    conf.low = conf.low,

    conf.high = conf.high

  )



# ----------------------------------------------------------
# Save
# ----------------------------------------------------------

Fig4_prediction <- list(

  df_wsc = df_wsc,

  df_spawn = df_spawn,

  pred_fl = pred_fl,

  pred_width = pred_width,

  pred_condition = pred_condition,

  pred_sex = pred_sex,

  pred_spawn = pred_spawn

)


dir.create(
  "14_WSC_GAMM_GAM/results",
  showWarnings = FALSE
)


saveRDS(
  Fig4_prediction,
  "14_WSC_GAMM_GAM/results/Fig4_prediction.rds"
)


message(
  "Figure 4 prediction data saved (log(ac_ratio) scale)."
)