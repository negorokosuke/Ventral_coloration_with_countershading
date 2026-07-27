df <- readRDS("01_data/processed/df_processed.rds")

DV_LMM <- lmer(
  log(ac_area) ~ fl_z* width_z + resid_log_Charr + sex + 
    offset(log(fish_area)) +
    (1 | site),
  data = subset(df, species == "DV" & site != "Oct")
)

summary(DV_LMM)

DV_LM_spawn <- lm(
  log(ac_area) ~ fl_z + resid_log_Charr + sex + SPorUNSP +
    offset(log(fish_area)),
  data = subset(df, species == "DV")
)

summary(DV_LM_spawn)

DV_models <- list(
  LMM = DV_LMM,
  LM_spawning = DV_LM_spawn
)

saveRDS(
  DV_models,
  "13_DV_LMM_LM/models/DV_models.rds"
)

DV_results <- list(
  models = DV_models,
  coef_LMM = broom.mixed::tidy(DV_LMM),
  coef_LM = broom::tidy(DV_LM_spawn),
  anova_LMM = anova(DV_LMM),
  anova_LM = anova(DV_LM_spawn)
)

saveRDS(
  DV_results,
  "13_DV_LMM_LM/results/DV_results.rds"
)