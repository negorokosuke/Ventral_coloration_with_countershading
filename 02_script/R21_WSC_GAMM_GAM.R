df <- readRDS("01_data/processed/df_processed.rds")

WSC_GAMM <- gam(
  log(ac_area) ~ 
    te(fl_z, width_z) +  
    resid_log_Charr +
    sex +
    offset(log(fish_area)) +    # ★ 体サイズ補正（log）★
    s(site, bs = "re"),          # site のランダム効果
  data = subset(df, species == "WSC" & site != "Oct"),
  method = "REML"
)

summary(WSC_GAMM)

WSC_GAM_spawn <- gam(
  log(ac_area) ~ 
    s(fl_z) + 
    resid_log_Charr +
    sex +
    SPorUNSP +
    offset(log(fish_area)) , 
  data = subset(df, species == "WSC"),
  method = "REML"
)

summary(WSC_GAM_spawn)

WSC_models <- list(
  GAMM = WSC_GAMM,
  GAM_spawn = WSC_GAM_spawn
)

saveRDS(
  WSC_models,
  "14_WSC_GAMM_GAM/models/WSC_models.rds"
)

WSC_results <- list(

    models = WSC_models,

    parametric_GAMM =
        summary(WSC_GAMM)$p.table,

    smooth_GAMM =
        summary(WSC_GAMM)$s.table,

    parametric_GAM =
        summary(WSC_GAM_spawn)$p.table,

    smooth_GAM =
        summary(WSC_GAM_spawn)$s.table,

    anova_GAMM =
        anova(WSC_GAMM),

    anova_GAM =
        anova(WSC_GAM_spawn)

)

saveRDS(
  WSC_results,
  "14_WSC_GAMM_GAM/results/WSC_results.rds"
)