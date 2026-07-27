#========================================================
# Load
#========================================================

profile_line <- readRDS("01_data/processed/profile_line.rds")

model_lumi <- readRDS("12_lateral_data_processed/models/model_luminance.rds")

#========================================================
# Prediction grid
#========================================================

ac_q <- quantile(
  profile_line$ac_ratio,
  probs = c(0.1,0.9),
  na.rm = TRUE
)

pred_grid2 <- expand.grid(
  dv_pos = seq(0,1,length.out=200),
  species = levels(profile_line$species),
  ac_ratio = ac_q
)

pred_grid2$image_name <- levels(profile_line$image_name)[1]
pred_grid2$site <- levels(profile_line$site)[1]

pred_grid2$ac_level <- factor(
  c(
    rep("Low",200*nlevels(profile_line$species)),
    rep("High",200*nlevels(profile_line$species))
  ),
  levels=c("Low","High")
)

pred_grid2$fit <- predict(
  model_lumi,
  newdata=pred_grid2,
  exclude=c("s(image_name)","s(site)")
)

species_order <- c("DV","WSC")

pred_grid2$species <- factor(
  pred_grid2$species,
  levels=species_order
)

#========================================================
# HSV
#========================================================

max_val <- max(
  c(profile_line$red,
    profile_line$green,
    profile_line$blue),
  na.rm=TRUE
)

rgb_to_hsv <- function(r,g,b,max_val){

  rgb_mat <- cbind(r,g,b)/max_val

  hsv <- rgb2hsv(t(rgb_mat))

  hsv_df <- as.data.frame(t(hsv))

  names(hsv_df) <- c("H","S","V")

  hsv_df

}

profile_line <- bind_cols(
  profile_line,
  rgb_to_hsv(
    profile_line$red,
    profile_line$green,
    profile_line$blue,
    max_val
  )
)

profile_line <- profile_line %>%
  group_by(image_name) %>%
  mutate(
    V_norm=V/max(V)
  ) %>%
  ungroup()

profile_line <- profile_line %>%
  mutate(
    abdominal_flag=
      H>=0&
      H<=40/180&
      S>=90/255&
      V_norm>=0.7
  )

abdominal_freq <-
  profile_line %>%
  group_by(species,dv_pos) %>%
  summarise(
    freq=mean(abdominal_flag),
    .groups="drop"
  )

abdominal_band2 <-
  abdominal_freq %>%
  split(.$species) %>%
  purrr::map_dfr(function(df){

    tibble(
      species=unique(df$species),
      dv_pos=pred_grid2 %>%
        filter(species==unique(df$species)) %>%
        pull(dv_pos) %>%
        unique(),
      freq=approx(
        x=df$dv_pos,
        y=df$freq,
        xout=pred_grid2 %>%
          filter(species==unique(df$species)) %>%
          pull(dv_pos) %>%
          unique(),
        rule=2
      )$y
    )

  })

abdominal_band2$species <- factor(
  abdominal_band2$species,
  levels=species_order
)

tile_height <-
  min(diff(sort(unique(pred_grid2$dv_pos))))

saveRDS(
  pred_grid2,
  "12_lateral_data_processed/predictions/pred_luminance.rds"
)

saveRDS(
  abdominal_band2,
  "12_lateral_data_processed/predictions/red_frequency.rds"
)

saveRDS(
  tile_height,
  "12_lateral_data_processed/predictions/tile_height.rds"
)