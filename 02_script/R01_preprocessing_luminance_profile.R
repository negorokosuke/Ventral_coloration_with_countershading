library(here)
library(dplyr)
library(purrr)
library(stringr)
library(readr)

# ==========================================================
# 01_preprocessing.R
# Create profile_line dataset
# ==========================================================

# ----------------------------------------------------------
# Read RGB profile data
# ----------------------------------------------------------
# ----------------------------------------------------------
# Read RGB profile data
# ----------------------------------------------------------

rgb_folder <- here(
  "01_data",
  "RGB_lateral_16bit"
)

files <- list.files(
  path = rgb_folder,
  pattern = "\\.csv$",
  full.names = TRUE
)

raw_profile <- purrr::map_dfr(
  files,
  read.csv
)

raw_profile <- files %>%
  purrr::map_dfr(read.csv) %>%
  mutate(
    image_num = as.numeric(stringr::str_extract(image, "\\d+"))
  )

# ----------------------------------------------------------
# Correct image numbers
# ----------------------------------------------------------

raw_profile <- raw_profile %>%
  mutate(
    image_num_adj = case_when(
      image_num >= 8209781 & image_num <= 8209933 ~ image_num + 1,
      image_num >= 8210001 & image_num <= 8210107 ~ image_num + 1,
      image_num >= 8219973 & image_num <= 8219998 ~ image_num + 1,
      TRUE ~ image_num - 1
    )
  )

map_from <- c(
  8209932,8209942,8209947,8209962,9130758,9130795,90802,90961,
  8220332,8220427,8220457,8210003,8210026,8210029,8210042,8210045,
  8210048,8210051,8210054,8210058,8210061,8210066,8210092,8210095,
  8210098,8210101,8210106,8219974,8219978,8219981,8219987,8219991,
  8219995,8220566,8209777,8209785,8209794,8209804,8209807,8209812,
  8209815,8209818,8209825,8209828,8209831,8209834,8209838,8209842,
  8209852,8209856,8209859,8209862,8209865,8209871,8209874,8209877,
  8209883,8209886,8209889,8209893,8209896,8209904,8209907,8209911,
  8209914,8209918,8209921,8209924,8209928,8210139,8210220,8210280
)

map_to <- c(
  8209933,8209941,8209952,8209962,9130760,9130794,90801,90960,
  8220331,8220426,8220456,8210004,8210027,8210030,8210043,8210046,
  8210049,8210052,8210056,8210059,8210062,8210067,8210093,8210096,
  8210099,8210102,8210104,8219975,8219979,8219983,8219989,8219993,
  8219996,8220565,8209777,8209786,8209796,8209805,8209808,8209812,
  8209816,8209819,8209826,8209829,8209832,8209835,8209839,8209846,
  8209854,8209857,8209860,8209863,8209868,8209872,8209875,8209878,
  8209884,8209887,8209891,8209894,8209900,8209905,8209908,8209912,
  8209915,8209919,8209922,8209926,8209930,8210138,8210220,8210279
)

map_vec <- setNames(map_to, map_from)

raw_profile <- raw_profile %>%
  mutate(
    image_num_adj = if_else(
      image_num_adj %in% map_from,
      as.numeric(map_vec[as.character(image_num_adj)]),
      image_num_adj
    ),
    image_name = paste0("P", image_num_adj, ".jpg")
  )

# ----------------------------------------------------------
# Read fish metadata
# ----------------------------------------------------------

fish <- read.csv("01_data/AC_area_summary_simple.csv")

fish <- fish %>%
  mutate(
    image_num = as.numeric(str_extract(image_name, "\\d+"))
  )

# ----------------------------------------------------------
# Merge metadata
# ----------------------------------------------------------

profile_raw <- raw_profile %>%
  left_join(
    fish,
    by = c("image_num_adj" = "image_num")
  ) %>%
  rename(
    image_name = image_name.y
  )

# ----------------------------------------------------------
# Standardize dorsoventral position
# ----------------------------------------------------------

profile_raw <- profile_raw %>%
  group_by(image_name) %>%
  mutate(
    dv_pos = (position - min(position)) /
      (max(position) - min(position))
  ) %>%
  ungroup()

# ----------------------------------------------------------
# Calculate luminance
# ----------------------------------------------------------

profile_raw <- profile_raw %>%
  mutate(
    species = factor(species),

    luminance = G,

    red   = R,
    green = G,
    blue  = B
  )

# ----------------------------------------------------------
# Average parallel transects
# ----------------------------------------------------------

meta_data <- profile_raw %>%
  group_by(image_name) %>%
  summarise(
    across(
      c(
        species,
        site,
        year,
        fish_area,
        ac_area,
        ac_ratio,
        fl,
        weight,
        sex
      ),
      first
    ),
    .groups = "drop"
  )

profile_line <- profile_raw %>%
  mutate(
    dv_pos = round(dv_pos, 3)
  ) %>%
  group_by(
    image_name,
    species,
    dv_pos
  ) %>%
  summarise(
    luminance = mean(luminance),
    red = mean(red),
    green = mean(green),
    blue = mean(blue),
    .groups = "drop"
  ) %>%
  left_join(
    meta_data,
    by = c("image_name", "species")
  )

# ----------------------------------------------------------
# Factor
# ----------------------------------------------------------

profile_line <- profile_line %>%
  mutate(
    species = factor(species),
    site = factor(site),
    image_name = factor(image_name)
  )

# ----------------------------------------------------------
# Save
# ----------------------------------------------------------

saveRDS(
  profile_line,
  "01_data/processed/profile_line.rds"
)

