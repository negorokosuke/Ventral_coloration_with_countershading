# ============================================================
# 01_preprocessing.R
# Data preprocessing
# ============================================================

# ---------- Read data ----------
df <- read_csv(here("01_data", "AC_area_summary_simple.csv"))


# ============================================================
# Factor variables
# ============================================================

## Sex
df$sex[is.na(df$sex)] <- 4

df$sex <- factor(
  df$sex,
  levels = c(0, 1, 2, 3),
  labels = c("Female", "Male", "Female", "Male")
)

df$sex <- addNA(df$sex)
levels(df$sex)[is.na(levels(df$sex))] <- "Unidentified"

## Site
df$site <- factor(df$site)

## River type
df$River_type <- ifelse(
  df$site %in% c("MS_T13", "MS_T50", "MS_T54"),
  "Main_stem",
  "Tributary"
)

df$River_type <- factor(
  df$River_type,
  levels = c("Tributary", "Main_stem")
)

# ============================================================
# Body condition
# ============================================================

df$log_FL <- log(df$fl)
df$log_Weight <- log(df$weight)

lm_log_Charr <- lm(
  log_Weight ~ log_FL,
  data = df,
  na.action = na.exclude
)

df$resid_log_Charr <- resid(lm_log_Charr)

# ============================================================
# Site information
# ============================================================

site_info <- tibble(
  site = c(
    "IK", "SS", "SZ",
    "T49", "T50", "TS",
    "MS_T13", "MS_T50",
    "MS_T54", "Oct"
  ),

  width = c(
    375, 546, 505,
    165, 140, 100,
    1325, 1920,
    3580, 505
  ),

  parahucho = c(
    1, 0, 0,
    0, 0, 0,
    1, 1,
    1, 0
  )
)

df <- left_join(df, site_info, by = "site")

df$parahucho <- factor(
  df$parahucho,
  levels = c(0, 1),
  labels = c("Absent", "Present")
)

df$site <- factor(df$site)

# ============================================================
# Continuous variables
# ============================================================

df$log_fish_area <- log(df$fish_area)

df$fl_z <- as.numeric(scale(df$fl))

df$width_z <- as.numeric(scale(df$width))

# ============================================================
# Spawning / Non-spawning
# ============================================================

df$SPorUNSP <- case_when(
  df$site == "Oct" ~ "Spawning_area",
  df$site == "SZ"  ~ "Unspawning_area",
  TRUE             ~ NA_character_
)

df$SPorUNSP <- factor(
  df$SPorUNSP,
  levels = c(
    "Unspawning_area",
    "Spawning_area"
  )
)

# ============================================================
# Save processed data
# ============================================================

dir.create(
  "01_data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

saveRDS(
  df,
  "01_data/processed/df_processed.rds"
)

message("Preprocessing completed.")