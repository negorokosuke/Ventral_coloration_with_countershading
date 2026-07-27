#--------------------------------------------------
# Original data
#--------------------------------------------------

df <- readRDS(
  "01_data/processed/df_processed.rds"
)


#--------------------------------------------------
# Figure 4 prediction data
#--------------------------------------------------

Fig4_prediction <- readRDS(
  "14_WSC_GAMM_GAM/results/Fig4_prediction.rds"
)


# Extract prediction objects

df_wsc <- Fig4_prediction$df_wsc

df_spawn <- Fig4_prediction$df_spawn

pred_fl <- Fig4_prediction$pred_fl

pred_width <- Fig4_prediction$pred_width

pred_condition <- Fig4_prediction$pred_condition

pred_sex <- Fig4_prediction$pred_sex

pred_spawn <- Fig4_prediction$pred_spawn



#--------------------------------------------------
# Plotting datasets
#--------------------------------------------------

# All WSC observations
df_wsc_all <- df_wsc


# Spawning dataset
df_spawn_plot <- df_spawn



#--------------------------------------------------
# Model (for common prediction line)
#--------------------------------------------------

WSC_models <- readRDS(
  "14_WSC_GAMM_GAM/models/WSC_models.rds"
)


WSC_GAM_offset <- 
  WSC_models$GAMM



#--------------------------------------------------
# Site reference for random effect exclusion
#--------------------------------------------------

site_ref <- levels(
  df_wsc$site
)[1]



#--------------------------------------------------
# Check variables
#--------------------------------------------------

str(pred_fl)
str(pred_width)
str(pred_spawn)

#==================================================
# 共通設定
#==================================================

x_range <- range(df_wsc$fl_z, na.rm = TRUE)
y_range <- c(-7, 0)

common_theme <- theme_bw(base_size = 14) +

  theme(
    axis.title = element_blank(),

    # 目盛り文字サイズを大きく
    axis.text = element_text(size = 15),

    # 目盛り線サイズ
    axis.ticks = element_line(linewidth = 0.5),

    plot.title = element_blank(),

    panel.grid.minor = element_blank(),

    plot.margin = margin(
      t = 0,
      r = 0,
      b = 0,
      l = 0
    ),

    panel.spacing = unit(0.5, "mm"),

    legend.position = c(0.97, 0.05),
    legend.justification = c(1, 0),
    legend.key.size = unit(3, "mm"),

    legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = NA
    ),

    # 凡例サイズを少し大きく
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12)
  )
  
#==================================================
# p1 : fl
#==================================================

p1 <- ggplot(
  df_wsc_all,
  aes(fl_z,
      log(ac_ratio))
) +

  geom_point(
    alpha = 0.45,
    size = 1.5,
    color = "grey40"
  ) +

  geom_line(
  data = pred_fl,
  aes(
    x = x,
    y = log_ac_ratio
  ),
  inherit.aes = FALSE,
  linewidth = 1.3,
  color = "black"
  ) +

  geom_ribbon(
  data = pred_fl,
  aes(
    x = x,
    ymin = conf.low,
    ymax = conf.high
  ),
  inherit.aes = FALSE,
  alpha = 0.15
  ) +

  coord_cartesian(
    xlim = x_range,
    ylim = y_range
  ) +

  annotate(
    "text",
    x = x_range[1],
    y = -0.2,
    label = "(A)",
    hjust = 0,
    vjust = 1,
    size = 5,
    fontface = "bold"
  ) +

  common_theme

#==================================================
# p2 : width
#==================================================

p2 <- ggplot(
  df_wsc,
  aes(fl_z,
      log(ac_ratio),
      color = width_z)
) +

  geom_point(
    alpha = 0.45,
    size = 1.3
  ) +

  geom_line(
  data = pred_width,
  aes(
    x = x,
    y = log_ac_ratio,
    color = width_val,
    group = group
    ),
    inherit.aes = FALSE,
    linewidth = 1.4
  ) +

  geom_ribbon(
    data = pred_width,
    aes(x = x,
        ymin = conf.low,
        ymax = conf.high,
        fill = width_val,
        group = group),
    inherit.aes = FALSE,
    alpha = 0.12,
    color = NA
  ) +

  scale_color_viridis_c(
    name = "River width",

    guide = guide_colorbar(
      barheight = unit(22, "mm"),
      barwidth  = unit(4, "mm")
    )
  ) +

  scale_fill_viridis_c(guide = "none") +

  coord_cartesian(
    xlim = x_range,
    ylim = y_range
  ) +

  annotate(
    "text",
    x = x_range[1],
    y = -0.2,
    label = "(B)",
    hjust = 0,
    vjust = 1,
    size = 5,
    fontface = "bold"
  ) +

  common_theme +

  theme(
    axis.text.y  = element_text(color = NA),
    axis.ticks.y = element_line(color = NA),

    axis.text.x  = element_text(color = NA),
    axis.ticks.x = element_line(color = NA)
  )

#==================================================
new_common <- data.frame(
  fl_z = seq(
    min(df_wsc$fl_z, na.rm = TRUE),
    max(df_wsc$fl_z, na.rm = TRUE),
    length.out = 200
  ),
  width_z = 0,
  resid_log_Charr = 0,
  sex = "Female",
  fish_area = 1,
  site = site_ref
)

pred <- predict(
  WSC_GAM_offset,
  newdata = new_common,
  se.fit = TRUE,
  exclude = "s(site)"
)

pred_common <- new_common %>%
  mutate(
    x = fl_z,
    log_ac_ratio = pred$fit,
    conf.low = pred$fit - 1.96 * pred$se.fit,
    conf.high = pred$fit + 1.96 * pred$se.fit
  )
#==================================================
# p3 : charr
#==================================================

p3 <- ggplot(
  df_wsc,
  aes(
    fl_z,
    log(ac_ratio),
    color = resid_log_Charr
  )
) +

  geom_point(
    alpha = 0.45,
    size = 1.3
  ) +

  geom_ribbon(
    data = pred_common,
    aes(
      x = x,
      ymin = conf.low,
      ymax = conf.high
    ),
    inherit.aes = FALSE,
    alpha = 0.10,
    fill = "grey40"
  ) +

  geom_line(
  data = pred_common,
  aes(
    x = x,
    y = log_ac_ratio
    ),
    inherit.aes = FALSE,
    linewidth = 1.2,
    linetype = "22",
    color = "black"
  ) +

  scale_color_viridis_c(
    name = "Condition",

    guide = guide_colorbar(
      barheight = unit(22, "mm"),
      barwidth  = unit(4, "mm")
    )
  ) +

  coord_cartesian(
    xlim = x_range,
    ylim = y_range
  ) +

  annotate(
    "text",
    x = x_range[1],
    y = -0.2,
    label = "(D)",
    hjust = 0,
    vjust = 1,
    size = 5,
    fontface = "bold"
  ) +

  common_theme

#==================================================
# p4 : sex
#==================================================

p4 <- ggplot(
  df_wsc,
  aes(
    fl_z,
    log(ac_ratio),
    color = sex
  )
) +

  geom_point(
    data = subset(df_wsc,
                  sex != "Unidentified"),
    alpha = 0.45,
    size = 1.3
  ) +

  geom_point(
    data = subset(df_wsc,
                  sex == "Unidentified"),
    shape = 4,
    stroke = 1,
    size = 2
  ) +

  geom_ribbon(
    data = pred_common,
    aes(
      x = x,
      ymin = conf.low,
      ymax = conf.high
    ),
    inherit.aes = FALSE,
    alpha = 0.10,
    fill = "grey40"
  ) +

  geom_line(
  data = pred_common,
  aes(
    x = x,
    y = log_ac_ratio
    ),
    inherit.aes = FALSE,
    linewidth = 1.2,
    linetype = "22",
    color = "black"
  ) +

  labs(color = "Sex") +

  coord_cartesian(
    xlim = x_range,
    ylim = y_range
  ) +

  annotate(
    "text",
    x = x_range[1],
    y = -0.2,
    label = "(C)",
    hjust = 0,
    vjust = 1,
    size = 5,
    fontface = "bold"
  ) +

  common_theme +

  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank(),

    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  )

#==================================================
# p5 : spawning status
#==================================================

p5 <- ggplot(
  df_spawn_plot,
  aes(
    fl_z,
    log(ac_ratio),
    color = SPorUNSP
  )
) +

  geom_point(
    alpha = 0.45,
    size = 1.3
  ) +

  geom_smooth(
    method = "gam",
    formula = y ~ s(x, k = 5),
    se = TRUE,
    linewidth = 1.1,
    linetype = "22",
    color = "black"
  ) +

  scale_color_manual(
    values = c(
      "firebrick3",
      "navy"
    ),
    labels = c(
      "Spawning",
      "Non-spawning"
    ),
    name = "Spawning status"
  ) +

  coord_cartesian(
    xlim = x_range,
    ylim = y_range
  ) +

  annotate(
    "text",
    x = x_range[1],
    y = -0.2,
    label = "(E)",
    hjust = 0,
    vjust = 1,
    size = 5,
    fontface = "bold"
  ) +

  common_theme +

  theme(
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  )


#==================================================
# blank
#==================================================

blank_panel <- patchwork::plot_spacer()

#==================================================
# layout
#==================================================
final_plot <- (
  p1 | p2 | p4
) / (
  blank_panel | p3 | p5
) +
  plot_layout(
    widths = c(1, 1, 1),
    heights = c(1, 1)) &
  theme(
    plot.margin = margin(1, 1, 15, 15)
  )

#==================================================
# figure
#==================================================

panel_plot <- final_plot &
  theme(
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),

    legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      color = NA
    ),

    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 11)
  )

fig4 <- ggdraw() +

  draw_plot(
    panel_plot,
    x = 0.08,
    y = 0.08,
    width = 0.88,
    height = 0.88
  ) +

  draw_label(
    "Standardized fork length",
    x = 0.5,
    y = 0.11,
    size = 17
  ) +

  draw_label(
    "log(abdominal coloration area / ventral area)",
    x = 0.1,
    y = 0.55,
    angle = 90,
    size = 17
  )

fig4

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  filename = "14_WSC_GAMM_GAM/figures/Fig4.pdf",
  plot = fig4,
  width = 300,
  height = 200,
  units = "mm",
  device = cairo_pdf
)

ggsave(
  filename = "14_WSC_GAMM_GAM/figures/Fig4.png",
  plot = fig4,
  width = 300,
  height = 200,
  units = "mm",
  dpi = 600
)