#==================================================
# Load prediction data
#==================================================

Fig3_prediction <- readRDS(
  "13_DV_LMM_LM/results/Fig3_prediction.rds"
)

pred_fl        <- Fig3_prediction$pred_fl
pred_width     <- Fig3_prediction$pred_width
pred_condition <- Fig3_prediction$pred_condition
pred_sex       <- Fig3_prediction$pred_sex
pred_spawn     <- Fig3_prediction$pred_spawn

df_dv          <- Fig3_prediction$df_dv
df_spawn <- Fig3_prediction$df_spawn |>
  dplyr::filter(
    !is.na(SPorUNSP)
  )

width_vals     <- Fig3_prediction$width_vals
condition_vals <- Fig3_prediction$condition_vals

#==================================================
# 共通設定
#==================================================

x_range <- range(df_dv$fl_z, na.rm = TRUE)
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
# p1
#==================================================

p1 <- ggplot(
  df_dv,
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
    aes(x = x,
        y = predicted),
    inherit.aes = FALSE,
    linewidth = 1.3,
    color = "black"
  ) +

  geom_ribbon(
    data = pred_fl,
    aes(x = x,
        ymin = conf.low,
        ymax = conf.high),
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
# p2
#==================================================

p2 <- ggplot(
  df_dv,
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
    aes(x = x,
        y = predicted,
        color = width_val,
        group = group),
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

  # 凡例タイトル変更
  scale_color_viridis_c(
    name = "River width",
    guide = guide_colorbar(
      barheight = unit(20, "mm"),
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

  # 横軸・縦軸の目盛りを消す
  theme(
    axis.text.y  = element_text(color = NA),
    axis.ticks.y = element_line(color = NA),

    axis.text.x  = element_text(color = NA),
    axis.ticks.x = element_line(color = NA)
  )

#==================================================
# p3
#==================================================

#==================================================
# p3 : body condition
#==================================================

p3 <- ggplot(
  df_dv,
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

  geom_line(
    data = pred_condition,
    aes(
      x = x,
      y = predicted,
      color = condition_val,
      group = group
    ),
    inherit.aes = FALSE,
    linewidth = 1.4
  ) +

  geom_ribbon(
    data = pred_condition,
    aes(
      x = x,
      ymin = conf.low,
      ymax = conf.high,
      fill = condition_val,
      group = group
    ),
    inherit.aes = FALSE,
    alpha = 0.12,
    color = NA
  ) +

  scale_color_viridis_c(
    name = "Condition",
    guide = guide_colorbar(
      barheight = unit(20, "mm"),
      barwidth  = unit(4, "mm")
    )
  ) +

  scale_fill_viridis_c(
    guide = "none"
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
  df_dv,
  aes(fl_z,
      log(ac_ratio),
      color = sex)
) +

  geom_point(
    data = subset(df_dv,
                  sex != "Unidentified"),
    alpha = 0.45,
    size = 1.3
  ) +

  geom_point(
    data = subset(df_dv,
                  sex == "Unidentified"),
    shape = 4,
    stroke = 1,
    size = 2
  ) +

  geom_line(
    data = pred_sex,
    aes(x = x,
        y = predicted,
        color = group),
    inherit.aes = FALSE,
    linewidth = 1.3
  ) +

  geom_ribbon(
    data = pred_sex,
    aes(x = x,
        ymin = conf.low,
        ymax = conf.high,
        fill = group),
    inherit.aes = FALSE,
    alpha = 0.12,
    color = NA
  ) +

  labs(
    color = "Sex"
  ) +

  guides(
    fill = "none"
  ) +

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

  # 横軸・縦軸の目盛りを消す
  theme(
    axis.text.y  = element_text(color = NA),
    axis.ticks.y = element_line(color = NA),

    axis.text.x  = element_text(color = NA),
    axis.ticks.x = element_line(color = NA)
  )

#==================================================
# p5 : spawning status
#==================================================

p5 <- ggplot(
  df_spawn,
  aes(fl_z,
      log(ac_ratio),
      color = SPorUNSP)
) +

  geom_point(
    alpha = 0.45,
    size = 1.3
  ) +

  geom_line(
    data = pred_spawn,
    aes(x = x,
        y = predicted,
        color = group),
    inherit.aes = FALSE,
    linewidth = 1.3
  ) +

  geom_ribbon(
    data = pred_spawn,
    aes(x = x,
        ymin = conf.low,
        ymax = conf.high,
        fill = group),
    inherit.aes = FALSE,
    alpha = 0.12,
    color = NA
  ) +

  scale_color_discrete(
    name = "Season",
    labels = c(
      "Spawning area"   = "Spawning season",
      "Unspawning area" = "Unspawning season"
    )
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

  guides(
    fill = "none"
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

  # p5は縦軸のみ消す
  theme(
    axis.text.y  = element_text(color = NA),
    axis.ticks.y = element_line(color = NA)
  )


#==================================================
# blank
#==================================================

blank_panel <- patchwork::plot_spacer()

#==================================================
# patchwork layout
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
    legend.position = c(0.97, 0.03),
    legend.justification = c(1, 0)
  )

fig3 <- ggdraw() +

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

fig3

dir.create(
  "figures",
  showWarnings = FALSE
)

ggsave(
  filename = "13_DV_LMM_LM/figures/Fig3.pdf",
  plot = fig3,
  width = 300,
  height = 200,
  units = "mm",
  device = cairo_pdf
)

ggsave(
  filename = "13_DV_LMM_LM/figures/Fig3.png",
  plot = fig3,
  width = 300,
  height = 200,
  units = "mm",
  dpi = 600
)
