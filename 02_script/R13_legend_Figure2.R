pred_grid2 <-
  readRDS(
    "12_lateral_data_processed/predictions/pred_luminance.rds"
  )

abdominal_band2 <-
  readRDS(
    "12_lateral_data_processed/predictions/red_frequency.rds"
  )

tile_height <-
  readRDS(
    "12_lateral_data_processed/predictions/tile_height.rds"
  )

pred_high <- pred_grid2 %>%
  filter(ac_level=="High")

pred_high_left <- pred_high %>%
  mutate(x_pos=1)

pred_high_right <- pred_high %>%
  mutate(x_pos=2)

abdominal_overlay <-
  abdominal_band2 %>%
  mutate(x_pos=2)
  
#凡例
library(ggplot2)

df_lum <- data.frame(
  lum = seq(0, 40000, length.out = 100),
  x = 1,
  y = seq(0, 1, length.out = 100)
)

p_lum <- ggplot(df_lum, aes(x = x, y = y, fill = lum)) +
  geom_tile() +
  scale_fill_gradient(
    low = "black",
    high = "white",
    limits = c(0, 40000),
    name = "Luminance",
    guide = guide_colorbar(
      title.position = "top",
      title.vjust = 2,     # ← 隙間を広げる
      barheight = unit(4, "cm"),
      barwidth  = unit(0.6, "cm")
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(
      size = 14,
      margin = margin(b = 8)   # ← タイトルの下に余白
    ),
    legend.text  = element_text(size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

legend_lum <- get_legend(p_lum)

df_red <- data.frame(
  freq = seq(0, 0.8, length.out = 100),
  x = 1,
  y = seq(0, 1, length.out = 100)
)

p_red <- ggplot(df_red, aes(x = x, y = y, fill = freq)) +
  geom_tile() +
  scale_fill_gradientn(
    colours = c("#ffffff", "#ffb3b3", "#ff6666", "#ff0000"),
    values = scales::rescale(c(0, 0.3, 0.6, 0.8)),
    limits = c(0, 0.8),
    name = "Red coloration\nfrequency",
    guide = guide_colorbar(
      title.position = "top",
      title.vjust = 2,     # ← 隙間を広げる
      barheight = unit(4, "cm"),
      barwidth  = unit(0.6, "cm")
    )
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(
      size = 14,
      margin = margin(b = 8)   # ← タイトルの下に余白
    ),
    legend.text  = element_text(size = 12),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
  
legend_red <- get_legend(p_red)

legend <- plot_grid(
  legend_lum,
  legend_red,
  ncol = 1,
  align = "v",
  axis = "lr"
)

ggsave(
  filename = "12_lateral_data_processed/figures/Figure2_bottom_legend.pdf",
  plot = legend,
  width = 2,
  height = 6,
  units = "in"
)

print(legend)
