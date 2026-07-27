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
  mutate(x_pos=2.5)

abdominal_overlay <-
  abdominal_band2 %>%
  mutate(x_pos=2.5)

p <- ggplot() +

  ## 左：Luminance
  geom_tile(
    data = pred_high_left,
    aes(
      x = x_pos,
      y = dv_pos,
      fill = fit
    ),
    width = 1,
    height = tile_height
  ) +

  ## 右：Luminance & red frequency
  geom_tile(
    data = pred_high_right,
    aes(
      x = x_pos,
      y = dv_pos,
      fill = fit
    ),
    width = 1,
    height = tile_height
  ) +

  ## 赤色頻度オーバーレイ
  geom_tile(
    data = abdominal_overlay,
    aes(
      x = x_pos,
      y = dv_pos,
      alpha = freq
    ),
    fill = "red",
    width = 1,
    height = tile_height
  ) +

  ## ★ DV と WSC を横に並べる
  facet_wrap(
    ~species,
    nrow = 1,
    labeller = labeller(
      species = c(
        DV = "Dolly Varden",
        WSC = "White-spotted charr"
      )
    )
  ) +

  ## ★ x 軸にパネル名を表示（各パネル下に配置される）
  scale_x_continuous(
    breaks = c(1, 2.5),
    labels = c("Luminance",
               "Luminance & red frequency"),
    limits = c(0.5, 3.0),
  expand = expansion(
    mult = c(0, 0.05)
  )) +

  labs(
    x = NULL,
    y = "Dorsoventral position"
  ) +

  scale_fill_gradient(
    low = "grey0",
    high = "grey100",
    name = "Luminance",
    guide = guide_colorbar(order = 1)
  ) +

  scale_alpha(
    range = c(0, 0.8),
    name = "Red coloration\nfrequency",
    guide = guide_legend(order = 2)
  ) +

  coord_cartesian(clip = "off") +
  scale_y_reverse(expand = c(0, 0)) +

  theme(
    legend.position = "none",   # 凡例を消す
    strip.text = element_blank(),  # 種名ラベルを消す
    panel.spacing.x = unit(1.6, "cm"),

    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 14,
                               margin = margin(r = 2)),
    axis.title.y = element_text(size = 15),
    axis.line.y = element_line(colour = "black", linewidth = 0.8),
    axis.line.x = element_blank(),

    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.background = element_blank(),
    plot.margin = margin(5, 25, 5, 5)
  )

ggsave(
  filename = "12_lateral_data_processed/figures/Figure2_bottom.pdf",
  plot = p,
  width = 7,
  height = 4,
  units = "in"
)

print(p)
##############################################
