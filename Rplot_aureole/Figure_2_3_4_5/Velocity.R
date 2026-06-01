library(ggplot2)
library(dplyr)


modeling1 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000),
  y = c(769.57, 675.166, 606.347, 553.972, 515.496, 486.8, 464.473, 446.32, 431.111, 418.082, 406.764, 396.822, 388.022, 380.178, 373.153, 366.829, 361.122, 355.953, 351.506, 349.966, 348.697, 347.903, 347.269, 346.756, 346.275, 338.167, 331.397, 326.506, 322.532, 320.894, 310.456, 305.64),
  group = "T/°C (IDW velocity)"
)
modeling2 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000),
  y = c(474.938, 436.897, 412.009, 393.84, 379.882, 368.791, 359.78, 352.337, 346.109, 340.84, 336.345, 332.48, 329.135, 326.223, 323.676, 321.437, 319.461, 317.71, 316.154, 314.766, 313.524, 312.41, 311.409, 310.506, 309.689, 308.951, 308.28, 307.67, 307.115, 306.608, 303.346, 301.711),
  group = "T/°C (No velocity field)"
)

# 添加模型标签
modeling1$Model <- "IDW velocity"
modeling2$Model <- "No velocity field"

# 对每一组数据进行插值，生成更多点
interp1 <- as.data.frame(spline(modeling1$x, modeling1$y, n = 400)) #可以调整n
interp1$Model <- "IDW velocity"

interp2 <- as.data.frame(spline(modeling2$x, modeling2$y, n = 400))
interp2$Model <- "No velocity field"

# 合并
interp_all <- bind_rows(interp1, interp2)
colnames(interp_all) <- c("x", "y", "Model")

desired_order <- c(
  "IDW velocity",
  "No velocity field"
)
interp_all$Model <- factor(interp_all$Model, levels = desired_order)

# 合并三组数据
combined_data <- bind_rows(modeling1, modeling2)
combined_data$Model <- factor(combined_data$Model, levels = desired_order)


p <- ggplot(interp_all, aes(x = x, y = y, color = Model, linetype = Model)) +
  geom_line(linewidth = 1) +
  annotate("rect", xmin = 0, xmax = 2500, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3, color = NA) +
  annotate("text", x = 1300, y = 870, label = "Deformed aureole", 
           angle = 0, color = "gray40", size = 5, fontface = "italic") +
  scale_color_manual(values = c("No velocity field" = "lightblue2",
                              "IDW velocity" = "blue4"),
                     guide = guide_legend(reverse = FALSE)
  ) +
  scale_linetype_manual(values = rep("solid", 3),
                        guide = guide_legend(reverse = FALSE)) +
            
  labs(# title = "Simulated temperature profiles across the pluton margin",
    x = "Distance from Intrusion Margin [m]",
    y = "Modeled Peak Temperature [°C]",
       color = "Velocity settings",
       linetype = "Velocity settings") +

  scale_x_continuous(breaks = seq(0, 5000, 1000), limits = c(0, 5000), expand = c(0.05, 0)) +
  scale_y_continuous(breaks = seq(300, 900, 100), limits = c(250, 900), expand = c(0, 0)) +
  
  # 边框
  theme_bw(base_size = 14) +
  theme(
    plot.margin = margin(10, 25, 10, 10),  # 上右下左，单位是pt
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    legend.position = c(0.95, 1), 
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "black", linewidth = 0.5),
    legend.text  = element_text(size = 16),
    legend.key.height = unit(1, "cm"),   
    legend.key.width = unit(1, "cm"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20),
    panel.grid.major = element_blank(), # 去掉主网格
    panel.grid.minor = element_blank(),   # 去掉次网格
    # 去掉默认的 panel.border
    panel.border = element_blank(),
    # 添加坐标轴线：只画左边和下边
    axis.line.x = element_line(color = "black", linewidth = 1),
    axis.line.y = element_line(color = "black", linewidth = 1),
    # 设置刻度线
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(5, "pt")  # 刻度长度，调小就变短横线
  )

ggsave("~/Downloads/Figure_5.png", plot = p, width = 8, height = 6, dpi = 300)

