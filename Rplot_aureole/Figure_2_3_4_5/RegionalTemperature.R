library(ggplot2)
library(dplyr)

# 原始数据
modeling1 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000, 10000),
  y = c(734.85, 617.239, 532.266, 471.474, 427.978, 395.485, 370.248, 349.886, 332.998, 318.699, 306.423, 295.774, 286.462, 278.265, 271.014, 264.57, 258.821, 253.684, 249.077, 244.939, 241.215, 238.315, 237.461, 236.755, 236.118, 230.411, 225.547, 223.445, 221.748, 220.199, 210.247, 205.661, 200.206),
  group = "T/°C (T_Regional = 200 °C)"
)

modeling2 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000, 10000),
  y = c(769.57, 675.166, 606.347, 553.972, 515.496, 486.8, 464.473, 446.32, 431.111, 418.082, 406.764, 396.822, 388.022, 380.178, 373.153, 366.829, 361.122, 355.953, 351.506, 349.966, 348.697, 347.903, 347.269, 346.756, 346.275, 338.167, 331.397, 326.506, 322.532, 320.894, 310.456, 305.64, 300.139),
  group = "T/°C (T_Regional = 300 °C)"
)
modeling3 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000, 10000),
  y = c(801.62, 728.73, 674.985, 633.733, 602.377, 578.213, 558.873, 543.033, 529.632, 518.045, 507.855, 498.806, 490.708, 483.412, 476.821, 470.856, 465.548, 462.978, 461.287, 460.102, 459.114, 458.523, 458.071, 457.725, 457.377, 446.313, 437.279, 431.126, 426.12, 422.366, 410.764, 405.646, 400.096),
  group = "T/°C (T_Regional = 400 °C)"
)


# 添加模型标签
modeling1$Model <- "200 °C"
modeling2$Model <- "300 °C"
modeling3$Model <- "400 °C"

# 对每一组数据进行插值，生成更多点
interp1 <- as.data.frame(spline(modeling1$x, modeling1$y, n = 400)) #可以调整n
interp1$Model <- "200 °C"

interp2 <- as.data.frame(spline(modeling2$x, modeling2$y, n = 400))
interp2$Model <- "300 °C"

interp3 <- as.data.frame(spline(modeling3$x, modeling3$y, n = 400)) 
interp3$Model <- "400 °C"

# 合并
interp_all <- bind_rows(interp1, interp2, interp3)
colnames(interp_all) <- c("x", "y", "Model")

desired_order <- c(
  "200 °C",
  "300 °C",
  "400 °C"
)
interp_all$Model <- factor(interp_all$Model, levels = desired_order)

# 合并三组数据
combined_data <- bind_rows(modeling1, modeling2, modeling3)
combined_data$Model <- factor(combined_data$Model, levels = desired_order)


p <- ggplot(interp_all, aes(x = x, y = y, color = Model, linetype = Model)) +
  geom_line(linewidth = 1) +
  annotate("rect", xmin = 0, xmax = 2500, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3, color = NA) +
  annotate("text", x = 1300, y = 870, label = "Deformed aureole", 
           angle = 0, color = "gray40", size = 5, fontface = "italic") +
  # # 在最上层绘制半透明矩形
  # annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = Inf,
  #          fill = "orangered", alpha = 0.2, color = NA) +
  # geom_vline(xintercept = 0, linetype = "dashed", color = "orangered", linewidth = 0.5)+
  # # 在矩形内添加文字
  # annotate("text", x = 0.8, y = 950, label = "Pluton", 
  #          angle = 90, color = "orangered", size = 5, fontface = "italic") +
  scale_color_manual(values = c("200 °C" = "blue4",
                                "300 °C" = "deepskyblue3",
                                "400 °C" = "lightblue2"),
                     guide = guide_legend(reverse = FALSE)) +
  scale_linetype_manual(values = rep("solid", 3),
                        guide = guide_legend(reverse = FALSE)) +
  # annotate("segment", x = 0, xend = 40, y = 50,  yend = 50,
  #          linetype = "dotted", color = "blue4", linewidth = 0.5) +
  # annotate("segment", x = 0, xend = 40, y = 150, yend = 150,
  #          linetype = "dotted", color = "skyblue2", linewidth = 0.5) +
  # annotate("segment", x = 0, xend = 40, y = 250, yend = 250,
  #          linetype = "dotted", color = "lightblue2", linewidth = 0.5) +
  labs(# title = "Simulated temperature profiles across the pluton margin",
       x = "Distance from Intrusion Margin [m]",
       y = "Modeled Peak Temperature [°C]",
       color = "Regional temperature",
       linetype = "Regional temperature") +

  scale_x_continuous(breaks = seq(0, 5000, 1000), limits = c(0, 5000), expand = c(0.05, 0)) +
  scale_y_continuous(breaks = seq(100, 900, 100), limits = c(100, 900), expand = c(0, 0)) +
  
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


ggsave("~/Downloads/Figure_2.png", plot = p, width = 8, height = 6, dpi = 300)

