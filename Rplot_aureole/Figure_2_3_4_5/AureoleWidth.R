library(ggplot2)
library(dplyr)
library(pracma)

modeling1 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 4500, 5000),
  y = c(794, 714.494, 652.368, 601.584, 561.716, 489.96, 454.504, 429.582, 410.498, 395.4, 383.165, 373.071, 364.623, 357.47, 351.356, 346.087, 341.514, 337.523, 334.019, 330.929, 328.192, 325.757, 323.586, 321.642, 319.897, 318.325, 316.906, 315.623, 314.459, 313.4, 306.645, 304.681, 303.256),
  group = "T/°C (width_Aureole = 500 m)"
)
modeling2 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 4500, 5000),
  y = c(769.57, 675.166, 606.347, 553.972, 515.496, 486.8, 464.473, 446.32, 431.111, 418.082, 406.764, 396.822, 388.022, 380.178, 373.153, 366.829, 361.122, 355.953, 351.506, 349.966, 348.697, 347.903, 347.269, 346.756, 346.275, 338.167, 331.397, 326.506, 322.532, 320.894, 310.456, 307.668, 305.64),
  group = "T/°C (width_Aureole = 2500 m)"
)
modeling3 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 4500, 5000),
  y = c(762.94, 663.875, 591.59, 536.602, 496.597, 466.645, 443.673, 425.584, 411.076, 399.206, 389.337, 380.991, 373.834, 367.622, 362.173, 357.353, 353.058, 349.209, 345.738, 342.595, 339.736, 337.128, 334.738, 332.545, 330.524, 328.658, 326.931, 325.331, 323.843, 322.459, 320.266, 321.559, 309.879),
  group = "T/°C (width_Aureole = 4500 m)"
)

# 添加模型标签
modeling1$Model <- "width_Aureole = 500 m"
modeling2$Model <- "width_Aureole = 2500 m"
modeling3$Model <- "width_Aureole = 4500 m"

# 生成新的 x 网格
xnew1 <- seq(min(modeling1$x), max(modeling1$x), length.out = 400)
xnew2 <- seq(min(modeling2$x), max(modeling2$x), length.out = 400)
xnew3 <- seq(min(modeling3$x), max(modeling3$x), length.out = 400)

# 插值
interp1 <- data.frame(
  x = xnew1,
  y = pchip(modeling1$x, modeling1$y, xnew1),
  Model = "500 m"
)

interp2 <- data.frame(
  x = xnew2,
  y = pchip(modeling2$x, modeling2$y, xnew2),
  Model = "2500 m"
)

interp3 <- data.frame(
  x = xnew3,
  y = pchip(modeling3$x, modeling3$y, xnew3),
  Model = "4500 m"
)
# # 对每一组数据进行插值，生成更多点
# interp1 <- as.data.frame(spline(modeling1$x, modeling1$y, n = 400)) #可以调整n
# interp1$Model <- "500 m"
# 
# interp2 <- as.data.frame(spline(modeling2$x, modeling2$y, n = 400))
# interp2$Model <- "2500 m"
# 
# interp3 <- as.data.frame(spline(modeling3$x, modeling3$y, n = 400))
# interp3$Model <- "4500 m"


# 合并
interp_all <- bind_rows(interp1, interp2, interp3)
# colnames(interp_all) <- c("x", "y", "Model")

desired_order <- c(
  "500 m",
  "2500 m",
  "4500 m"
)
interp_all$Model <- factor(interp_all$Model, levels = desired_order)

combined_data <- bind_rows(modeling1, modeling2, modeling3)
combined_data$Model <- factor(combined_data$Model, levels = desired_order)

# 合并三组数据

p <- ggplot(interp_all, aes(x = x, y = y, color = Model, linetype = Model)) +
  geom_line(linewidth = 1.0) +
  geom_vline(xintercept = 500, color = "blue4", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 500, y = max(interp_all$y), label = "500 m", 
           angle = 90, vjust = +2, color = "black")+
  geom_vline(xintercept = 2500, color = "deepskyblue3", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 2500, y = max(interp_all$y), label = "2500 m", 
           angle = 90, vjust = +2, color = "black")+
  geom_segment(x = 4500, xend = 4500, 
               y = 0, yend = 600,
               color = "lightblue2",
               linetype = "dashed",
               linewidth = 1)+
  annotate("text", x = 4500, y = 500, label = "4500 m", 
           angle = 90, vjust = +2, color = "black")+
  scale_color_manual(
    values = c(
      "500 m" = "blue4",
      "2500 m" = "deepskyblue3",
      "4500 m" = "lightblue2"
      # "500 m" = "blue4",
      # "300 m" = "dodgerblue3",
      # "500 m" = "skyblue2",
      # "1,000 m" = "lightblue2",
      # "5,000 m" = "orangered",
      # "10,000 m" = "orange"
      
    ),
    guide = guide_legend(reverse = FALSE)
  ) +
  scale_linetype_manual(values = rep("solid", 6), guide = guide_legend(reverse = FALSE)) +
  
  labs(# title = "Simulated temperature profiles across the pluton margin",
       x = "Distance from Intrusion Margin [m]",
       y = "Modeled Peak Temperature [°C]",
       color = "Aureole width",
       linetype = "Aureole width") +

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



ggsave("~/Downloads/Figure_4.png", plot = p, width = 8, height = 6, dpi = 300)

