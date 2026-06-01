library(ggplot2)
library(dplyr)
library(pracma)


modeling1 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000),
  y = c(786.81, 699.85, 633.76, 579.868, 537.594, 505.032, 479.431, 458.761, 441.615, 427.051, 414.498, 403.548, 393.908, 385.36, 377.738, 370.91, 364.774, 359.243, 354.246, 349.723, 346.535, 344.819, 343.525, 342.431, 341.465, 335.498, 330.343, 326.248, 323.936, 322.205, 311.145, 305.873),
  group = "T/°C (time_Emplacement = 150 kyr)"
)

modeling2 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000),
  y = c(769.57, 675.166, 606.347, 553.972, 515.496, 486.8, 464.473, 446.32, 431.111, 418.082, 406.764, 396.822, 388.022, 380.178, 373.153, 366.829, 361.122, 355.953, 351.506, 349.966, 348.697, 347.903, 347.269, 346.756, 346.275, 338.167, 331.397, 326.506, 322.532, 320.894, 310.456, 305.64),
  group = "T/°C (time_Emplacement = 100 kyr)"
)
modeling3 <- data.frame(
  x = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000, 2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000, 4000, 5000),
  y = c(725.769, 623.735, 557.149, 514.605, 485.415, 463.484, 445.847, 431.085, 418.423, 407.395, 397.686, 389.066, 381.36, 374.437, 368.186, 362.525, 358.645, 357.783, 357.342, 357.046, 356.996, 357.089, 357.248, 357.461, 357.536, 342.462, 332.121, 325.699, 321.28, 319.451, 309.693, 305.29),
  group = "T/°C (time_Emplacement = 50 kyr)"
)


# 添加模型标签
modeling1$Model <- "time_Emplacement = 150 kyr"
modeling2$Model <- "time_Emplacement = 100 kyr"
modeling3$Model <- "time_Emplacement = 50 kyr"
# 
# # 对每一组数据进行插值，生成更多点
# interp1 <- as.data.frame(spline(modeling1$x, modeling1$y, n = 400)) #可以调整n
# interp1$Model <- "150 kyr"
# 
# interp2 <- as.data.frame(spline(modeling2$x, modeling2$y, n = 400))
# interp2$Model <- "100 kyr"
# 
# interp3 <- as.data.frame(spline(modeling3$x, modeling3$y, n = 400)) 
# interp3$Model <- "50 kyr"
# 
# # 合并
# interp_all <- bind_rows(interp1, interp2, interp3)
# colnames(interp_all) <- c("x", "y", "Model")
# 
# desired_order <- c(
#   "50 kyr",
#   "100 kyr",
#   "150 kyr"
# )
# interp_all$Model <- factor(interp_all$Model, levels = desired_order)
# 
# # 合并三组数据
# combined_data <- bind_rows(modeling1, modeling2, modeling3)
# combined_data$Model <- factor(combined_data$Model, levels = desired_order)
# 

# 生成新的 x 网格
xnew1 <- seq(min(modeling1$x), max(modeling1$x), length.out = 400)
xnew2 <- seq(min(modeling2$x), max(modeling2$x), length.out = 400)
xnew3 <- seq(min(modeling3$x), max(modeling3$x), length.out = 400)

# 插值
interp1 <- data.frame(
  x = xnew1,
  y = pchip(modeling1$x, modeling1$y, xnew1),
  Model = "150 kyr"
)

interp2 <- data.frame(
  x = xnew2,
  y = pchip(modeling2$x, modeling2$y, xnew2),
  Model = "100 kyr"
)

interp3 <- data.frame(
  x = xnew3,
  y = pchip(modeling3$x, modeling3$y, xnew3),
  Model = "50 kyr"
)

# 合并
interp_all <- bind_rows(interp1, interp2, interp3)

desired_order <- c(
  "50 kyr",
  "100 kyr",
  "150 kyr"
)

interp_all$Model <- factor(interp_all$Model, levels = desired_order)

# 原始数据合并
combined_data <- bind_rows(modeling1, modeling2, modeling3)
combined_data$Model <- factor(combined_data$Model, levels = desired_order)

p <- ggplot(interp_all, aes(x = x, y = y, color = Model, linetype = Model)) +
  geom_line(linewidth = 1) +
  annotate("rect", xmin = 0, xmax = 2500, ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.3, color = NA) +
  annotate("text", x = 1300, y = 870, label = "Deformed aureole", 
           angle = 0, color = "gray40", size = 5, fontface = "italic") +
  scale_color_manual(values = c("150 kyr" = "lightblue2",
                                "100 kyr" = "deepskyblue3",
                                "50 kyr" = "blue4"),
                     guide = guide_legend(reverse = FALSE)) +
  scale_linetype_manual(values = rep("solid", 3),
                        guide = guide_legend(reverse = FALSE)) +
  labs(# title = "Simulated temperature profiles across the pluton margin",
    x = "Distance from Intrusion Margin [m]",
    y = "Modeled Peak Temperature [°C]",
       color = "Emplacement duration",
       linetype = "Emplacement duration") +

  # 100-10000 m
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

ggsave("~/Downloads/Figure_3.png", plot = p, width = 8, height = 6, dpi = 300)

