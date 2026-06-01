library(readxl)
library(dplyr)
library(ggplot2)
library(pracma)

# 拉曼数据
# 通用的读取函数，sheet_name作为参数
read_temperature_data <- function(file_path, sheet_name) {
  # 读取Excel文件的特定工作表，跳过标题行
  data <- read_excel(file_path, sheet = sheet_name, skip = 1)
  
  cat("正在读取工作表:", sheet_name, "\n")
  cat("数据列名：\n")
  print(names(data))
  cat("数据维度：", dim(data), "\n")
  
  # 创建四个组的数据框
  groups <- list()
  
  # 组1: 列A,B 对应 x列J
  group1 <- data.frame(
    x = data[[10]],  # 第10列是J列
    y = data[[1]],   # 第1列是A列
    sd = data[[2]],  # 第2列是B列
    group = "T/°C (Aoya et al., 2010)"
  )
  
  # 组2: 列C,D 对应 x列J
  group2 <- data.frame(
    x = data[[10]],  # 第10列是J列
    y = data[[3]],   # 第3列是C列
    sd = data[[4]],  # 第4列是D列
    group = "T/°C (Lahfid et al., 2010)"
  )
  
  # 组3: 列E,F 对应 x列J
  group3 <- data.frame(
    x = data[[10]],  # 第10列是J列
    y = data[[5]],   # 第5列是E列
    sd = data[[6]],  # 第6列是F列
    group = "T/°C (Beyssac et al., 2002)"
  )
  
  # 组4: 列G,H 对应 x列J
  group4 <- data.frame(
    x = data[[10]],  # 第10列是J列
    y = data[[7]],   # 第7列是G列
    sd = data[[8]],  # 第8列是H列
    group = "T/°C (Kouketsu et al., 2014)"
  )
  
  # 合并所有组
  all_groups <- list(group1, group2, group3, group4)
  
  # 移除每个组中的NA值（缺失数据）
  clean_groups <- lapply(all_groups, function(df) {
    df[complete.cases(df), ]
  })
  
  return(clean_groups)
}


# 选择文件
file_path <- "/Users/oo/Desktop/geology phd/科研资料/CYY有关项目/Flamanville/RstudioCodeFig/RamanFit/Raman_temp-Distance/Data-42RSCM-S1L1.xlsx"

# 指定工作表名称
groups <- read_temperature_data(file_path, sheet_name = "plot42")

# 提取四个组
group1 <- groups[[1]]
group2 <- groups[[2]]
group3 <- groups[[3]]
group4 <- groups[[4]]


# 模拟数据
modelingBest <- data.frame(
  x = c(21, 86, 181, 223, 248, 253, 347, 352, 411, 414, 443, 524, 591, 593, 597, 598, 618, 691, 739, 797, 802, 879, 963, 990, 1109, 1134, 1140, 1153, 1206, 1209, 1217, 1316, 1338, 1347, 1434, 1504, 1648, 2738, 3244, 5080, 6495, 7074),
  y = c(869.29,797.02,708.558,676.883,659.97,656.641,599.825,597.172,568.051,566.671,553.894,523.519,502.956,502.403,501.303,501.03,495.633,477.912,467.688,456.534,455.622,442.59,430.093,426.392,411.689,408.891,408.23,406.816,401.302,401,400.199,390.936,389.016,388.248,381.201,376.006,366.478,325.561,316.317,302.623,300.471,300.213),
  group = "Advection-diffusion, Best-fit (130 kyr, 300 °C)"
)

thermalOnly <- data.frame(
  x = c(21, 86, 181, 223, 248, 253, 347, 352, 411, 414, 443, 524, 591, 593, 597, 598, 618, 691, 739, 797, 802, 879, 963, 990, 1109, 1134, 1140, 1153, 1206, 1209, 1217, 1316, 1338, 1347, 1434, 1504, 1648, 2738, 3244, 5080, 6495, 7074),
  y = c(533.111, 482.238, 442.774, 430.368, 423.863, 422.622, 402.828, 401.928, 392.137, 391.678, 387.422, 376.995, 369.691, 369.491, 369.09, 368.991, 367.034, 360.52, 356.714, 352.542, 352.202, 347.33, 342.691, 341.331, 335.974, 334.967, 334.73, 334.227, 332.266, 332.16, 331.877, 328.642, 327.981, 327.717, 325.32, 323.581, 320.458, 308.041, 305.545, 301.618, 300.577, 300.368),
  group = "Diffusion-only (300 °C)"
)

thermalOnlyBest <- data.frame(
  x = c(21, 86, 181, 223, 248, 253, 347, 352, 411, 414, 443, 524, 591, 593, 597, 598, 618, 691, 739, 797, 802, 879, 963, 990, 1109, 1134, 1140, 1153, 1206, 1209, 1217, 1316, 1338, 1347, 1434, 1504, 1648, 2738, 3244, 5080, 6495, 7074),
  y = c(600.116, 557.248, 523.305, 512.601, 506.98, 505.925, 488.81, 488.034, 479.564, 479.168, 475.488, 466.466, 460.146, 459.971, 459.621, 459.534, 457.841, 452.203, 448.905, 445.293, 444.998, 440.78, 436.764, 435.585, 430.949, 430.077, 429.873, 429.436, 427.74, 427.649, 427.405, 424.607, 424.035, 423.807, 421.735, 420.233, 417.536, 406.847, 404.71, 401.218, 400.379, 400.227),
  group = "Diffusion-only, Best-fit (400 °C)"
)

# —— 拉曼数据合并 ——
all_data <- rbind(group1, group2, group3, group4)

xnew <- seq(min(modelingBest$x), max(modelingBest$x), length.out = 400)

lines_df <- rbind(
  data.frame(
    x = xnew,
    y = pchip(modelingBest$x, modelingBest$y, xnew),
    model = "Advection-diffusion, Best-fit (130 kyr, 300 °C)"
  ),
  data.frame(
    x = xnew,
    y = pchip(thermalOnly$x, thermalOnly$y, xnew),
    model = "Diffusion-only (300 °C)"
  ),
  data.frame(
    x = xnew,
    y = pchip(thermalOnlyBest$x, thermalOnlyBest$y, xnew),
    model = "Diffusion-only, Best-fit (400 °C)"
  )
)

# 设置组别的顺序（从上到下：Aoya, Lahfid, Beyssac, Kouketsu）
desired_order <- c(
  "T/°C (Aoya et al., 2010)",
  "T/°C (Lahfid et al., 2010)", 
  "T/°C (Beyssac et al., 2002)",
  "T/°C (Kouketsu et al., 2014)"
)

# 将group列转换为因子，并设置指定的顺序
all_data$group <- factor(all_data$group, levels = desired_order)

# 设置自定义颜色
custom_colors <- c(
  "T/°C (Aoya et al., 2010)" = "red",           # 红色
  "T/°C (Lahfid et al., 2010)" = "orange",  # 橙红色
  "T/°C (Beyssac et al., 2002)" = "#6F6F6F",   # 深灰色
  "T/°C (Kouketsu et al., 2014)" = "#CECECE"  # 浅灰色
)


# —— 画图 ——

  p <- ggplot() +
  
  # 灰色点
  geom_point(data = subset(all_data, group %in% c("T/°C (Beyssac et al., 2002)", "T/°C (Kouketsu et al., 2014)")),
             aes(x = x, y = y, color = group), shape = 17, size = 2) +
  geom_errorbar(data = subset(all_data, group %in% c("T/°C (Beyssac et al., 2002)", "T/°C (Kouketsu et al., 2014)")),
                aes(x = x, ymin = y - sd, ymax = y + sd, color = group), width = 0.02) +
  
  # 彩色点
  geom_point(data = subset(all_data, group %in% c("T/°C (Aoya et al., 2010)", "T/°C (Lahfid et al., 2010)")), 
             aes(x = x, y = y, color = group), size = 2) +
  geom_errorbar(data = subset(all_data, group %in% c("T/°C (Aoya et al., 2010)", "T/°C (Lahfid et al., 2010)")),
                aes(x = x, ymin = y - sd, ymax = y + sd, color = group), width = 0.05) +
  
  # ✅ 模型线（分开画，保证颜色正确 + 图例正常）
  geom_line(
    data = subset(lines_df, model == "Advection-diffusion, Best-fit (130 kyr, 300 °C)"),
    aes(x = x, y = y, linetype = model),
    inherit.aes = FALSE,
    color = "#0054FF",   # 深蓝
    linewidth = 0.8
  ) +
  geom_line(
    data = subset(lines_df, model == "Diffusion-only (300 °C)"),
    aes(x = x, y = y, linetype = model),
    inherit.aes = FALSE,
    color = "#66B2FF",   # 浅蓝
    linewidth = 0.8
  ) +
  geom_line(
    data = subset(lines_df, model == "Diffusion-only, Best-fit (400 °C)"),
    aes(x = x, y = y, linetype = model),
    inherit.aes = FALSE,
    color = "#66B2FF",   # 浅蓝虚线
    linewidth = 0.8
  ) +
  
  # ✅ 线型图例（统一）
  scale_linetype_manual(
    name = "Modeled peak temperature profile",
    values = c(
      "Advection-diffusion, Best-fit (130 kyr, 300 °C)" = "solid",
      "Diffusion-only (300 °C)" = "solid",
      "Diffusion-only, Best-fit (400 °C)" = "dashed"
    )
  ) +
  
  # 背景框
  annotate("rect",
           xmin = 3700, xmax = 7600, ymin = 510, ymax = 890,
           fill = NA, color = "black", linewidth = 0.5) +
  annotate("rect", xmin = 0, xmax = 300, ymin = -Inf, ymax = Inf,
           fill = "#3E3E3E", alpha = 0.15, color = NA) +
  
  # ✅ 用 coord_cartesian（避免 warning）
  coord_cartesian(xlim = c(0, 8000), ylim = c(200, 900)) +
  
  scale_y_continuous(breaks = seq(200, 900, 100), expand = c(0, 0)) +
  scale_x_continuous(breaks = sort(unique(c(seq(0, 8000, 1000), 300))), expand = c(0, 0)) +
  
  # 点的颜色（只保留这一套 color scale）
  scale_color_manual(
    values = custom_colors,
    breaks = desired_order,
    name = "RSCM-derived temperatures \nusing published geothermometers "
  ) +
  
  labs(
    x = "Distance from Intrusion Margin [m]",
    y = "Peak Metamorphic Temperature [°C]"
  ) +
  
    theme_minimal(base_size = 14) +
    theme(
      plot.margin = margin(10, 25, 10, 10),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      legend.position = c(0.95, 0.99),
      legend.justification = c("right", "top"),
      legend.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 16),
      legend.key.height = unit(0.9, "cm"),   
      legend.key.width = unit(1, "cm"),
      legend.spacing.y = unit(0, "cm"),
      axis.text = element_text(size = 20),
      axis.title = element_text(size = 24),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line.x = element_line(color = "black", linewidth = 1),
      axis.line.y = element_line(color = "black", linewidth = 1),
      axis.ticks = element_line(color = "black", linewidth = 1),
      axis.ticks.length = unit(5, "pt")
    )

ggsave("~/Downloads/Figure_9.png", plot = p, width = 12, height = 8, dpi = 300)

