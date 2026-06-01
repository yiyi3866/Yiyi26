# 加载必要的库
library(ggplot2)
library(dplyr)

# 设置常数
Rp <- 3000  
tGrowth <- 100000  
a <- Rp * tGrowth^(-1/3)
Ld <- 1000  
z_range <- seq(0, 3000, by = 1)

# 定义函数v
v_function <- function(z, t, Ld) {
  Vd <- 1/3*a*t^(-2/3)
  zb <- a * t^(1/3)
  if (z < zb) {
    return(Vd)
  } else if (z > zb + Ld) {
    return(0)
  } else {
    # return(Vd * zb / Ld - (Vd / Ld) * z + Vd) # linear
    return(1/3*a^3/z^2) # inverse
  }
}

# 生成数据
t_values <- c(100, 1000, 10000)  
data <- expand.grid(z = z_range, t = t_values)  
data$v <- mapply(v_function, data$z, data$t, MoreArgs = list(Ld = Ld))

# corner 点数据
corner_data <- data.frame(
  t = t_values,
  zb = a * t_values^(1/3),
  Vd = 1/3 * a * t_values^(-2/3)
)
# 终点数据 (速度为0的点)
end_data <- data.frame(
  t = t_values,
  zb_end = a * t_values^(1/3) + Ld,   # z = zb + Ld
  v_end = 0
)

# 在图上加点和标签
p <- ggplot(data, aes(x = z, y = v, color = factor(t))) +
  annotate("text", 
           x = 2900, y = 0.15, 
           label = "Emplacement duration: 100 kyr",
           hjust = 1, size = 6, color = "gray40",
           fontface = "italic") +
  geom_line(linewidth = 1.0, linetype = "longdash") +
  geom_point(data = corner_data, aes(x = zb, y = Vd, color = factor(t)), 
             inherit.aes = FALSE, size = 2) +
  geom_point(data = end_data, aes(x = zb_end, y = v_end, color = factor(t)),
             inherit.aes = FALSE, size = 2, shape = 17) +   
  labs(# title = "Velocity Distribution Across Intrusion Margin Over Time",
    x = "Radial Distance from Intrusion Center [m]", 
    y = "IDW Velocity [m/yr]",
    color = "Time since emplacement onset") +
  scale_color_manual(
    values = c("100" = "blue4", "1000" = "deepskyblue3", "10000" = "lightblue2"),
    labels = c("0.1 kyr", "1 kyr", "10 kyr")
    ) +
  scale_x_continuous(breaks = seq(0, 3000, 500), limits = c(0, 3000), expand = c(0, 0)) +
  
  theme_minimal() +
  theme(
    plot.margin = margin(10, 20, 10, 10),  # 上右下左，单位是pt
    
    axis.title = element_text(size = 20,face = "bold"),
    axis.text = element_text(size = 18),
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
    legend.position = c(1.0, 1.0),          
    legend.justification = c("right", "top"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16),
    legend.key.height = unit(0.9, "cm"),   
    legend.key.width = unit(1.8, "cm"),
    legend.background = element_rect(fill = "white", color = "black", size = 0.6),
    panel.grid.major = element_blank(), # 主网格线虚线
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

print(p)

# 保存图像
ggsave("~/Downloads/velocitySnapshotsIDW.png", plot = p, width = 8, height = 5, dpi = 300)
