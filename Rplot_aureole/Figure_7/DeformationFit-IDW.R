library(dplyr)
library(ggplot2)


### Brun et al. (1990) 的观测数据
obs_Brun_z <- c(16.67, 93.33, 280.00, 513.33)
obs_Brun_rate <- c(0.82, 0.72, 0.54, 0.50)
obs_Brun_z_sd <- c(16.67, 24.00, 30.00, 34.00)

obs_Brun <- data.frame(
  Source = "Brun et al. (1990)",
  Distance = obs_Brun_z,
  shortening = obs_Brun_rate,
  DistanceError = obs_Brun_z_sd
)


dykes_z <- c(30.00, 110.00, 110.00, 110.00, 110.00, 210.00, 430.00)
dykes_rate <- c(0.694, 0.820, 0.658, 0.808, 0.714, 0.529, 0.426)
dykes_z_sd <- c(19.15, 25.64, 25.64, 25.64, 25.64, 28.87, 32.45)
dykes <- data.frame(
  Source = "dykes",
  Distance = dykes_z,
  shortening = dykes_rate,
  DistanceError = dykes_z_sd
)

folds_z <- c(30.00, 110.00, 130.00, 140.00, 140.00, 400.00)
folds_rate <- c(0.778, 0.747, 0.657, 0.711, 0.803, 0.451)
folds_z_sd <- c(19.15, 25.64, 26.47, 26.84, 26.84, 32.09)
folds <- data.frame(
  Source = "folds",
  Distance = folds_z,
  shortening = folds_rate,
  DistanceError = folds_z_sd
)


obs_Our <- rbind(dykes, folds)
obs_all <- rbind(obs_Brun, obs_Our)

###  合并两组数据（Brun 在前）
obs_all <- rbind(obs_Brun, obs_Our)

### 按距离升序排序
obs_all <- obs_all[order(obs_all$Distance), ]
print(obs_all)

# All data
obs_z <- obs_all$Distance
obs_rate <- obs_all$shortening
obs_z_sd <- obs_all$DistanceError


Rp <- 3000

# 通用RSS计算函数，传入model参数
calc_rss <- function(lateral_movement, Ld, model = c("inverse")) {
  model <- match.arg(model)
  z_Rp <- obs_z + lateral_movement
  
    pred_rate <- case_when(
      z_Rp < ((Ld^3 + Rp^3)^(1/3)) - Rp ~ 1 - (z_Rp^3 + 3*z_Rp^2*Rp + 3*z_Rp*Rp^2)^(2/3)*(z_Rp + Rp)^(-2),
      z_Rp >= ((Ld^3 + Rp^3)^(1/3)) - Rp & z_Rp < Ld ~ 1 - ((Ld*(4*z_Rp^3 + 12*z_Rp^2*Rp + 12*z_Rp*Rp^2 - Ld^3))/(3*(z_Rp+Rp)^4))^(1/2),
      z_Rp >= Ld ~ 0
    )

  
  if (any(is.nan(pred_rate)) || any(is.infinite(pred_rate))) {
    return(Inf)
  }
  
  sum((pred_rate - obs_rate)^2)
}

# 一个函数：跑一个模型，返回best参数和RMSE
fit_model <- function(model) {
  results <- expand.grid(lateral_movement = 0:800, Ld = 1000:4000)
  results$rss <- mapply(calc_rss, results$lateral_movement, results$Ld, MoreArgs = list(model = model))
  
  best <- results[which.min(results$rss), ]
  z_Rp <- obs_z + best$lateral_movement
  
  # 用最佳参数计算预测值

    pred_rate <- case_when(
      z_Rp < ((best$Ld^3 + Rp^3)^(1/3)) - Rp ~ 1 - (z_Rp^3 + 3*z_Rp^2*Rp + 3*z_Rp*Rp^2)^(2/3)*(z_Rp + Rp)^(-2),
      z_Rp >= ((best$Ld^3 + Rp^3)^(1/3)) - Rp & z_Rp < best$Ld ~ 1 - ((best$Ld*(4*z_Rp^3 + 12*z_Rp^2*Rp + 12*z_Rp*Rp^2 - best$Ld^3))/(3*(z_Rp+Rp)^4))^(1/2),
      z_Rp >= best$Ld ~ 0
    )
  
  rmse <- sqrt(mean((pred_rate - obs_rate)^2))
  
  list(best = best, z_Rp = z_Rp, pred_rate = pred_rate, rmse = rmse)
}

# 拟合inverse
fit_inverse <- fit_model("inverse")

# output
cat("Inverse best lateral_movement:", fit_inverse$best$lateral_movement, "\n")
cat("Inverse best Ld:", fit_inverse$best$Ld, "\n")
cat("Inverse best RMSE:", sprintf("%.10f", fit_inverse$rmse), "\n")
fit_inverse$pred_rate


### 绘图
# —— 显示精度 ——
options(digits = 10)

# 一个通用预测函数：给定原始x(=obs_z)，以及best参数和模型，返回预测的rate
predict_rate <- function(x_orig, best, model = c("inverse")) {
  model <- match.arg(model)
  z_Rp <- x_orig + best$lateral_movement  # 注意：这里把best的lateral_movement加进来

    pred <- dplyr::case_when(
      z_Rp < ((best$Ld^3 + Rp^3)^(1/3)) - Rp ~ 1 - (z_Rp^3 + 3*z_Rp^2*Rp + 3*z_Rp*Rp^2)^(2/3)*(z_Rp + Rp)^(-2),
      z_Rp >= ((best$Ld^3 + Rp^3)^(1/3)) - Rp & z_Rp < best$Ld ~ 1 - ((best$Ld*(4*z_Rp^3 + 12*z_Rp^2*Rp + 12*z_Rp*Rp^2 - best$Ld^3))/(3*(z_Rp+Rp)^4))^(1/2),
      z_Rp >= best$Ld ~ 0
    )

  pred
}

# —— 1) 四组“点”：obs_Brun、obs_Our、inverse点（x轴都用原始 obs_z）——
obs_Brun_df <- data.frame(
  x = obs_Brun_z,
  y = obs_Brun_rate,
  x_sd = obs_Brun_z_sd,
  series = "Observation_Brun"
)


dykes_df <- data.frame(
  x = dykes_z,
  y = dykes_rate,
  x_sd = dykes_z_sd,
  series = "dykes"
)

folds_df <- data.frame(
  x = folds_z,
  y = folds_rate,
  x_sd = folds_z_sd,
  series = "folds"
)


inverse_pts  <- predict_rate(obs_z, fit_inverse$best, "inverse")

pts_df <- data.frame(x = obs_z, y = inverse_pts, series = "inverse")

# —— 2) 两条“曲线”：用更密的x网格（x仍是原始坐标），但内部按 x + best$lateral_movement 代入模型 —— 
x_grid <- seq(min(obs_z), max(obs_z), length.out = 400)

curve_inverse <- data.frame(
  x = x_grid,
  y = predict_rate(x_grid, fit_inverse$best, "inverse"),
  series = "inverse"
)


# —— 3) 画图：obs只画点；inverse既画点也画曲线 —— 
library(scales)

# 自动生成 legend 标签（带主副标题）
make_label <- function(main, Ld, lat) {
  bquote(.(main) ~ "\n" ~ phantom() * "(" * "Deforming aureole width:" ~ .(Ld) ~ "m;" ~
           "Systematic radial error:" ~ .(lat) ~ "m" * ")")
}

# —— 图例标签 ——  
labels_map <- c(
  "obs_Brun"     = "  Brun et al. (1990)",
  "dykes"     = "  Dykes in this study",
  "folds"     = "  Folds in this study",
  "inverse" = paste0(
    "  Model fit: IDW Velocity"
  )
)

# —— 作图 ——  
p <- ggplot() +
  geom_point(data = obs_Brun_df, aes(x = x, y = y, color = series), size = 2) +
  geom_errorbarh(
    data = obs_Brun_df,
    aes(xmin = x - x_sd, xmax = x + x_sd, y = y, color = series),
    height = 0.01, linewidth = 0.5
  ) +
  geom_point(data = dykes_df, aes(x = x, y = y, color = series), size = 2) +
  geom_errorbarh(
    data = dykes_df,
    aes(xmin = x - x_sd, xmax = x + x_sd, y = y, color = series),
    height = 0.01, linewidth = 0.5
  ) +
  geom_point(data = folds_df, aes(x = x, y = y, color = series), size = 2) +
  geom_errorbarh(
    data = folds_df,
    aes(xmin = x - x_sd, xmax = x + x_sd, y = y, color = series),
    height = 0.01, linewidth = 0.5
  ) +

  #geom_point(data = pts_df, aes(x = x, y = y, color = series), size = 2) +
  geom_line(data = curve_inverse, aes(x = x, y = y, color = series), linewidth = 1, linetype = "dashed") +
  annotate("text", x = 110, y = 0.6, label = paste0("Aureole width: ", fit_inverse$best$Ld, " m\n", "Systematic radial error: ", fit_inverse$best$lateral_movement, " m"), 
           angle = 0, color = "#0054FF", size = 5.5, fontface = "italic") +
  scale_color_manual(
    values = c(
      "Observation_Brun" = "#CC5500",#"#993D00",
      # "dykes" =  "#FF6E00",
      # "folds" =  "#FFAD65",
      "dykes" =  "red",
      "folds" =  "orange",
      "inverse"          = "#0054FF"
    ),
    labels = c(
      "Observation_Brun" = "  Brun et al. (1990)",
      "dykes" =  "  Dykes in this study",
      "folds" =  "  Folds in this study",
      "inverse"          = "  Model fit: IDW Velocity"
    ),
    breaks = c("Observation_Brun", "dykes", "folds", "inverse")
    
  ) +
  labs(
    # title = "Shortening vs. Distance to Pluton",
    x = "Distance from Intrusion Margin [m]",
    y = "Shortening"
  ) +
  scale_x_continuous(breaks = seq(0, 600, 100), limits = c(0, 600)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.4, 0.85, 0.05),   # 0% 到 100%，间隔 10%
    limits = c(0.4, 0.85),           # 控制显示范围
    expand = c(0.05, 0)
  )+
  theme_bw(base_size = 18) +
  theme(
    plot.margin = margin(10, 10, 10, 10),  # 上右下左，单位是pt
    legend.title = element_blank(),
    legend.position = c(1, 1),   
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.6), color = "black", linewidth = 0.5),
    legend.text = element_text(size = 16, margin = margin(t = 6, b = 6)),  # 👈 加上下 margin
    legend.key.width = unit(1.8, "cm"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 22),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_line(color = "grey80", linetype = "dashed", size = 0), # 主网格线虚线
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
ggsave("~/Downloads/VelocityBestFit.png", plot = p, width = 10, height = 6, dpi = 300)

# —— 同时把最优参数和pred_rate点列出来（可选）——
cat("Inverse best lateral_movement:", fit_inverse$best$lateral_movement, "\n")
cat("Inverse best Ld:", fit_inverse$best$Ld, "\n")
cat("Inverse pred_rate at obs_z:", paste(sprintf("%.8f", inverse_pts), collapse = ", "), "\n")

# —— 汇总结果表 ——  
results_table <- data.frame(
  Model = rep(c("Inverse"), each = length(obs_z)),
  Distance_m = rep(obs_z, 2),
  Observed_Rate = rep(obs_rate, 2),
  Predicted_Rate = c(fit_inverse$pred_rate),
  Difference = c(fit_inverse$pred_rate - obs_rate),
  Abs_Diff = abs(c(fit_inverse$pred_rate - obs_rate))
)

# 计算每组 RMSE 并添加
rmse_values <- data.frame(
  Model = c("Inverse"),
  RMSE = c(fit_inverse$rmse),
  Ld = c(fit_inverse$best$Ld),
  Lateral_Movement = c(fit_inverse$best$lateral_movement)
)

# 打印查看
print(results_table)
print(rmse_values)

# 保存为 Excel
library(openxlsx)
wb <- createWorkbook()
addWorksheet(wb, "Detailed_Results")
addWorksheet(wb, "Summary")
writeData(wb, "Detailed_Results", results_table)
writeData(wb, "Summary", rmse_values)
saveWorkbook(wb, "~/Downloads/deformation_fit_results.xlsx", overwrite = TRUE)

cat("拟合结果和RMSE保存到 ~/Downloads/deformation_fit_results.xlsx\n")

