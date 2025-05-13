#--------------------start-------------------------------
# Get current working directory
getwd()

# 安装并加载必要的包
#install.packages("PMCMRplus")
#install.packages("coin")
library(PMCMRplus)
library(coin)  # 加载包含perm.relation函数的包
library(dplyr) # 用于数据处理

#----------------read dataset--------------------------
data <- read.csv("/Users/haoshilong/Desktop/Practice 4-20250513/data_for_analysis.csv")
summary(data)

# 检查数据中的重复值
cat("Lipids1 重复值数量:", sum(duplicated(data$lipids1)), "\n")
cat("Lipids2 重复值数量:", sum(duplicated(data$lipids2)), "\n")

# testing for normality of distribution
shapiro.test(data$lipids1)
shapiro.test(data$lipids2)

# 可视化分布
par(mfrow=c(1,2))
hist(data$lipids1, main = "Lipids1的直方图（Histogram of Lipids1）")  
qqnorm(data$lipids1)
qqline(data$lipids1)

# Spearman's correlation test (处理重复值的警告)
spearman_result <- cor.test(data$lipids1, data$lipids2, method="spearman", exact = FALSE)
print(spearman_result)

# data.frame for result
results <- data.frame(
  variable = character(),
  spearman_corr = numeric(),
  s_p_value = numeric(),
  stringsAsFactors = FALSE
)

# variables for analysis
target_vars <- c("lipids2", "lipids3", "lipids4")

# main 
for (var in target_vars) {
  # 使用coin包的spearman_test进行置换检验
  perm_spearman <- spearman_test(
    data$lipids1 ~ data[[var]],
    distribution = approximate(B = 10000)  # 执行10000次置换
  )
  
  # add result
  results <- rbind(results, data.frame(
    variable = var,
    spearman_corr = cor(data$lipids1, data[[var]], method = "spearman"),
    s_p_value = pvalue(perm_spearman)
  ))
}

# output result
print(results)

#------visualization of significant results of correlation analysis---------
data <- data[order(data$lipids1),]

# 绘制散点图并添加回归线
plot(data$lipids1, data$lipids2, 
     main = "Lipids1与Lipids2的关系（Relationship between Lipids1 and Lipids2）",
     xlab = "Lipids1",
     ylab = "Lipids2",
     pch = 16,
     col = rgb(0,0,1,0.5))

# 添加平滑曲线
lines(data$lipids1, predict(lm(lipids2 ~ lipids1, data = data)), col = "red", lwd = 2)

# 添加低ess平滑曲线
lines(data$lipids1, loess(lipids2 ~ lipids1, data = data)$fitted, col = "blue", lwd = 2, lty = 2)


#_____________regression analysis________________ 
df <- data
df <- df[order(df$lipids1),]

#linear regression
model_linear <- lm(lipids1 ~ lipids2, data = df)
summary(model_linear)

#second degree polynomial
model_2 <- lm(lipids1 ~ poly(lipids2, 2, raw = TRUE), data = df)
summary(model_2)

#third degree polynomial
model_3 <- lm(lipids1 ~ poly(lipids2, 3, raw = TRUE), data = df)
summary(model_3)

#exponential dependence (修正为正确的指数模型)
model_exp <- lm(lipids1 ~ exp(lipids2), data = df)
summary(model_exp)

# log dependence (修正为正确的对数模型)
model_log <- lm(lipids1 ~ log(lipids2), data = df)
summary(model_log)

#comparison of models
#table of result
rezult <- data.frame(
  model = c("model_linear", "model_2", "model_3", "model_exp", "model_log"),
  BIC_value = c(BIC(model_linear), BIC(model_2), BIC(model_3), BIC(model_exp), BIC(model_log)),
  R_squared = c(summary(model_linear)$r.squared, 
                summary(model_2)$r.squared, 
                summary(model_3)$r.squared, 
                summary(model_exp)$r.squared, 
                summary(model_log)$r.squared)
)

rezult <- rezult[order(rezult$BIC_value),]
print(rezult)

# __________building graphs______________
# 绘制所有模型的预测曲线
par(mfrow=c(2,3))

# 原始数据散点图
plot(df$lipids2, df$lipids1, 
     main = "原始数据（Original Data）", 
     pch = 16, 
     col = rgb(0,0,1,0.5))

# 线性模型
plot(df$lipids2, df$lipids1, 
     main = "线性模型（Linear Model）", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_linear), col = "red", lwd = 2)

# 二次多项式模型
plot(df$lipids2, df$lipids1, 
     main = "二次多项式（Quadratic Polynomial）", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_2), col = "blue", lwd = 2)

# 三次多项式模型
plot(df$lipids2, df$lipids1, 
     main = "三次多项式（Cubic Polynomial）", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_3), col = "green", lwd = 2)

# 指数模型
plot(df$lipids2, df$lipids1, 
     main = "指数模型（Exponential Model）", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_exp), col = "purple", lwd = 2)

# 对数模型
plot(df$lipids2, df$lipids1, 
     main = "对数模型（Logarithmic Model）", 
     pch = 16, 
     col = rgb(0,0,1,0.5))
lines(df$lipids2, predict(model_log), col = "orange", lwd = 2)

