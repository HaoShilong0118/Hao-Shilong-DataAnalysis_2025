# 加载必要的库
# Load necessary libraries
libraries <- c("dplyr", "skimr", "visdat", "naniar", "mice", "ggplot2", "dbscan", "FactoMineR", "factoextra")
for (lib in libraries) {
  if (!require(lib, character.only = TRUE)) {
    install.packages(lib)
    library(lib, character.only = TRUE)
  }
}

# 获取当前工作目录
# Get the current working directory
getwd()

# 创建相对于项目根目录的路径
# Create a path relative to the project root directory
data_path <- "/Users/haoshilong/Downloads/DataSet_No_Details.csv"

# 读取数据集
# Read the dataset
df <- read.csv(data_path)

# 显示数据结构和变量类型
# Display the data structure and variable types
str(df)

# 进行数据概述，包含数值变量的直方图
# Conduct a data overview, including histograms of numeric variables
skim(df) 

# 数据集准备
# Prepare the dataset
cols_to_remove <- c("h_index_34", "h_index_56", "hormone10_1", "hormone10_2", "an_index_23", "outcome", "factor_eth", "factor_h", "factor_pcos", "factor_prl")
MD_df <- df %>% select(-any_of(cols_to_remove))
factor_df <- df %>% select(record_id, outcome, factor_eth, factor_h, factor_pcos, factor_prl)
str(MD_df)
summary(factor_df)

# 识别缺失值
# Identify missing values
total_na <- sum(is.na(MD_df))               
col_na_counts <- colSums(is.na(MD_df))           
skim(MD_df)
na_stats <- colMeans(is.na(MD_df)) * 100 
na_stats_filtered <- na_stats[na_stats <= 35] 
na_stats_filtered_table <- data.frame(
  Column = names(na_stats_filtered),
  NA_Percent = na_stats_filtered,
  row.names = NULL
)
na_stats_filtered_1 <- na_stats[na_stats > 35] 
na_stats_filtered_1_table <- data.frame(
  Column = names(na_stats_filtered_1),
  NA_Percent = na_stats_filtered_1,
  row.names = NULL
)

# 可视化缺失数据模式
# Visualize missing data patterns
vis_miss(MD_df)
gg_miss_var(MD_df)

# 处理缺失数据，删除部分列
# Handle missing data by removing some columns
cols_to_remove1 <- c("hormone9", "hormone11", "hormone12", "hormone13", "hormone14")
handle_MD_df <- MD_df %>% select(-any_of(cols_to_remove1))
str(handle_MD_df)

# 执行Little's MCAR检验
# Perform Little's MCAR test
handle_MD_df_clean <- handle_MD_df %>%
  select(where(~!all(is.na(.)))) %>%
  mutate(across(where(is.character), as.factor))
mcar_result <- mcar_test(handle_MD_df_clean)
print(mcar_result)

# 解释MCAR检验结果
# Interpret the MCAR test results
interpret_mcar <- function(mcar_result) {
  p <- mcar_result$p.value
  if (p > 0.05) {
    message("✅ p-value > 0.05 → Data is likely MCAR. Safe to delete or impute.")
    message("✅ p值大于0.05 → 数据可能是完全随机缺失（MCAR）。可以安全地删除或插补。")
  } else {
    message("❗ p-value <= 0.05 → Data is NOT MCAR. Assume MAR or MNAR.")
    message("❗ p值小于等于0.05 → 数据不是完全随机缺失（MCAR）。假设是随机缺失（MAR）或非随机缺失（MNAR）。")
    message("➡️ 建议使用多重插补（mice）方法，如 pmm / rf 等。")
    message("➡️ It is recommended to use the multiple imputation (mice) method, such as pmm / rf, etc.")
  }
}
interpret_mcar(mcar_result)

# 多重插补 - PMM方法
# Multiple imputation - PMM method
imputed_pmm <- mice(handle_MD_df[,!names(handle_MD_df) %in% "New"], method = "pmm")
imputed_pmm_final <- complete(imputed_pmm)

# 多重插补 - RF方法
# Multiple imputation - RF method
imputed_rf <- mice(handle_MD_df[,!names(handle_MD_df) %in% "New"], method = "rf")
imputed_rf_final <- complete(imputed_rf)

# 定义函数比较原始数据和插补后数据的密度
# Define a function to compare the density of original data and imputed data
compare_density <- function(var, original_data, pmm_data, rf_data) {
  original <- original_data %>%
    select(all_of(var)) %>%
    mutate(source = "Original")
  pmm <- pmm_data %>%
    select(all_of(var)) %>%
    mutate(source = "PMM")
  rf <- rf_data %>%
    select(all_of(var)) %>%
    mutate(source = "RF")
  combined <- bind_rows(original, pmm, rf)
  
  if (var == "hormone10_generated") {
    x_lim <- c(0, 5)
  } else {
    x_min <- min(combined[[var]], na.rm = TRUE)
    x_max <- max(combined[[var]], na.rm = TRUE)
    x_range <- x_max - x_min
    buffer <- x_range * 0.05
    x_lim <- c(x_min - buffer, x_max - buffer)
  }
  
  ggplot(combined, aes_string(x = var, fill = "source", color = "source")) +
    geom_density(alpha = 0.4, size = 1, na.rm = TRUE) +
    labs(title = paste("Density Comparison of:", var),
         x = var,
         y = "Density") +
    scale_x_continuous(limits = x_lim) +
    scale_fill_manual(values = c(
      "Original" = "black",
      "PMM" = "#E69F00",
      "RF" = "#56B4E9"
    )) +
    scale_color_manual(values = c(
      "Original" = "black",
      "PMM" = "#E69F00",
      "RF" = "#56B4E9"
    )) +
    theme_minimal() +
    theme(legend.position = "top")
}

# 自动提取需要插值的变量名（有缺失的数值变量）
# Automatically extract the names of variables that need imputation (numeric variables with missing values)
vars_to_plot <- handle_MD_df %>%
  select(where(is.numeric)) %>%
  select(where(~ any(is.na(.)))) %>%
  names()

# 对每个变量绘制密度比较图并保存
# Plot density comparison graphs for each variable and save them
for (v in vars_to_plot) {
  plot <- compare_density(v, handle_MD_df, imputed_pmm_final, imputed_rf_final)
  print(plot)
  ggsave(filename = paste0("/Users/haoshilong/Desktop/Graphs/imputation_density_", v, ".png"), 
         plot = plot, width = 6, height = 4)
}

# 异常值检测
# Outlier detection
# 选择特定列并重塑数据用于异常值检测
# Select specific columns and reshape the data for outlier detection
library(tidyr)
library(dplyr)
outliers_data <- imputed_rf_final %>%
  select(lipids1, lipids2, lipids3, lipids4, lipids5) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")
# 生成箱线图检测异常值
# Generate boxplots to detect outliers
boxplot1 <- ggplot(outliers_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Outlier Detection",
       x = "variables",
       y = "value") +
  theme_minimal()
print(boxplot1)
ggsave(filename = "/Users/haoshilong/Desktop/Graphs/outlier_detection_boxplot.png", 
       plot = boxplot1, width = 6, height = 4)

# 对所有数值变量生成箱线图检测异常值
# Generate boxplots for all numeric variables to detect outliers
boxplot2 <- imputed_rf_final %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Boxplots for Outlier Detection")
print(boxplot2)
ggsave(filename = "/Users/haoshilong/Desktop/Graphs/all_numeric_outlier_boxplot.png", 
       plot = boxplot2, width = 8, height = 6)

# 计算LOF因子
# Calculate LOF factors
lof_data <- imputed_rf_final %>%
  select(where(is.numeric)) %>%
  na.omit()
lof_scores <- lof(lof_data, k = 20)
lof_df <- lof_data %>%
  mutate(lof_score = lof_scores)

# 可视化LOF因子
# Visualize LOF factors
# 直方图
# Histogram
lof_hist <- ggplot(lof_df, aes(x = lof_score)) +
  geom_histogram(
    binwidth = 0.05,
    fill = "#FF7F00",
    color = "black",
    alpha = 0.7
  ) +
  labs(
    title = "Histogram of  LOF Scores",
    x = "LOF Score",
    y = "Frequency"
  ) +
  theme_minimal()
print(lof_hist)
ggsave(filename = "/Users/haoshilong/Desktop/Graphs/lof_histogram.png", 
       plot = lof_hist, width = 6, height = 4)

# 散点图（以lipids1和lipids2为例展示LOF得分分布）
# Scatter plot (show the distribution of LOF scores using lipids1 and lipids2 as an example)
lof_scatter <- ggplot(lof_df, aes(x = lipids1, y = lipids2)) +
  geom_point(aes(color = lof_score), size = 2, alpha = 0.8) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "LOF-based Outlier Detection",
       x = "lipids1",
       y = "lipids2",
       color = "LOF Score") +
  theme_minimal()
print(lof_scatter)
ggsave(filename = "/Users/haoshilong/Desktop/Graphs/lof_scatterplot.png", 
       plot = lof_scatter, width = 6, height = 4)

# 确定异常值（如LOF得分前5%为异常值）
# Identify outliers (e.g., the top 5% of LOF scores are considered outliers)
threshold <- quantile(lof_scores, 0.95)
lof_df <- lof_df %>%
  mutate(is_outlier = lof_score > threshold)
# 高亮异常值的散点图
# Scatter plot highlighting outliers
outlier_scatter <- ggplot(lof_df, aes(x = lipids1, y = lipids2)) +
  geom_point(aes(color = is_outlier), size = 2, alpha = 0.7) +
  scale_color_manual(values = c("FALSE" = "grey", "TRUE" = "red")) +
  labs(title = "LOF Outliers (Top 5%)",
       color = "Outlier") +
  theme_minimal()
print(outlier_scatter)
ggsave(filename = "/Users/haoshilong/Desktop/Graphs/lof_outliers_scatterplot.png", 
       plot = outlier_scatter, width = 6, height = 4)
