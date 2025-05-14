# 加载必要的包 ----
# Load necessary packages ----
library(dplyr)         # 数据处理工具包 | Data manipulation toolkit
library(MASS)          # 用于分布拟合 | For distribution fitting
library(gtsummary)     # 生成美观的统计表格 | Generate nice statistical tables
library(car)           # 包含多种统计检验方法 | Contains various statistical tests
library(lawstat)       # 包含Brunner - Munzel检验 | Contains Brunner - Munzel test
library(DataExplorer)  # 用于探索性数据分析 | For exploratory data analysis

# 设置工作目录和文件路径 ----
# Set working directory and file paths ----
setwd("/Users/haoshilong/Desktop/Practical class 3-20250513")

# 读取数据 ----
# Read data ----
example_df <- read.csv("distribution.csv", header = TRUE, dec = ',', sep = ";")
factor_df <- read.csv("factor_data.csv")
imputed_df <- read.csv("imputed_data.csv")

# 查看数据结构 ----
# Check data structure ----
str(example_df)
str(factor_df)
str(imputed_df)

# 合并数据 ----
# Merge datasets ----
data_for_analysis <- merge(
  factor_df, 
  imputed_df, 
  by = "record_id",        # 合并键 | Merge key
  all = FALSE              # INNER JOIN (仅保留匹配项) | Only keep matching entries
)

# 保存合并后的数据 ----
# Save merged data ----
write.csv(data_for_analysis, "data_for_analysis.csv", row.names = FALSE)  

# 任务1：估计连续变量按组（outcome）的分布 ----
# Task 1: Estimate distributions of continuous variables by group (outcome) ----
# 定义连续变量列表 | Define continuous variables
continuous_vars <- c("lipids1", "lipids2", "lipids3", "lipids4")

# 按outcome分组进行分布拟合 | Fit distributions by outcome group
outcome_groups <- unique(data_for_analysis$outcome)
distribution_results <- list()

for (var in continuous_vars) {
  for (group in outcome_groups) {
    subset_data <- data_for_analysis %>% 
      filter(outcome == group) %>% 
      pull({{var}})
    # 检查子集数据是否为空
    if (length(subset_data) == 0) {
      warning(paste("变量", var, "在组别", group, "下无数据"))
      next
    }
    # 检查数据中是否存在缺失值
    if (any(is.na(subset_data))) {
      warning(paste("变量", var, "在组别", group, "下存在缺失值"))
    }
    # 检查数据是否为单一值
    if (length(unique(subset_data)) == 1) {
      warning(paste("变量", var, "在组别", group, "下数据为单一值"))
    }
    
    # 拟合三种分布 | Fit three distributions
    fit_normal <- try(fitdistr(subset_data, "normal"), silent = TRUE)
    fit_lognormal <- try(fitdistr(subset_data, "lognormal"), silent = TRUE)
    fit_exponential <- try(fitdistr(subset_data, "exponential"), silent = TRUE)
    
    # 计算BIC值 | Calculate BIC values
    bics <- c(
      normal = if(!inherits(fit_normal, "try-error")) BIC(fit_normal) else NA,
      lognormal = if(!inherits(fit_lognormal, "try-error")) BIC(fit_lognormal) else NA,
      exponential = if(!inherits(fit_exponential, "try-error")) BIC(fit_exponential) else NA
    )
    valid_bics <- bics[!is.na(bics)]
    if (length(valid_bics) > 0) {
      best_dist_index <- which.min(valid_bics)
      best_dist_names <- names(valid_bics)
      dist <- best_dist_names[best_dist_index]
    } else {
      dist <- "unknown"
      warning(paste("变量", var, "在组别", group, "下无法确定最佳分布"))
    }
    
    distribution_results[[paste0(var, "_outcome_", group)]] <- dist
    
    cat("变量:", var, "| 组别:", group, "| 最佳分布:", dist, "\n")
    cat("BIC值:", paste(names(bics), round(bics, 2), collapse = ", "), "\n\n")
  }
}

# 任务2：创建描述性统计表格并指定参数 ----
# Task 2: Create descriptive statistics table with distribution parameters ----
tbl_by_outcome <- data_for_analysis %>% 
  select(all_of(c("outcome", continuous_vars))) %>% 
  tbl_summary(
    by = outcome,
    statistic = all_continuous() ~ "{mean} ({sd})"  # 均值(标准差) | Mean(SD)
  ) %>% 
  add_p()  # 添加p值 | Add p-values

# 根据分布结果添加参数说明 | Add distribution parameters to the table
for (var in continuous_vars) {
  for (group in outcome_groups) {
    dist <- distribution_results[[paste0(var, "_outcome_", group)]]
    if (!is.null(dist) && dist != "") {
      subset_data <- data_for_analysis %>% 
        filter(outcome == group) %>% 
        pull({{var}})
      if (dist == "normal") {
        fit <- fitdistr(subset_data, "normal")
        params <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                         ", σ = ", round(fit$estimate["sd"], 2))
        params_en <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                            ", σ = ", round(fit$estimate["sd"], 2))
      } else if (dist == "lognormal") {
        fit <- fitdistr(subset_data, "lognormal")
        params <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                         ", σ = ", round(fit$estimate["sdlog"], 2))
        params_en <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                            ", σ = ", round(fit$estimate["sdlog"], 2))
      } else if (dist == "exponential") {
        fit <- fitdistr(subset_data, "exponential")
        params <- paste0("λ = ", round(fit$estimate["rate"], 2))
        params_en <- paste0("λ = ", round(fit$estimate["rate"], 2))
      }
      # 添加参数到表格 | Add parameters to the table
      tbl_by_outcome <- tbl_by_outcome %>% 
        modify_footnote(
          all_stat_cols() ~ paste0("分布: ", dist, "; 参数: ", params, 
                                   "\nDistribution: ", dist, "; Parameters: ", params_en)
        )
    } else {
      warning(paste("变量", var, "在组别", group, "下的dist值为空，未添加参数到表格"))
    }
  }
}

# 保存表格 | Save the table
tbl_by_outcome %>% 
  as_gt() %>% 
  gt::gtsave("descriptive_stats_by_outcome.html")

# 任务3：进行Brunner - Munzel检验 ----
# Task 3: Perform Brunner - Munzel test ----
brunner_munzel_results <- list()

for (var in continuous_vars) {
  group1 <- data_for_analysis %>% 
    filter(outcome == 0) %>% 
    pull({{var}})
  
  group2 <- data_for_analysis %>% 
    filter(outcome == 1) %>% 
    pull({{var}})
  
  # 进行Brunner - Munzel检验 | Perform Brunner - Munzel test
  result <- brunner.munzel.test(group1, group2)
  
  # 保存结果 | Save results
  brunner_munzel_results[[var]] <- list(
    statistic = result$statistic,
    p_value = result$p.value
  )
  
  cat("变量:", var, "| Brunner - Munzel检验结果:\n")
  cat("统计量:", round(result$statistic, 3), "\n")
  cat("p值:", format.pval(result$p.value, digits = 3), "\n\n")
}

# 将p值添加到描述性统计表格 | Add p-values to the descriptive table
tbl_by_outcome <- tbl_by_outcome %>% 
  modify_table_body(~(.) %>% dplyr::select(-p.value))
tbl_by_outcome <- tbl_by_outcome %>% 
  modify_header(p.value ~ "**Brunner - Munzel p值**\n**Brunner - Munzel p-value**") %>% 
  add_p(test = all_continuous() ~ "brunner.munzel.test")

# 保存最终表格 | Save the final table
tbl_by_outcome %>% 
  as_gt() %>% 
  gt::gtsave("descriptive_stats_with_bm_test.html")

# 加分项：数据错误查找与修正（假设lipids5存在且有缺失值） ----
# Bonus: Find and correct data errors (assuming lipids5 exists with missing values) ----
if ("lipids5" %in% names(data_for_analysis)) {
  # 检查缺失值 | Check missing values
  missing_count <- sum(is.na(data_for_analysis$lipids5))
  cat("lipids5缺失值数量:", missing_count, "\n")
  
  # 简单均值插补（实际应用中可能需要更复杂的方法） | Simple mean imputation
  if (missing_count > 0) {
    mean_value <- mean(data_for_analysis$lipids5, na.rm = TRUE)
    data_for_analysis$lipids5[is.na(data_for_analysis$lipids5)] <- mean_value
    
    cat("已使用均值", round(mean_value, 2), "插补lipids5的缺失值\n")
    
    # 保存修正后的数据 | Save corrected data
    write.csv(data_for_analysis, "data_for_analysis_imputed.csv", row.names = FALSE)
  }
  
  # 重新执行分析（包括分布拟合、表格和检验） | Re - run analysis with corrected data
  continuous_vars_updated <- c(continuous_vars, "lipids5")
  
  # 重新进行分布拟合 | Re - fit distributions
  distribution_results_updated <- list()
  
  for (var in continuous_vars_updated) {
    for (group in outcome_groups) {
      subset_data <- data_for_analysis %>% 
        filter(outcome == group) %>% 
        pull({{var}})
      # 检查子集数据是否为空
      if (length(subset_data) == 0) {
        warning(paste("变量", var, "在组别", group, "下无数据"))
        next
      }
      # 检查数据中是否存在缺失值
      if (any(is.na(subset_data))) {
        warning(paste("变量", var, "在组别", group, "下存在缺失值"))
      }
      # 检查数据是否为单一值
      if (length(unique(subset_data)) == 1) {
        warning(paste("变量", var, "在组别", group, "下数据为单一值"))
      }
      
      fit_normal <- try(fitdistr(subset_data, "normal"), silent = TRUE)
      fit_lognormal <- try(fitdistr(subset_data, "lognormal"), silent = TRUE)
      fit_exponential <- try(fitdistr(subset_data, "exponential"), silent = TRUE)
      
      bics <- c(
        normal = if(!inherits(fit_normal, "try-error")) BIC(fit_normal) else NA,
        lognormal = if(!inherits(fit_lognormal, "try-error")) BIC(fit_lognormal) else NA,
        exponential = if(!inherits(fit_exponential, "try-error")) BIC(fit_exponential) else NA
      )
      valid_bics <- bics[!is.na(bics)]
      if (length(valid_bics) > 0) {
        best_dist_index <- which.min(valid_bics)
        best_dist_names <- names(valid_bics)
        dist <- best_dist_names[best_dist_index]
      } else {
        dist <- "unknown"
        warning(paste("变量", var, "在组别", group, "下无法确定最佳分布"))
      }
      
      distribution_results_updated[[paste0(var, "_outcome_", group)]] <- dist
      
      cat("修正后 - 变量:", var, "| 组别:", group, "| 最佳分布:", dist, "\n")
    }
  }
  
  # 重新创建描述性统计表格 | Re - create descriptive table
  tbl_by_outcome_updated <- data_for_analysis %>% 
    select(all_of(c("outcome", continuous_vars_updated))) %>% 
    tbl_summary(
      by = outcome,
      statistic = all_continuous() ~ "{mean} ({sd})"
    ) %>% 
    add_p()
  
  # 添加参数说明 | Add parameter descriptions
  for (var in continuous_vars_updated) {
    for (group in outcome_groups) {
      dist <- distribution_results_updated[[paste0(var, "_outcome_", group)]]
      if (!is.null(dist) && dist != "") {
        subset_data <- data_for_analysis %>% 
          filter(outcome == group) %>% 
          pull({{var}})
        if (dist == "normal") {
          fit <- fitdistr(subset_data, "normal")
          params <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                           ", σ = ", round(fit$estimate["sd"], 2))
          params_en <- paste0("μ = ", round(fit$estimate["mean"], 2), 
                              ", σ = ", round(fit$estimate["sd"], 2))
        } else if (dist == "lognormal") {
          fit <- fitdistr(subset_data, "lognormal")
          params <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                           ", σ = ", round(fit$estimate["sdlog"], 2))
          params_en <- paste0("μ = ", round(fit$estimate["meanlog"], 2), 
                              ", σ = ", round(fit$estimate["sdlog"], 2))
        } else if (dist == "exponential") {
          fit <- fitdistr(subset_data, "exponential")
          params <- paste0("λ = ", round(fit$estimate["rate"], 2))
          params_en <- paste0("λ = ", round(fit$estimate["rate"], 2))
        }
        tbl_by_outcome_updated <- tbl_by_outcome_updated %>% 
          modify_footnote(
            all_stat_cols() ~ paste0("分布: ", dist, "; 参数: ", params, 
                                     "\nDistribution: ", dist, "; Parameters: ", params_en)
          )
      } else {
        warning(paste("变量", var, "在组别", group, "下的dist值为空，未添加参数到表格"))
      }
    }
  }
  
  # 重新进行Brunner - Munzel检验 | Re - perform Brunner - Munzel test
  brunner_munzel_results_updated <- list()
  
  for (var in continuous_vars_updated) {
    group1 <- data_for_analysis %>% 
      filter(outcome == 0) %>% 
      pull({{var}})
    
    group2 <- data_for_analysis %>% 
      filter(outcome == 1) %>% 
      pull({{var}})
    
    result <- brunner.munzel.test(group1, group2)
    
    brunner_munzel_results_updated[[var]] <- list(
      statistic = result$statistic,
      p_value = result$p.value
    )
    
    cat("修正后 - 变量:", var, "| Brunner - Munzel检验结果:\n")
    cat("统计量:", round(result$statistic, 3), "\n")
    cat("p值:", format.pval(result$p.value, digits = 3), "\n\n")
  }
  
  # 更新表格并保存 | Update and save the table
  tbl_by_outcome_updated <- tbl_by_outcome_updated %>% 
    modify_header(p.value ~ "**Brunner - Munzel p值**\n**Brunner - Munzel p-value**") %>% 
    add_p(test = all_continuous() ~ "brunner.munzel.test")
  
  tbl_by_outcome_updated %>% 
    as_gt() %>% 
    gt::gtsave("descriptive_stats_updated_with_bm_test.html")
}

# 生成最终报告 ----
# Generate final report ----
cat("\n数据分析完成！\n")
cat("描述性统计表格已保存为: descriptive_stats_by_outcome.html\n")
cat("带Brunner - Munzel检验的表格已保存为: descriptive_stats_with_bm_test.html\n")

if (exists("tbl_by_outcome_updated")) {
  cat("修正后的描述性统计表格已保存为: descriptive_stats_updated_with_bm_test.html\n")
}
}
