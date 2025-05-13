# Load required packages
library(dplyr)
library(coin)

# Read data
data <- read.csv("data_for_analysis.csv")

# Display data structure and basic information
cat("Basic information about the data:\n")
str(data)

# 1. Correlation analysis (excluding lipids1 and lipids2 which have been analyzed)
# Extract the names of numeric variables (ensure only numeric variables are included)
numeric_vars <- names(Filter(is.numeric, data))

if (length(numeric_vars) < 2) {
  stop("At least two numeric variables are required in the dataset for correlation analysis")
}

# Initialize the result data frame
corr_results <- data.frame(
  variable1 = character(),
  variable2 = character(),
  spearman_corr = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Conduct correlation analysis (add error handling)
cat("\nStarting correlation analysis...\n")

for (i in 1:length(numeric_vars)) {
  for (j in (i + 1):length(numeric_vars)) {
    var1 <- numeric_vars[i]
    var2 <- numeric_vars[j]
    
    # Print the current variable pair (for debugging)
    cat(paste0("Analyzing variable pair: ", var1, " vs ", var2, "\n"))
    
    tryCatch({
      # Calculate the Spearman correlation coefficient
      corr <- cor(data[[var1]], data[[var2]], method = "spearman", use = "complete.obs")
      
      # Evaluate significance using the permutation test
      perm_test <- spearman_test(data[[var1]] ~ data[[var2]], 
                                 distribution = approximate(B = 10000))
      p_val <- pvalue(perm_test)
      
      # Add results to the data frame
      corr_results <- rbind(corr_results, data.frame(
        variable1 = var1,
        variable2 = var2,
        spearman_corr = corr,
        p_value = p_val
      ))
      
    }, error = function(e) {
      # Error handling: log error information
      cat(paste0("Warning: Error occurred while analyzing variable pair ", var1, " and ", var2, ": ", e$message, "\n"))
    })
  }
}

# 2. Output the table of correlation coefficients and significance assessment
cat("\nCorrelation analysis results:\n")
print(corr_results)

# 3. Regression analysis (using lipids1 and lipids2 as an example, similar for other variable combinations)
# Check if target variables exist and are numeric
if (!all(c("lipids1", "lipids2") %in% names(data))) {
  stop("'lipids1' or 'lipids2' variables do not exist in the dataset")
}

if (!all(sapply(data[c("lipids1", "lipids2")], is.numeric))) {
  stop("'lipids1' or 'lipids2' are not numeric variables, unable to perform regression analysis")
}

# Linear regression
model_linear <- lm(lipids1 ~ lipids2, data = data)

# Quadratic polynomial regression
model_2 <- lm(lipids1 ~ poly(lipids2, 2, raw = TRUE), data = data)

# Cubic polynomial regression
model_3 <- lm(lipids1 ~ poly(lipids2, 3, raw = TRUE), data = data)

# Exponential regression (corrected to the correct exponential model form)
model_exp <- lm(log(lipids1) ~ lipids2, data = data)

# Logarithmic regression (corrected to the correct logarithmic model form)
model_log <- lm(lipids1 ~ log(lipids2), data = data)

# 4. Select the best model (based on BIC)
models <- list(model_linear, model_2, model_3, model_exp, model_log)
model_names <- c("Linear Model", "Quadratic Polynomial Model", "Cubic Polynomial Model", "Exponential Model", "Logarithmic Model")
bic_values <- sapply(models, BIC)
best_model_index <- which.min(bic_values)
best_model <- models[[best_model_index]]

cat("\nModel comparison results (sorted in ascending order of BIC):\n")
model_comparison <- data.frame(
  Model = model_names,
  BIC_Value = bic_values,
  Rank = rank(bic_values)
)
model_comparison <- model_comparison[order(model_comparison$Rank), ]
print(model_comparison)

cat(paste0("\nBest model: ", model_names[best_model_index], " (BIC = ", round(bic_values[best_model_index], 2), ")\n"))

# Bonus: Plot the relationship between variables (using lipids1 and lipids2 as an example)
# Create directory to save charts
if (!dir.exists("plots")) {
  dir.create("plots")
}

# Set image save path and file names
linear_plot_name <- "plots/Lipids1_Lipids2_Linear_Regression.png"
poly_plot_name <- "plots/Lipids1_Lipids2_Polynomial_Regression.png"
all_models_plot_name <- "plots/Lipids1_Lipids2_All_Models.png"

# 1. Plot linear regression
png(linear_plot_name, width = 800, height = 600, res = 100)
plot(data$lipids2, data$lipids1, 
     main = "Lipids1 vs Lipids2 (Linear Regression)",
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
abline(model_linear, col = "red", lwd = 2)
legend("topleft", legend = c("Data Points", "Linear Regression"), 
       col = c(rgb(0, 0, 1, 0.5), "red"), 
       pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))
dev.off()

# 2. Plot polynomial regression
png(poly_plot_name, width = 800, height = 600, res = 100)
plot(data$lipids2, data$lipids1, 
     main = "Lipids1 vs Lipids2 (Polynomial Regression)",
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))

# Add quadratic polynomial fitting curve
x_range <- seq(min(data$lipids2), max(data$lipids2), length.out = 100)
lines(x_range, predict(model_2, newdata = data.frame(lipids2 = x_range)), 
      col = "blue", lwd = 2, lty = 1)

# Add cubic polynomial fitting curve
lines(x_range, predict(model_3, newdata = data.frame(lipids2 = x_range)), 
      col = "green", lwd = 2, lty = 2)

legend("topleft", legend = c("Data Points", "Quadratic Polynomial", "Cubic Polynomial"), 
       col = c(rgb(0, 0, 1, 0.5), "blue", "green"), 
       pch = c(16, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2, 2))
dev.off()

# 3. Plot comparison of all models
png(all_models_plot_name, width = 1000, height = 800, res = 100)
par(mfrow = c(2, 3))  # 2 rows and 3 columns chart layout

# Scatter plot of original data
plot(data$lipids2, data$lipids1, 
     main = "Original Data", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))

# Linear model
plot(data$lipids2, data$lipids1, 
     main = "Linear Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, predict(model_linear, newdata = data.frame(lipids2 = x_range)), 
      col = "red", lwd = 2)

# Quadratic polynomial model
plot(data$lipids2, data$lipids1, 
     main = "Quadratic Polynomial Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, predict(model_2, newdata = data.frame(lipids2 = x_range)), 
      col = "blue", lwd = 2)

# Cubic polynomial model
plot(data$lipids2, data$lipids1, 
     main = "Cubic Polynomial Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, predict(model_3, newdata = data.frame(lipids2 = x_range)), 
      col = "green", lwd = 2)

# Exponential model
plot(data$lipids2, data$lipids1, 
     main = "Exponential Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, exp(predict(model_exp, newdata = data.frame(lipids2 = x_range))), 
      col = "purple", lwd = 2)

# Logarithmic model
plot(data$lipids2, data$lipids1, 
     main = "Logarithmic Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, predict(model_log, newdata = data.frame(lipids2 = x_range)), 
      col = "orange", lwd = 2)

dev.off()

cat("\nCharts have been successfully saved in the 'plots' directory\n")