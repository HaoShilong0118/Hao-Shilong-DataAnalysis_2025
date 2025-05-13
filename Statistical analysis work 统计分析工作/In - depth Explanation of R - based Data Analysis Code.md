# In - depth Explanation of R - based Data Analysis Code
This code conducts a comprehensive data analysis on the "data<u> </u>for<u>  </u>analysis.csv" dataset, covering key aspects such as data reading, correlation analysis, regression analysis, model selection, and data visualization, which helps to deeply understand data characteristics and variable relationships.

## I. Code Function Overview
1. **Data Processing and Exploration**: Read the dataset and display its structure, providing a basis for subsequent analysis.
2. **Correlation Analysis**: Calculate the correlations between variables and evaluate their significance.
3. **Regression Analysis**: Build multiple regression models to explore different relationships between variables.
4. **Model Selection**: Select the optimal model based on the Bayesian Information Criterion (BIC).
5. **Data Visualization**: Draw charts to visually present variable relationships and model fitting effects.

## II. Detailed Code Analysis
### (I) Loading Necessary R Packages
```r
library(dplyr)
library(coin)
```
 - The `dplyr` package: It provides convenient functions for data processing and transformation, such as data filtering, aggregation, and other operations.
 - The `coin` package: It is used to perform permutation tests, which evaluate the reliability of statistical estimates in correlation analysis.

### (II) Reading and Examining Data
```r
data <- read.csv("data_for_analysis.csv")
cat("Basic information about the data:\n")
str(data)
```
 - The `read.csv()` function reads the CSV - formatted dataset at the specified path.
 - `str(data)` is used to display the structure of the data, including the data types of each variable, the number of observations, and other information, helping analysts to have a preliminary understanding of the data.

### (III) Correlation Analysis
```r
numeric_vars <- names(Filter(is.numeric, data))
if (length(numeric_vars) < 2) {
  stop("At least two numeric variables are required in the dataset for correlation analysis")
}
corr_results <- data.frame(
  variable1 = character(),
  variable2 = character(),
  spearman_corr = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)
```
 - First, use `Filter(is.numeric, data)` to filter out the numeric variables in the dataset and store their variable names in `numeric_vars`.
 - Check the number of numeric variables. If it is less than 2, stop the program because at least two variables are required for correlation analysis.
 - Initialize the `corr_results` data frame to store the results of the correlation analysis, including variable pairs, Spearman correlation coefficients, and significance p - values.

```r
for (i in 1:length(numeric_vars)) {
  for (j in (i + 1):length(numeric_vars)) {
    var1 <- numeric_vars[i]
    var2 <- numeric_vars[j]
    cat(paste0("Analyzing variable pair: ", var1, " vs ", var2, "\n"))
    tryCatch({
      corr <- cor(data[[var1]], data[[var2]], method = "spearman", use = "complete.obs")
      perm_test <- spearman_test(data[[var1]] ~ data[[var2]], 
                                 distribution = approximate(B = 10000))
      p_val <- pvalue(perm_test)
      corr_results <- rbind(corr_results, data.frame(
        variable1 = var1,
        variable2 = var2,
        spearman_corr = corr,
        p_value = p_val
      ))
    }, error = function(e) {
      cat(paste0("Warning: Error occurred while analyzing variable pair ", var1, " and ", var2, ": ", e$message, "\n"))
    })
  }
}
```
 - Traverse all pairs of numeric variables through nested loops.
 - For each pair of variables, calculate the Spearman correlation coefficient `corr`, and use the `spearman_test` in the `coin` package to perform a permutation test. Repeat 10,000 times (`B = 10000`) to evaluate the significance of the correlation and obtain the p - value `p_val`.
 - The `tryCatch` statement is used to capture errors during the analysis process and output error messages, ensuring that the code can continue to run when problems occur.

### (IV) Outputting Correlation Analysis Results
```r
cat("\nCorrelation analysis results:\n")
print(corr_results)
```
 - Print the `corr_results` data frame to display the correlation analysis results of all variable pairs, including correlation coefficients and significance levels, facilitating analysts to view the degree of association between variables.

### (V) Regression Analysis
```r
if (!all(c("lipids1", "lipids2") %in% names(data))) {
  stop("'lipids1' or 'lipids2' variables do not exist in the dataset")
}
if (!all(sapply(data[c("lipids1", "lipids2")], is.numeric))) {
  stop("'lipids1' or 'lipids2' are not numeric variables, unable to perform regression analysis")
}
```
 - Check whether the `lipids1` and `lipids2` variables exist in the dataset and confirm that they are numeric variables. If the conditions are not met, stop the regression analysis.

```r
model_linear <- lm(lipids1 ~ lipids2, data = data)
model_2 <- lm(lipids1 ~ poly(lipids2, 2, raw = TRUE), data = data)
model_3 <- lm(lipids1 ~ poly(lipids2, 3, raw = TRUE), data = data)
model_exp <- lm(log(lipids1) ~ lipids2, data = data)
model_log <- lm(lipids1 ~ log(lipids2), data = data)
```
 - Build the following regression models respectively:
    - The linear regression model `model_linear`: Describes the linear relationship between `lipids1` and `lipids2`.
    - The quadratic polynomial regression model `model_2`: Explores the quadratic polynomial relationship between them.
    - The cubic polynomial regression model `model_3`: Analyzes the cubic polynomial - form relationship.
    - The exponential regression model `model_exp`: Studies the exponential - form dependence.
    - The logarithmic regression model `model_log`: Characterizes the logarithmic - form relationship.

### (VI) Selecting the Best Model
```r
models <- list(model_linear, model_2, model_3, model_exp, model_log)
model_names <- c("Linear Model", "Quadratic Polynomial Model", "Cubic Polynomial Model", "Exponential Model", "Logarithmic Model")
bic_values <- sapply(models, BIC)
best_model_index <- which.min(bic_values)
best_model <- models[[best_model_index]]
```
 - Store the built regression models in the `models` list.
 - Name each model and store them in the `model_names` vector.
 - Use `sapply(models, BIC)` to calculate the Bayesian Information Criterion (BIC) values of each model. The BIC comprehensively considers the goodness - of - fit and complexity of the model. A lower value indicates a better model.
 - Find the index of the model with the minimum BIC value through `which.min(bic_values)`, and then determine the best model `best_model`.

```r
cat("\nModel comparison results (sorted in ascending order of BIC):\n")
model_comparison <- data.frame(
  Model = model_names,
  BIC_Value = bic_values,
  Rank = rank(bic_values)
)
model_comparison <- model_comparison[order(model_comparison$Rank), ]
print(model_comparison)
cat(paste0("\nBest model: ", model_names[best_model_index], " (BIC = ", round(bic_values[best_model_index], 2), ")\n"))
```
 - Create the `model_comparison` data frame, which contains model names, BIC values, and ranking information, and sort it in ascending order of BIC values.
 - Print the `model_comparison` data frame to display the model comparison results.
 - Output the name and BIC value of the best model, providing a basis for model selection.

### (VII) Visualizing Variable Relationships (Bonus)
```r
if (!dir.exists("plots")) {
  dir.create("plots")
}
linear_plot_name <- "plots/Lipids1_Lipids2_Linear_Regression.png"
poly_plot_name <- "plots/Lipids1_Lipids2_Polynomial_Regression.png"
all_models_plot_name <- "plots/Lipids1_Lipids2_All_Models.png"
```
 - Check whether the "plots" directory exists. If not, create it to save visual charts.
 - Set the save paths and file names of three charts, which are used to store the linear regression chart, polynomial regression chart, and all - models comparison chart respectively.

```r
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
```
 - Use the `png()` function to set the save parameters of the linear regression chart, including file name, width, height, and resolution.
 - Draw a scatter plot to display the distribution of data points of `lipids1` and `lipids2`, add a linear regression line, and use the `legend` function to add a legend to label the data points and the linear regression line. Finally, close the graphics device.

```r
png(poly_plot_name, width = 800, height = 600, res = 100)
plot(data$lipids2, data$lipids1, 
     main = "Lipids1 vs Lipids2 (Polynomial Regression)",
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16,
     col = rgb(0, 0, 1, 0.5))
x_range <- seq(min(data$lipids2), max(data$lipids2), length.out = 100)
lines(x_range, predict(model_2, newdata = data.frame(lipids2 = x_range)), 
      col = "blue", lwd = 2, lty = 1)
lines(x_range, predict(model_3, newdata = data.frame(lipids2 = x_range)), 
      col = "green", lwd = 2, lty = 2)
legend("topleft", legend = c("Data Points", "Quadratic Polynomial", "Cubic Polynomial"), 
       col = c(rgb(0, 0, 1, 0.5), "blue", "green"), 
       pch = c(16, NA, NA), lty = c(NA, 1, 2), lwd = c(NA, 2, 2))
dev.off()
```
 - Similarly, set the save parameters of the polynomial regression chart.
 - After drawing the scatter plot, generate the value range of `lipids2` as `x_range`, and predict the corresponding `lipids1` values according to the quadratic polynomial model `model_2` and the cubic polynomial model `model_3` to draw the fitting curves. Add a legend to distinguish the data points, quadratic polynomial curve, and cubic polynomial curve. Finally, close the graphics device.

```r
png(all_models_plot_name, width = 1000, height = 800, res = 100)
par(mfrow = c(2, 3))
plot(data$lipids2, data$lipids1, 
     main = "Original Data", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
plot(data$lipids2, data$lipids1, 
     main = "Linear Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, predict(model_linear, newdata = data.frame(lipids2 = x_range)), 
      col = "red", lwd = 2)
plot(data$lipids2, data$lipids1, 
     main = "Quadratic Polynomial Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, predict(model_2, newdata = data.frame(lipids2 = x_range)), 
      col = "blue", lwd = 2)
plot(data$lipids2, data$lipids1, 
     main = "Cubic Polynomial Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, predict(model_3, newdata = data.frame(lipids2 = x_range)), 
      col = "green", lwd = 2)
plot(data$lipids2, data$lipids1, 
     main = "Exponential Model", 
     xlab = "Lipids2", 
     ylab = "Lipids1",
     pch = 16, 
     col = rgb(0, 0, 1, 0.5))
lines(x_range, exp(predict(model_exp, newdata = data.frame(lipids2 = x_range))), 
      col = "purple", lwd = 2)
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
```
 - Set the save parameters of the all - models comparison chart.
 - Use `par(mfrow = c(2, 3))` to divide the drawing area into a sub - plot layout of 2 rows and 3 columns.
 - Draw the scatter plot of the original data and the fitting plots based on different regression models in sequence to display the fitting effect of each model on the data. Add corresponding titles and fitting curves to each sub - plot. Finally, close the graphics device and output a prompt message indicating that the charts have been successfully saved.