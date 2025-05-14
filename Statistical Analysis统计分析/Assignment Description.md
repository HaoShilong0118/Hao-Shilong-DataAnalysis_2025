#### Assignment Description

This R script is designed to complete a statistical analysis assignment. The main tasks include:

1. Merging multiple datasets
2. Analyzing the distribution characteristics of continuous variables
3. Creating descriptive statistical tables
4. Performing Brunner-Munzel nonparametric tests
5. Handling errors and missing values in the data

#### Data Description

The project uses three datasets:
1. `distribution.csv` - Contains lipid-related data
2. `factor_data.csv` - Contains categorical variables and outcome variables
3. `imputed_data.csv` - Contains other variables with imputed values

The merged dataset `data_for_analysis.csv` contains all variables required for analysis, including `outcome` (the outcome variable) and several lipid-related continuous variables.

#### R Version and Dependencies

- **R Version**: R 4.0.0 or higher is recommended
- **Required Packages**:
  - `dplyr` - Data manipulation
  - `MASS` - Distribution fitting
  - `gtsummary` - Generating statistical tables
  - `car` - Contains various statistical tests
  - `lawstat` - Contains Brunner-Munzel test
  - `DataExplorer` - Exploratory data analysis

#### Procedures

1. **Data Preparation**:
   - Read three datasets
   - Merge datasets by `record_id`
   - Save the merged dataset

2. **Distribution Analysis**:
   - Group each continuous variable (`lipids1`-`lipids4`) by `outcome`
   - Fit three distributions: normal, lognormal, exponential
   - Calculate BIC values and select the best-fitting distribution
   - Record the best distribution for each variable in each group

3. **Descriptive Statistics**:
   - Create descriptive statistical tables grouped by `outcome`
   - Add parameter descriptions based on the fitted distributions
   - Save tables as HTML files

4. **Brunner-Munzel Test**:
   - Perform Brunner-Munzel test for each continuous variable
   - Record test statistics and p-values
   - Add p-values to the descriptive statistical tables

5. **Data Error Handling (Bonus)**:
   - Check and handle missing values in `lipids5` (if any)
   - Impute missing values with the mean
   - Re-run all analysis steps

#### Key Code Explanations

1. **Distribution Fitting and BIC Calculation**:

   ```r
   fit_normal <- try(fitdistr(subset_data, "normal"), silent = TRUE)
   fit_lognormal <- try(fitdistr(subset_data, "lognormal"), silent = TRUE)
   fit_exponential <- try(fitdistr(subset_data, "exponential"), silent = TRUE)
   
   bics <- c(
     normal = if(!inherits(fit_normal, "try-error")) BIC(fit_normal) else NA,
     lognormal = if(!inherits(fit_lognormal, "try-error")) BIC(fit_lognormal) else NA,
     exponential = if(!inherits(fit_exponential, "try-error")) BIC(fit_exponential) else NA
   )
   
   best_dist <- names(which.min(bics))
   ```

2. **Brunner-Munzel Test**:

   ```r
   result <- brunner.munzel.test(group1, group2)
   ```

3. **Missing Value Handling**:

   ```r
   if ("lipids5" %in% names(data_for_analysis)) {
     missing_count <- sum(is.na(data_for_analysis$lipids5))
     if (missing_count > 0) {
       mean_value <- mean(data_for_analysis$lipids5, na.rm = TRUE)
       data_for_analysis$lipids5[is.na(data_for_analysis$lipids5)] <- mean_value
     }
   }
   ```

#### Output Files

After running the script, the following files will be generated:

1. `data_for_analysis.csv` - Merged dataset

2. `descriptive_stats_by_outcome.html` - Basic descriptive statistics table
3. `descriptive_stats_with_bm_test.html` - Table with Brunner-Munzel test results
4. `descriptive_stats_updated_with_bm_test.html` - Updated table after handling missing values (if applicable)

#### How to Run

1. Ensure all required R packages are installed
2. Set the correct working directory and ensure data files are in the specified path
3. Run the entire R script

