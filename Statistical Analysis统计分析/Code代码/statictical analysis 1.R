#--------------------start-------------------------------
# Get current working directory
getwd()
#----------------read dataset--------------------------
example_df<-read.csv("/Users/haoshilong/Desktop/Practical class 3-20250513/distribution.csv", header = TRUE,dec = ',', sep = ";")
factor_df <- read.csv("/Users/haoshilong/Desktop/Practical class 3-20250513/factor_data.csv")
imputed_df <- read.csv("/Users/haoshilong/Desktop/Practical class 3-20250513/imputed_data.csv")
# Display structure with variable types
str(example_df)
str(factor_df)
str(imputed_df)
#---------------merge two files-------------------------
data_for_analysis <- merge(
  factor_df, 
  imputed_df, 
  by = "record_id",        # column for merge
  all = FALSE       # FALSE = INNER JOIN (only coincidences), TRUE = FULL JOIN
)
str(data_for_analysis)

# save data_for_analysis in CSV
write.csv(data_for_analysis, "data_for_analysis.csv", row.names = FALSE)  

#------------------Probability Distributions----------------------- 
install.packages("MASS", dependencies=T)
library(MASS)
#----------------example-----------------------------------------
summary (example_df)
example_df$value <- as.numeric(example_df$value)
summary(example_df)
#building histograms for example
# normal distribution
val<-example_df[example_df$distribution=="norm",]$value

mean(val)

sd(val)

hist(val)

fit<-fitdistr(val, densfun="normal")

fit
#lognormal distribution
val<-example_df[example_df$distribution=="lognorm",]$value

mean(val)

sd(val)

hist(val)

fit<-fitdistr(val, densfun="lognormal")

fit

unname(fit$estimate[1])

unname(fit$estimate[2])

m_log<-exp(unname(fit$estimate[1]))*sqrt(exp(unname(fit$estimate[2])^2))
m_log
sd_log<-sqrt(exp(2*unname(fit$estimate[1]))*(exp(unname(fit$estimate[2])^2)-1)*sqrt(exp(unname(fit$estimate[2])^2)))
sd_log
#exponential distribution

val<-example_df[example_df$distribution=="exp",]$value

mean(val)

sd(val)

hist(val)

fit<-fitdistr(val, densfun="exponential")

fit

unname(fit$estimate[1])

m_exp<-1/unname(fit$estimate[1])
m_exp
#Poisson distribution
val<-example_df[example_df$distribution=="pois",]$value

mean(val)

sd(val)

hist(val)

fit<-fitdistr(val, densfun="Poisson")

fit

unname(fit$estimate[1])

sd_pois<-sqrt(unname(fit$estimate[1]))
sd_pois
#Selecting a Distribution Model

val<-example_df[example_df$distribution=="lognorm",]$value

fit_1<-fitdistr(val, densfun="normal")
fit_2<-fitdistr(val, densfun="lognormal")
fit_3<-fitdistr(val, densfun="exponential")

#Bayesian Information Criterion calculation
BIC(fit_3)

#calculation of the Bayesian information criterion for all models
BIC_value<-c(BIC(fit_1), BIC(fit_2), BIC(fit_3))

#forming a vector with the name of the models
distribution<-c("normal", "lognormal", "exponential")

#combining the results into a final table
rez<-data.frame(BIC_value=BIC_value, distribution=distribution)

#sort table in ascending order of Bayesian Information Criterion value
rez<-rez[order(rez$BIC_value, decreasing=F),]

rez


#calculation of absolute values of the confidence interval for the mean of a lognormal distribution
error_min<-unname(fit_2$estimate[1])-unname(fit_2$sd[1])
error_max<-unname(fit_2$estimate[1])+unname(fit_2$sd[1])

error_min
error_max

m<-exp(unname(fit_2$estimate[1]))*sqrt(exp(unname(fit_2$estimate[2])^2))
value_error_min<-exp(error_min)*sqrt(exp(unname(fit_2$estimate[2])^2))
value_error_max<-exp(error_max)*sqrt(exp(unname(fit_2$estimate[2])^2))

value_error_min
m
value_error_max

#--------------data for analysis--------------------------
#building histograms
value_d1<-data_for_analysis$lipids1
hist(value_d1)
value_d2<-data_for_analysis$lipids2
hist(value_d2)
value_d3<-data_for_analysis$lipids3
hist(value_d3)
value_d4<-data_for_analysis$lipids4
hist(value_d4)


# d1 distribution estimate


fit_d1_1<-fitdistr(value_d1,densfun="normal")
fit_d1_2<-fitdistr(value_d1,densfun="lognormal")
fit_d1_3<-fitdistr(value_d1,densfun="exponential")

#calculation of the Bayesian information criterion (BIC) and finding of BIC minimum for d1

BIC_value_d1 <- c(BIC(fit_d1_1),BIC(fit_d1_2),BIC(fit_d1_3))
distribution <-c("normal","lognormal","exponential")
result_d1<-data.frame(BIC_value_d1=BIC_value_d1, distribution=distribution)
result_d1
min(result_d1$BIC_value_d1)
distribution_d1<-result_d1[result_d1$BIC_value_d1==min(result_d1$BIC_value_d1),]$distribution
distribution_d1
# Finding parameters for d1
fit_d1_1$estimate[1:2]

# d2 distribution estimate


fit_d2_1<-fitdistr(value_d2,densfun="normal")
fit_d2_2<-fitdistr(value_d2,densfun="lognormal")
fit_d2_3<-fitdistr(value_d2,densfun="exponential")

#calculation of the Bayesian information criterion (BIC) and finding of BIC minimum for d2

BIC_value_d2 <- c(BIC(fit_d2_1),BIC(fit_d2_2),BIC(fit_d2_3))
distribution <-c("normal","lognormal","exponential")
result_d2<-data.frame(BIC_value_d2=BIC_value_d2, distribution=distribution)
result_d2
min(result_d2$BIC_value_d2)
distribution_d2<-result_d2[result_d2$BIC_value_d2==min(result_d2$BIC_value_d2),]$distribution
distribution_d2
# Finding parameters for d2
fit_d2_1$estimate[1:2]

#-----------descriptive statistics------------------
#-----------for publication tables-----------------
install.packages("gtsummary")
library(gtsummary)

tbl_summary(data_for_analysis)  # Automatic table
tbl_summary(data_for_analysis, by = outcome)  # By groups
#---------------Creating a custom table--------------
# Homework: Creating a custom table with descriptive statistics results
#--------------Statistical Tests---------------------
value_outcome1<-data_for_analysis[data_for_analysis$outcome=="1",]$lipids1
hist(value_outcome1)
value_outcome0<-data_for_analysis[data_for_analysis$outcome=="0",]$lipids1
hist(value_outcome0)
#-------Levene's Test for Homogeneity of Variance--------------
install.packages("car")
library(car)
str(data_for_analysis)
data_for_analysis$outcome<- as.factor(data_for_analysis$outcome)
car::leveneTest(lipids1 ~ outcome, data = data_for_analysis)
#---------------Application of the Brunner-Munzel test----------
install.packages("lawstat")
library(lawstat)
group1 <- data_for_analysis$lipids1[data_for_analysis$outcome == "0"]
group2 <- data_for_analysis$lipids1[data_for_analysis$outcome == "1"]

brunner.munzel.test(group1, group2)
#-------------comparison of results with other tests--------------
t.test(group1, group2)
wilcox.test(group1, group2)

#----------------------------EDA----------------------------------
install.packages("DataExplorer")
library(DataExplorer)
create_report(data_for_analysis)  # Generates HTML report with graphs and statistics
create_report(
  data = data_for_analysis,
  output_file = "EDA_Report.html",  
  output_dir = getwd(),                
  report_title = "EDA Report"          
)
