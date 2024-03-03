### Regression

library('readxl')
library('lattice')


# Data loading

data_regr <- read_excel("/Users/adamdanielgreen/Desktop/Business Statistics/Regression-20231218/Regression_data.xlsx")

data_regr$Population<-log(data_regr$Population)

data_regr$GDP<-log(data_regr$GDP)


# A summary of the dataset

summary(data_regr)


# Pair-wise relationships of the variables: the scatterplot matrix

splom(~data_regr[c(2:6)], groups=NULL,data=data_regr)


# Estimation of the model

results <- lm(Digital_Index ~ GDP+Population + School + Internet, data_regr)

summary(results)


# Confidence Intervals on the Parameters

confint(results, level = .95)


# Prediction: 95% confidence interval of the Digital Adoption Index for CHAD

GDP <- 23.05
Population <- 16.49
School <- 7
Internet<-5

prediction_info<-data.frame(GDP, Population, School,Internet)



# Prediction: 95% confidence interval on the expected sales

conf_int_Digital <- predict(results, prediction_info, leve1 = .95, interval = "confidence")

conf_int_Digital


# Diagnostics

# Evaluating the Residuals: centered on zero with a constant variance

with(results, {plot(fitted.values, residuals, ylim = c(-40, 40)) 
  points(c(min(fitted.values), max(fitted.values)), c(0, 0), type = "l")})

# Diagnostics

# Evaluating the Normality Assumption of residuals

hist(results$residuals, main="Histogram of residuals")


# Evaluating the Normality Assumption of residuals

qqnorm(results$residuals, ylab="Residuals")
qqline(results$residuals)


# -------------------------------------------------------------------------


## Logistic regression with R

# Load the dataset and have a view
  
Maintenance_data<-read.csv2("/Users/adamdanielgreen/Desktop/Business Statistics/Regression-20231218/Maintenance_data.csv")
  
summary(Maintenance_data)
  
# Correcting importation errors
  
Maintenance_data$Type <- factor(Maintenance_data$Type)
Maintenance_data$Machine_failure <- factor(Maintenance_data$Machine_failure)
summary(Maintenance_data)
  

# The dataset is divided in two: a training set and a test set

Maintenance_data_training <- Maintenance_data[1:(dim(Maintenance_data)[1] - 100), ]
Maintenance_data_test <- Maintenance_data[(dim(Maintenance_data)[1] - 100 + 1):dim(Maintenance_data)[1], ]
  
  
# Model estimation

logit_model <- glm (Machine_failure ~ Type + Air_temperature + Process_temperature +
                      Rotational_speed + Torque + Tool_wear, 
                    data = Maintenance_data_training, binomial(link = "logit"))
summary(logit_model)
  
  
  
# Which variable is the most important?
    
#install.packages("caret")
library("caret")
  
varImp(logit_model)
  

# Diagnostics
  
# Classification Rate: how well the model does in predicting the dependent variable on out-of-sample observations?
    
    
prediction_test <- predict(logit_model, newdata = Maintenance_data_test, type = "response")

prop.table(table(Maintenance_data_test$Machine_failure, prediction_test > 0.5))
  
