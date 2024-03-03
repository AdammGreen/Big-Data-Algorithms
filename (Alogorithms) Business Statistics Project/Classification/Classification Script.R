### Classification

# Load the dataset and have a view

library(readxl)

Maintenance_data <- read_excel("/Users/adamdanielgreen/Desktop/Business Statistics/Classification-20231218/Maintenance_data.xlsx")

summary(Maintenance_data)

# Correcting importation errors

Maintenance_data$Type<-factor(Maintenance_data$Type)
Maintenance_data$Air_temperature<-as.numeric(Maintenance_data$Air_temperature)
Maintenance_data$Process_temperature<-as.numeric(Maintenance_data$Process_temperature)
Maintenance_data$Torque<-as.numeric(Maintenance_data$Torque)

Maintenance_data$Machine_failure<-factor(Maintenance_data$Machine_failure)
summary(Maintenance_data)


# Packages

#install.packages("rpart.plot")
  
library("rpart") 

library("rpart.plot")


# **rpart** function is used to construct a decision tree

decision_tree <- rpart(Machine_failure~Type+Air_temperature+
                         Process_temperature+Rotational_speed+Torque+Tool_wear, 
                       method="class", data=Maintenance_data, 
                       control=rpart.control(minsplit=1), 
                       parms=list(split="information"))


summary(decision_tree)

rpart.plot(decision_tree, type=2, extra=1)


### Prediction for a new product 

new_product<-data.frame(Type="L", Air_temperature=300.3, 
                        Process_temperature=309.9, Rotational_speed=1394, 
                        Torque=46.7, Tool_wear=210)

new_product

predict(decision_tree,newdata=new_product,type="class")



## Naive Bayes with R: Same example than previously

#install.packages("e1071")

library("e1071") 
  
# Define the data frames for the Naive Bayes classifier

# Discretizing variables
#using mean values as numbers
  
Maintenance_data$Air_temperature2 <- ifelse(Maintenance_data$Air_temperature < 299, "<299", ">299")
Maintenance_data$Process_temperature2 <- ifelse(Maintenance_data$Process_temperature < 309, "<309", ">309")
Maintenance_data$Rotational_speed2 <- ifelse(Maintenance_data$Rotational_speed < 1524, "<1524", ">1524")
Maintenance_data$Torque2 <- ifelse(Maintenance_data$Torque < 43, "<43", ">43")
Maintenance_data$Tool_wear2 <- ifelse(Maintenance_data$Tool_wear < 120, "<120", ">120")

## Naive Bayes with R: Same example than previously

# Define the testing and the training sets

training_data <- as.data.frame(Maintenance_data[1:dim(Maintenance_data)[1] - 1, ]) 
test_data <- as.data.frame(Maintenance_data[dim(Maintenance_data)[1], ])

test_data


## Naive Bayes with R: Same example than previously

# The package "e1071" in R has a built-in naiveBayes function

model <- naiveBayes(Machine_failure ~ Type + Air_temperature2 + Process_temperature2+
                      Rotational_speed2 + Torque2 + Tool_wear2, training_data)
model


## Naive Bayes with R: Same example than previously

# Predict with test_data


results <- predict(model,test_data)
results





# Excercise ---------------------------------------------------------------
#1) 80% training data and 20% test data
#2) Fit Naive Bayes using this new training set
#3) Predict using test-data
#4) Compute the accuracy
# -------------------------------------------------------------------------

num_train_rows <- round(0.80 * nrow(Maintenance_data))
num_train_rows

training_data <- as.data.frame(Maintenance_data[1:800,])
test_data <- as.data.frame(Maintenance_data[801:1000,])

test_data$Machine_failure


## Naive Bayes with R: Same example than previously
# The package "e1071" in R has a built-in naiveBayes function

model <- naiveBayes(Machine_failure~Type+Air_temperature2+Process_temperature2+
                      Rotational_speed2+Torque2+Tool_wear2, training_data)
model


## Naive Bayes with R: Same example than previously
# Predict with test_data
results <- predict(model,test_data)
results

table = table(test_data$Machine_failure,results)
prop.table(table)


#88.5% [because you plus 0.65 and 0.235 from the results table]







