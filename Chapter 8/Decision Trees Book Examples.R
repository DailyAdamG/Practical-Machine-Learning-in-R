#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 8")

#Loading necessary libraries and data set

library(tidyverse)

permits <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 8/Data/permits.csv", col_types = "ffffffnnnnfffff")

#View the data

glimpse(permits)

summary(permits)

#Change unreasonable values to missing values instead

permits <- permits %>%
  mutate(valuation = ifelse(valuation < 1, NA, valuation)) %>%
  mutate(floorArea = ifelse(floorArea < 1, NA, floorArea)) %>%
  mutate(numberUnits = ifelse(numberUnits < 1, NA, numberUnits)) %>%
  mutate(stories = ifelse(stories <  1, NA, stories)) %>%
  mutate(stories = ifelse(stories > 73, NA, stories))

#Look at the new summary statistics

summary(select(permits, valuation, floorArea, numberUnits, stories))

#Reducing data set to only four features

permits <- permits %>%
  select(permitType, permitSubtype, initiatingOffice, permitCategory)

#Splitting the data

set.seed(1234)

sample_set <- sample(nrow(permits), round(nrow(permits) * .8), replace = FALSE)
permits_train <- permits[sample_set,]
permits_test <- permits[-sample_set,]

#Check the class distributions

round(prop.table(table(select(permits, permitCategory))), 2)
round(prop.table(table(select(permits_train, permitCategory))), 2)
round(prop.table(table(select(permits_test, permitCategory))), 2)

#Building the model

library(rpart)

permits_mod <- rpart(permitCategory ~ ., method = "class", data = permits_train)

#Plot the model

library(rpart.plot)

rpart.plot(permits_mod)

#Create a confusion matrix to evaluate the accuracy of the model.

permits_pred <- predict(permits_mod, permits_test, type = "class")

permits_pred_table <- table(permits_test$permitCategory, permits_pred)

permits_pred_table

sum(diag(permits_pred_table)) / nrow(permits_test)

#CASE STUDY: REVISITING THE INCOME PREDICTION PROBLEM

#Importing the data

library(tidyverse)

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 8/Data/income.csv", col_types = "nffnfffffnff")

#View the data

glimpse(income)

summary(income)

#Split the data

set.seed(1234)

sample_set <- sample(nrow(income), round(nrow(income) * .75), replace = FALSE)

income_train <- income[sample_set, ]

income_test <- income[-sample_set, ]

#Check the class distributions

round(prop.table(table(select(income, income), exclude = NULL)), 4) * 100

round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100

round(prop.table(table(select(income_test, income), exclude = NULL)), 4) * 100

#Fix class imbalance problem

library(DMwR)

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

round(prop.table(table(select(income_train, income))), 4) * 100

#Build the model

library(rpart)

income_mod <- rpart(income ~., method = "class", data = income_train)

#Evaluate the model

library(rpart.plot)

rpart.plot(income_mod)

#Create a confusion matrix to evaluate the accuracy of the model

income_pred <- predict(income_mod, income_test, type = "class")

income_pred_table <- table(income_test$income, income_pred)

income_pred_table

sum(diag(income_pred_table)) / nrow(income_test)