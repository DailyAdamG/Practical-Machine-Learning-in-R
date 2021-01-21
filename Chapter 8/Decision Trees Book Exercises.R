#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 8")

#Exercise 1

#Use the decision tree that we built in the case study (shown in Figure 8.8) to
#predict the income level for each of the following people:

#a) A married 30-year-old woman with 16 years of education

#I predict this person has an income greater than or equal to $50,000.

#b) A divorced 45-year-old man with 12 years of education

#I predict this person has an income greater than or equal to $50,000.

#c) A married 40-year-old woman with 8 years of education

#I predict this person has an income less than $50,000.

#Exercise 2

#Attempt to improve the accuracy of the building permit model by including additional
#features in the decision tree. What improvement in predictive accuracy were you
#able to achieve?

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

summary(permits)

#Remove status, year and month from the data set.

permits <- permits %>%
  select(-status, -year, -month)

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

#Accuracy improved to 87.7%

#Exercise 3

#The C5.0 algorithm discussed in this chapter takes a different approach to building
#decision trees. Use the C50 package in R to build a decision tree model of the building
#permit data set using the same features that we used in this chapter. What results
#did you achieve? How do they differ from the results in the chapter and the results
#in Exercise 2?

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

summary(permits)

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

library(C50)

permits_mod <- C5.0(permitCategory ~ ., data = permits_train)

summary(permits_mod)

#Create a confusion matrix to evaluate the accuracy of the model.

permits_pred <- predict(permits_mod, permits_test, type = "class")

permits_pred_table <- table(permits_test$permitCategory, permits_pred)

permits_pred_table

sum(diag(permits_pred_table)) / nrow(permits_test)

#Model accuracy is 86.7% using this approach. This is better than the original
#problem from the chapter using only 4 features, but not as good as the model from
#exercise 2 that uses all the features in the model.