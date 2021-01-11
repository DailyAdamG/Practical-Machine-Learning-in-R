#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 7")

#Loading necessary libraries and data set

library(tidyverse)
library(dplyr)

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 7/Data/income.csv", col_types = "nffnfffffnff")

#Exercises 1 and 2 use the following frequency table. This is data collected from
#a gym that offers several different levels of membership. Standard membership allows
#members to participate in three classes per week. Elite membership allows members
#to participate in an unlimited number of classes each week. Drop-in membership
#includes no classes, but members may attend a class after paying a per-session fee.
#The frequency table shows the number of individuals at the gym who have purchased
#each membership plan, broken out by their age (teenager, adult, or senior citizen),
#their gender (male or female), and their homeownership status.

"
Level Teenager Adult Senior Male Female Homeowner Total
Drop-in 94 458 280 406 426 422 832
Standard 112 915 174 581 620 817 1201
Elite 20 250 95 60 305 270 365
Total 226 1623 549 1047 1351 1509 2398

"

#Exercise 1

#The gym is soliciting a new member who is a female adult homeowner.

#a) Compute Likelihood(Drop-in|Female, Adult, Homeowner)

Prob_D = (832/2398)

Prob_D_f = (426/1351)

Prob_D_a = (458/1623)

Prob_D_h = (422/1509)

round(Prob_D * Prob_D_f * Prob_D_a * Prob_D_h,3)

#b) Compute Likelihood(Standard|Female, Adult, Homeowner)

Prob_S = (1201/2398)

Prob_S_f = (620/1351)

Prob_S_a = (915/1623)

Prob_S_h = (817/1509)

round(Prob_S * Prob_S_f * Prob_S_a * Prob_S_h,3)

#c) Compute Likelihood(Standard|Female, Adult, Homeowner)

Prob_E = (365/2398)

Prob_E_f = (305/1351)

Prob_E_a = (250/1623)

Prob_E_h = (270/1509)

round(Prob_E * Prob_E_f * Prob_E_a * Prob_E_h,3)

#d) Which membership level is this person most likely to select?

round(Prob_D * Prob_D_f * Prob_D_a * Prob_D_h /
  (Prob_D * Prob_D_f * Prob_D_a * Prob_D_h + 
 Prob_S * Prob_S_f * Prob_S_a * Prob_S_h +
  Prob_E * Prob_E_f * Prob_E_a * Prob_E_h),3)

round(Prob_S * Prob_S_f * Prob_S_a * Prob_S_h /
        (Prob_D * Prob_D_f * Prob_D_a * Prob_D_h + 
           Prob_S * Prob_S_f * Prob_S_a * Prob_S_h +
           Prob_E * Prob_E_f * Prob_E_a * Prob_E_h),3)

round(Prob_E * Prob_E_f * Prob_E_a * Prob_E_h /
        (Prob_D * Prob_D_f * Prob_D_a * Prob_D_h + 
           Prob_S * Prob_S_f * Prob_S_a * Prob_S_h +
           Prob_E * Prob_E_f * Prob_E_a * Prob_E_h),3)

# This person is most likely to select a Standard membership level.

#Exercise 2

#The gym is soliciting a new member who is a male teenager who does not own a home.

#a) Compute Likelihood(Drop-in|Male, Teenager, noHomeowner)

Prob_D_m = (406/1047)

Prob_D_t = (94/226)
  
Prob_D_nh = ((832-422)/(2398-1509))

round(Prob_D * Prob_D_m * Prob_D_t * Prob_D_nh,3)

#b) Compute Likelihood(Standard|Male, Teenager, noHomeowner)

Prob_S_m = (581/1047)

Prob_S_t = (112/226)

Prob_S_nh = ((1201-817)/(2398-1509))

round(Prob_S * Prob_S_m * Prob_S_t * Prob_S_nh,3)

#c) Compute Likelihood(Elite|Male, Teenager, noHomeowner)

Prob_E_m = (60/1047)

Prob_E_t = (20/226)

Prob_E_nh = ((365-270)/(2398-1509))

Prob_E * Prob_E_m * Prob_E_t * Prob_E_nh

#d) Which membership level is this person most likely to select?

round(Prob_D * Prob_D_m * Prob_D_t * Prob_D_nh /
        (Prob_D * Prob_D_m * Prob_D_t * Prob_D_nh + 
           Prob_S * Prob_S_m * Prob_S_t * Prob_S_nh +
           Prob_E * Prob_E_m * Prob_E_t * Prob_E_nh),3)

round(Prob_S * Prob_S_m * Prob_S_t * Prob_S_nh /
        (Prob_D * Prob_D_m * Prob_D_t * Prob_D_nh + 
           Prob_S * Prob_S_m * Prob_S_t * Prob_S_nh +
           Prob_E * Prob_E_m * Prob_E_t * Prob_E_nh),3)

round(Prob_E * Prob_E_m * Prob_E_t * Prob_E_nh /
        (Prob_D * Prob_D_m * Prob_D_t * Prob_D_nh + 
           Prob_S * Prob_S_m * Prob_S_t * Prob_S_nh +
           Prob_E * Prob_E_m * Prob_E_t * Prob_E_nh),3)

#This person is most likely to select a Standard membership level, but not as likely
#as the previous person.

#Exercise 3

#In Chapter 5, we used logistic regression to predict the income of prospective customers.
#Using the same income data set, attempt to improve upon the predictive accuracy of the
#previous model by using a naive Bayes approach. Just like we did in Chapter 5, limit
#your data to only the categorical features and don't forget to balance your training data.
#Did your predictive accuracy improve?

#Look at data set

glimpse(income)

#Limit data set to only categorical variables

income %>%
  keep(is.factor) %>%
  summary()

#Split the data

sample_set <- sample(nrow(income), round(nrow(income) * .75), replace = FALSE)
income_train <- income[sample_set,]
income_test <- income[-sample_set,]

#Check the class distributions

round(prop.table(table(select(income, income))), 2)
round(prop.table(table(select(income_train, income))), 2)
round(prop.table(table(select(income_test, income))), 2)

#Balance the data using SMOTE

library(DMwR)

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

round(prop.table(table(select(income_train, income))), 2) * 100

#Create a model

library(e1071)

income_mod <- naiveBayes(income ~ ., data = income_train, laplace = 1)

income_mod

#Finding the predicted labels for each record

income_pred <- predict(income_mod, income_test, type = "class")

#Create a confusion matrix to evaluate the accuracy of the model

income_pred_table <- table(income_test$income, income_pred)

income_pred_table

sum(diag(income_pred_table)) / nrow(income_test)

#Both models performed similarly, but the Naive Bayes model has an accuracy of 77.71%,
#which is slightly better than the previous model.