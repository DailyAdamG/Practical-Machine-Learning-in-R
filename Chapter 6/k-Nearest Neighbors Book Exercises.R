#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 6")

#Loading necessary libraries and data set

library(tidyverse)
library(dplyr)

donors <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 6/Data/donors.csv", col_types = "nnnnnnnnnnnnffffffffff")

#Exercise 1

#Examine the following figure. The square near the center of the diagram represents
#a new, unlabeled point. Using the k-nearest neighbors algorithm, what class would
#you assign the point using each of the following parameters?

#a) k=1

#Circle, because it is the closest figure to the unlabeled point.

#b) k=3

#Star, because there are more stars closer to the unlabeled point than any other figure.

#c) k=7

#Triangle, because there are more triangles closer to the unlabeled point than any other figure.

#d) k=15

#Plus sign, because there are more Plus signs closer to the unlabeled point than any other figure.

#Exercise 2

#Modify the code used for the donation data use case to incorporate categorical variables
#into the model. What impact does this have on the accuracy of the model?

#Summarize the data set

summary(donors)

#Replace null values in categorical variables with UNK

donors <- donors %>%
  mutate(urbanicity = as.character(urbanicity)) %>%
  mutate(socioEconomicStatus = as.character(socioEconomicStatus)) %>%
  mutate(isHomeowner = as.character(isHomeowner)) %>%
  mutate(gender = as.character(gender)) %>%
  mutate(urbanicity = as.factor(ifelse(is.na(urbanicity), "UNK", urbanicity))) %>%
  mutate(socioEconomicStatus = as.factor(ifelse(is.na(socioEconomicStatus), "UNK", socioEconomicStatus))) %>%
  mutate(isHomeowner = as.factor(ifelse(is.na(isHomeowner), "UNK", isHomeowner))) %>%
  mutate(gender = as.factor(ifelse(is.na(gender), "UNK", gender)))

#Use mean imputation for missing age values in the age variable by gender

donors <- donors %>%
  group_by(gender) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age)) %>%
  ungroup()

summary(donors$age)

#Use median imputation for missing age values in the age variable

donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren), median(numberChildren, na.rm = TRUE), numberChildren))

summary(donors$numberChildren)

#Exclude missing incomeRating and wealthRating values from the data set

donors <- donors %>%
  filter(!is.na(incomeRating) & !is.na(wealthRating) & wealthRating > 0)

summary(select(donors, incomeRating, wealthRating))

#Normalize the data using the min-max method

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

donors <- donors %>%
  mutate(age = normalize(age)) %>%
  mutate(numberChildren = normalize(numberChildren)) %>%
  mutate(incomeRating = normalize(incomeRating)) %>%
  mutate(wealthRating = normalize(wealthRating)) %>%
  mutate(mailOrderPurchases = normalize(mailOrderPurchases)) %>%
  mutate(totalGivingAmount = normalize(totalGivingAmount)) %>%
  mutate(numberGifts = normalize(numberGifts)) %>%
  mutate(smallestGiftAmount = normalize(smallestGiftAmount)) %>%
  mutate(largestGiftAmount = normalize(largestGiftAmount)) %>%
  mutate(averageGiftAmount = normalize(averageGiftAmount)) %>%
  mutate(yearsSinceFirstDonation = normalize(yearsSinceFirstDonation)) %>%
  mutate(monthsSinceLastDonation = normalize(yearsSinceFirstDonation))

#Summarize data

summary(donors)

#Create dummy variables from categorical data

donors <- data.frame(donors)

library(dummies)

donors <- dummy.data.frame(data = donors,
                           names = c("inHouseDonor", "plannedGivingDonor",
                                     "sweepstakesDonor", "P3Donor", "state", "urbanicity",
                                     "socioEconomicStatus", "isHomeowner", "gender"), sep = "_")
#View new column names

colnames(donors)

#Splitting and balancing the data

donors <- data.frame(donors)

set.seed(1234)

sample_index <- sample(nrow(donors), round(nrow(donors) * .75), replace = FALSE)
donors_train <- donors[sample_index,]
donors_test <- donors[-sample_index,]

#Checking to see if there is a class imbalance

round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

#Balance the training data

library(DMwR)

set.seed(1234)

donors_train <- SMOTE(respondedMailing ~ ., data.frame(donors_train), perc.over = 100, perc.under = 200)

round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100

#Convert the vector labels into factors

donors_train_labels <- as.factor(pull(donors_train, respondedMailing))
donors_test_labels <- as.factor(pull(donors_test, respondedMailing))

#Remove the class labels from the data sets

donors_train <- data.frame(select(donors_train, -respondedMailing))
donors_test <- data.frame(select(donors_test, -respondedMailing))

#Building the model

library(class)

donors_pred <- knn(train = donors_train, test = donors_test, cl = donors_train_labels, k = 5)

head(donors_pred)

#Create a confusion matrix and test accuracy of the model

donors_pred_table <- table(donors_test_labels, donors_pred)
donors_pred_table

sum(diag(donors_pred_table)) / nrow(donors_test)

#The model is roughly 5% more accurate than the previous model with 59.5% of predictions being accurate.