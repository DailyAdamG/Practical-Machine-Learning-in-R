#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 6")

#Loading necessary library and data set

library(tidyverse)
library(dplyr)

heart <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 6/Data/heart.csv", col_types = "nffnnffnfnfnff")

#Preview the data

glimpse(heart)

#View the summary of the data

summary(heart)

#Removing data with any missing values

heart <- heart %>%
  filter(!is.na(restingBP) & !is.na(cholesterol) & !is.na(highBloodSugar) & !is.na(restingECG)
         & !is.na(restingHR) & !is.na(exerciseAngina) & !is.na(STdepression) & !is.na(STslope)
         & !is.na(coloredVessels) & !is.na(defectType))

#Normalize the data using the min-max method

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

heart <- heart %>%
  mutate(age = normalize(age)) %>%
  mutate(restingBP = normalize(restingBP)) %>%
  mutate(cholesterol = normalize(cholesterol)) %>%
  mutate(restingHR = normalize(restingHR)) %>%
  mutate(STdepression = normalize(STdepression)) %>%
  mutate(coloredVessels = normalize(coloredVessels))

#Summarize the new data set

summary(heart)

#Change the data set from a tibble to a data frame

heart <- data.frame(heart)

#Separate the class labels from the rest of the data

heart_labels <- heart %>% select(heartDisease)

heart <- heart %>% select(-heartDisease)

#View the original column names

colnames(heart)

#Create dummy variables using dummies library

library(dummies)

heart <- dummy.data.frame(data = heart, sep = "_")

colnames(heart)

#Splitting the data

set.seed(1234)

sample_index <- sample(nrow(heart), round(nrow(heart) * .75), replace = FALSE)
heart_train <- heart[sample_index,]
heart_test <- heart[-sample_index,]

#Splitting the class data

heart_train_labels <- as.factor(heart_labels[sample_index,])
heart_test_labels <- as.factor(heart_labels[-sample_index,])

#Classifying unlabeled data

library(class)

heart_pred1 <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k = 15)

#Preview the predictions

head(heart_pred1)

#Creating a confusion matrix to evaluate the model

heart_pred1_table <- table(heart_test_labels, heart_pred1)

heart_pred1_table

#Viewing the results

sum(diag(heart_pred1_table)) / nrow(heart_test)

#Setting the k to 40 to see if it has a meaningful impact on predictive accuracy

heart_pred3 <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k = 40)

#Creating a confusion matrix to evaluate the model

heart_pred3_table <- table(heart_test_labels, heart_pred3)

sum(diag(heart_pred3_table)) / nrow(heart_test)

#CASE STUDY: REVISITING THE DONOR DATA SET

#Loading necessary library and data set

library(tidyverse)

donors <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 6/Data/donors.csv", col_types = "nnnnnnnnnnnnffffffffff")

#View the imported data set

glimpse(donors)

#Limiting data to only numeric columns

donors <- donors %>%
  select(age, numberChildren, incomeRating, wealthRating, mailOrderPurchases, totalGivingAmount,
         numberGifts, smallestGiftAmount, largestGiftAmount, averageGiftAmount, yearsSinceFirstDonation,
         monthsSinceLastDonation, respondedMailing)

#Summarize the data set

summary(donors)

#Use mean imputation for missing age values in the age variable

donors <- donors %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age))

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