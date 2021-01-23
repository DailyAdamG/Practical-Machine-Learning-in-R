#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 9")

#Loading necessary libraries and data set

library(tidyverse)

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 9/Data/income.csv", col_types = "nffnfffffnff")

#View the data

glimpse(income)

#Split the data

library(caret)

set.seed(1234)

sample_set <- createDataPartition(y = income$income, p = .75, list = FALSE)

income_train <- income[sample_set, ]

income_test <- income[-sample_set, ]

#Since we've worked with this data set before, we know that we need to balance the distribution class

library(DMwR)

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#Train and evaluate the model using the k-fold cross-validation approach

library(rpart)

set.seed(1234)

income_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rpart",
                    trControl = trainControl(method = "cv", number = 5))

#View the results of each fold

income_mod$resample %>%
  arrange(Resample)

#Find the average accuracy of the five iterations

income_mod$resample %>%
  summarise(AvgAccuracy = mean(Accuracy))

#Train and evaluate the model using the leave-one-out cross-validation approach

#This approach would take too long to compute. LOOCV should really only be used
#on smaller data sets.I will put code in quotations, so that it won't run.

"library(rpart)

set.seed(1234)

income_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rpart",
                    trControl = trainControl(method = "LOOCV"))
"

#Train and evaluate the model using the random cross-validation approach

library(rpart)

set.seed(1234)

income_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rpart", 
                    trControl = trainControl(method = "LGOCV", p = .1, number = 10))

#View the results of each iteration

income_mod$resample %>%
  arrange(Resample)

#Find the average accuracy of the ten iterations

income_mod$resample %>%
  summarise(AvgAccuracy = mean(Accuracy))

#Train and evaluate the model using the .632 bootstrap re-sampling technique

library(rpart)

set.seed(1234)

income_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rpart", 
                    trControl = trainControl(method = "boot632", number = 3))

#View the results of each iteration

income_mod$resample %>%
  arrange(Resample)

#Find the average accuracy of the three iterations

income_mod$resample %>%
  summarise(AvgAccuracy = mean(Accuracy))

#Load the spam data

load("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 9/Data/spam.RData")

#Create and view the confusion matrix results

spam_matrix <- confusionMatrix(email_pred, email_test$message_label, positive = "spam")

spam_matrix

#View accuracy only

spam_accuracy <- as.numeric(spam_matrix$overall["Accuracy"])

spam_accuracy

#View kappa only

spam_kappa <- as.numeric(spam_matrix$overall["Kappa"])

spam_kappa

#Find the spam sensitivity

spam_sensitivity <- sensitivity(email_pred, email_test$message_label, positive = "spam")

spam_sensitivity

#Find the spam specificity

spam_specificity <- specificity(email_pred, email_test$message_label, negative = "ham")

spam_specificity

#Find the spam precision

spam_precision <- posPredValue(email_pred, email_test$message_label, positive = "spam")

spam_precision

#Find the spam recall

spam_recall <- spam_sensitivity

spam_recall

#Find the f-score

spam_fmeasure <- (2 * spam_precision * spam_recall) / (spam_precision + spam_recall)

spam_fmeasure

#Load the spam data

load("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 9/Data/spam.RData")

#Get the raw email probabilities instead of the classifiers

library(e1071)

email_pred_prob <- predict(email_mod, email_test, type = "raw")

head(email_pred_prob)

#Create a prediction object

library(ROCR)

roc_pred <- prediction(predictions = email_pred_prob[, "spam"], labels = email_test$message_label)

#Create a performance object

roc_perf <- performance(roc_pred, measure = "tpr", x.measure = "fpr")

#Plot the ROC curve

plot(roc_perf, main = "ROC Curve", col = "green", lwd = 3)
abline(a = 0, b = 1, lwd = 3, lty = 2, col = 1)

#Find the area under the curve

auc_perf <- performance(roc_pred, measure = "auc")

spam_auc <- unlist(slot(auc_perf, "y.values"))

spam_auc