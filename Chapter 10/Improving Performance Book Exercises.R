#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 10")

#Exercise 1

#Research the tuning parameters available for other learning methods with the caret
#package. What parameters may be tuned for each of the following techniques?

#a) k-nearest-neighbor (with the knn package)

#k

#b) Generalized linear models (with the glm package)

#none

#c) Naive Bayes (with the naive_bayes package)

#laplace, usekernel, adjust

#d) Random forest (with the rf package)

#mtry

#Exercise 2

#Attempt to improve the accuracy of the income prediction random forest model by
#doing some additional parameter tuning. What improvement in predictive accuracy
#were you able to achieve?

#Loading necessary libraries and data set

library(caret)
library(DMwR)
library(tidyverse)

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 10/Data/income.csv", col_types = "nffnfffffnff")

#Split the data

set.seed(1234)

sample_set <- createDataPartition(y = income$income, p = .75, list = FALSE)

income_train <- income[sample_set, ]

income_test <- income[-sample_set, ]

#We know that we need to account for the class imbalance, since we've used this data set before.

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#Build and tune the model using the train() function

library(randomForest)

set.seed(1234)

rf_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rf",
                    trControl = trainControl(method = "boot632", number = 3),
                    tuneGrid = expand.grid(.mtry = seq(from = 1, to = 11, by = 2)))

rf_mod

#Evaluate how well the model performed

rf_pred <- predict(rf_mod, income_test)

confusionMatrix(rf_pred, income_test$income, positive = "<=50K")

#Accuracy increased from 79% to 81%.

#Exercise 3

#Now, attempt to improve the predictive accuracy of the income prediction model by
#using the extreme gradient boosting approach. This time, instead of explicitly
#setting the tuning parameters, have caret evaluate two values per hyperparameter
#in order to select the combination that provides the best predictive accuracy.
#What improvement in predictive accuracy were you able to achieve?

#Loading necessary libraries and data set

library(caret)
library(DMwR)
library(tidyverse)

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 10/Data/income.csv", col_types = "nffnfffffnff")

#Split the data

set.seed(1234)

sample_set <- createDataPartition(y = income$income, p = .75, list = FALSE)

income_train <- income[sample_set, ]

income_test <- income[-sample_set, ]

#We know that we need to account for the class imbalance, since we've used this data set before.

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#Build and tune the model using the train() function

library(xgboost)

set.seed(1234)

xgb_mod <- train(income ~ ., data = income_train, metric = "Kappa", method = "xgbTree",
                 trControl = trainControl(method = "boot632", number = 3),
                 tuneLength = 2)

xgb_mod

#Check how the XGBoost ensemble model does against the test data

xgb_pred <- predict(xgb_mod, income_test)

confusionMatrix(xgb_pred, income_test$income, positive = "<=50K")

#The accuracy of the model stayed at 82%.