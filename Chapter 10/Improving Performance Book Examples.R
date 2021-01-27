#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 10")

#Loading necessary libraries and data set

library(caret)
library(rpart)

modelLookup("rpart")

library(tidyverse)

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 10/Data/income.csv", col_types = "nffnfffffnff")

#Split the data

set.seed(1234)

sample_set <- createDataPartition(y = income$income, p = .75, list = FALSE)

income_train <- income[sample_set, ]

income_test <- income[-sample_set, ]

#We know that we need to account for the class imbalance, since we've used this data set before.

set.seed(1234)

library(DMwR)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#Build and tune the model using the train() function

set.seed(1234)

income_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rpart",
                    trControl = trainControl(method = "boot632", number = 3))

income_mod

#Evaluate how well the model performed

income_pred <- predict(income_mod, income_test)

confusionMatrix(income_pred, income_test$income, positive = "<=50K")

#Customizing the model using tuneLength

set.seed(1234)

income_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rpart",
                    trControl = trainControl(method = "boot632", number = 3), tuneLength = 20)

income_mod

#Specifying the cp values considered

expand.grid(.alpha = c(1, 2, 3), .beta = c(TRUE, FALSE), .gamma = seq(from = 4, to = 5, by = .5))

expand.grid(.cp = seq(from = .0001, to = .002, by = .0001))

#Update the model again using tuneGrid argument

set.seed(1234)

income_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rpart",
                    trControl = trainControl(method = "boot632", number = 3), 
                    tuneGrid = expand.grid(.cp = seq(from = .0001, to = .002, by = .0001)))

income_mod

#Test model results on test data to ensure we're not just overfitting

income_pred <- predict(income_mod, income_test)

confusionMatrix(income_pred, income_test$income, positive = "<=50K")

#Look to see what parameters to tune for random forest models

library(randomForest)

modelLookup("rf")

#Create an ensemble model using random forest method

set.seed(1234)

rf_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "rf",
                trControl = trainControl(method = "none"), tuneGrid = expand.grid(.mtry = 3))

#Check how the random forest ensemble model does against the test data

rf_pred <- predict(rf_mod, income_test)

confusionMatrix(rf_pred, income_test$income, positive = "<=50K")

#Look to see what parameters to tune for XGBoost models

library(xgboost)

modelLookup("xgbTree")

#Create an ensemble model using extreme gradient boosting method

set.seed(1234)

xgb_mod <- train(income ~ ., data = income_train, metric = "Accuracy", method = "xgbTree",
                 trControl = trainControl(method = "none"), tuneGrid = expand.grid(
                   nrounds = 100, max_depth = 6, eta = 0.3, gamma = .01, 
                   colsample_bytree = 1, min_child_weight = 1, subsample = 1))

#Check how the XGBoost ensemble model does against the test data

xgb_pred <- predict(xgb_mod, income_test)

confusionMatrix(xgb_pred, income_test$income, positive = "<=50K")

#Rename income variable, so that values don't start with a symbol or contain numbers

library(tidyverse)

library(DMwR)

income <- income %>%
  mutate(income = as.factor(recode(income, "<=50K" = "Below", ">50K" = "Above")))

#Split the data

library(caret)

set.seed(1234)

sample_set <- createDataPartition(y = income$income, p = .75, list = FALSE)

income_train <- income[sample_set, ]

income_test <- income[-sample_set, ]

#Balance the training data

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#Create a list of ensemble learners needed to build the ensemble model

library(caretEnsemble)

ensembleLearners <- c("rpart", "glm", "knn")

library(rpart)
library(stats)
library(class)

#Train a model based on the list of learners

models <- caretList(income ~ ., data = income_train, metric = "Accuracy",
                    methodList = ensembleLearners, trControl = trainControl(
                      method = "repeatedcv", number = 10, repeats = 5,
                      savePredictions = "final", classProbs = TRUE
                    ))

#View the results

results <- resamples(models)

summary(results)

#Check for correlation between models

modelCor(results)

#Build the meta-model

library(randomForest)

stack_mod <- caretStack(models, method = "rf", metric = "Accuracy", 
                        trControl = trainControl(method = "repeatedcv", number = 10,
                                                 repeats = 5, savePredictions = "final",
                                                 classProbs = TRUE))

#View the results

stack_pred <- predict(stack_mod, income_test)

confusionMatrix(stack_pred, income_test$income, positive = "Below")