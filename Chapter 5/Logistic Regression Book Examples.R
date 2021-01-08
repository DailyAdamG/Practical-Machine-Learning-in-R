#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 5")

#Loading necessary library and data set

library(tidyverse)

donors <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 5/Data/donors.csv", col_types = "nnffnnnnnnnnffffffffff")

#View the imported data set

glimpse(donors)

#View the summary stats for categorical variables

donors %>%
  keep(is.factor) %>%
  summary()

#Examine percentage of missing values from incomeRating variable

donors %>%
  select(incomeRating) %>%
  table(exclude = NULL) %>%
  prop.table()

#Replacing null values with UNK for unknown

donors <- donors %>%
  mutate(incomeRating = as.character(incomeRating)) %>%
  mutate(incomeRating = as.factor(ifelse(is.na(incomeRating), "UNK", incomeRating)))

#Review table to ensure changes worked

donors %>%
  select(incomeRating) %>%
  table() %>%
  prop.table()

#Repeat the same process for all other missing variables

donors <- donors %>%
  mutate(wealthRating = as.character(wealthRating)) %>%
  mutate(wealthRating = as.factor(ifelse(is.na(wealthRating), "UNK", wealthRating)))

donors <- donors %>%
  mutate(urbanicity = as.character(urbanicity)) %>%
  mutate(urbanicity = as.factor(ifelse(is.na(urbanicity), "UNK", urbanicity)))

donors <- donors %>%
  mutate(socioEconomicStatus = as.character(socioEconomicStatus)) %>%
  mutate(socioEconomicStatus = as.factor(ifelse(is.na(socioEconomicStatus), "UNK", socioEconomicStatus)))

donors <- donors %>%
  mutate(isHomeowner = as.character(isHomeowner)) %>%
  mutate(isHomeowner = as.factor(ifelse(is.na(isHomeowner), "UNK", isHomeowner)))

donors <- donors %>%
  mutate(gender = as.character(gender)) %>%
  mutate(gender = as.factor(ifelse(is.na(gender), "UNK", gender)))

#View summary after the new changes

donors %>%
  keep(is.factor) %>%
  summary()

#Look at the summary statistics for continuous variables

donors %>%
  keep(is.numeric) %>%
  summary()

#Substituting mean value for missing age values grouped by gender

donors <- donors %>%
  group_by(gender) %>%
  mutate(age = ifelse(is.na(age), mean(age, na.rm = TRUE), age)) %>%
  ungroup()

#View the new summary for age variable

donors %>%
  select(age) %>%
  summary()

#Substituting median value for missing numberChildren values

donors <- donors %>%
  mutate(numberChildren = ifelse(is.na(numberChildren), median(numberChildren, na.rm = TRUE), numberChildren))

#View the new summary for numberChildren variable

donors %>%
  select(numberChildren) %>%
  summary()

#Calculate the outlier cutoff values and remove outliers

donors <- donors %>%
  mutate(max1 = quantile(mailOrderPurchases, .75) + (1.5 * IQR(mailOrderPurchases))) %>%
  mutate(max2 = quantile(totalGivingAmount, .75) + (1.5 * IQR(totalGivingAmount))) %>%
  mutate(max3 = quantile(numberGifts, .75) + (1.5 * IQR(numberGifts))) %>%
  mutate(max4 = quantile(smallestGiftAmount, .75) + (1.5 * IQR(smallestGiftAmount))) %>%
  mutate(max5 = quantile(largestGiftAmount, .75) + (1.5 * IQR(largestGiftAmount))) %>%
  mutate(max6 = quantile(averageGiftAmount, .75) + (1.5 *IQR(averageGiftAmount))) %>%
  filter(mailOrderPurchases <= max1) %>%
  filter(totalGivingAmount<= max2) %>%
  filter(numberGifts<= max3) %>%
  filter(smallestGiftAmount<= max4) %>%
  filter(largestGiftAmount<= max5) %>%
  filter(averageGiftAmount<= max6) %>%
  select(-max1, -max2, -max3, -max4, -max5, -max6)

#View summary statistics without outliers

donors %>%
  keep(is.numeric) %>%
  summary()

#Splitting the data

set.seed(1234)

sample_set <- sample(nrow(donors), round(nrow(donors) * .75), replace = FALSE)
donors_train <- donors[sample_set,]
donors_test <- donors[-sample_set,]

#Comparing sample data distribution to population distribution

round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

#Balance training data using SMOTE

library(DMwR)
set.seed(1234)
donors_train <- SMOTE(respondedMailing ~ ., data.frame(donors_train), perc.over = 100, perc.under = 200)

#Comparing the new distributions

round(prop.table(table(select(donors, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_train, respondedMailing), exclude = NULL)), 4) * 100
round(prop.table(table(select(donors_test, respondedMailing), exclude = NULL)), 4) * 100

#Translating FALSE/TRUE to 0/1 for response variable

donors <- donors %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE, 1, 0)))

donors_train <- donors_train %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE, 1, 0)))

donors_test <- donors_test %>%
  mutate(respondedMailing = as.factor(ifelse(respondedMailing == TRUE, 1, 0)))

#Building the logistic regression model

donors_mod1 <- glm(data = donors_train, family = binomial, formula = respondedMailing ~ .)

#View the summary statistics for the model

summary(donors_mod1)

#Converting coefficient of averageGiftAmount to odds instead of log-odds

exp(coef(donors_mod1)["averageGiftAmount"])

#Converting coefficient of monthsSinceLastDonation to odds instead of log-odds

exp(coef(donors_mod1)["monthsSinceLastDonation"])

#Converting coefficient of incomeRating2 to odds instead of log-odds

exp(coef(donors_mod1)["incomeRating2"])

#Generate predicitons from model

donors_pred1 <- predict(donors_mod1, donors_test, type = "response")

#Remove states from test data that did not appear in training data

filter(donors_test, state == "RI" | state == "NH")

#Removing the data, since there are only three records

donors_test <- donors_test %>%
  filter(state!= "RI" & state != "NH")

#Redo predictions without missing data

donors_pred1 <- predict(donors_mod1, donors_test, type = "response")
head(donors_pred1)

#Convert predictions to binary responses

donors_pred1 <- ifelse(donors_pred1 >= .5, 1, 0)
head(donors_pred1)

#Create a confusion matrix

donors_pred1_table <- table(donors_test$respondedMailing, donors_pred1)
donors_pred1_table

#Calculate the correct number of predictions by the model

sum(diag(donors_pred1_table) / nrow(donors_test))

#Check for multicollinearity by creating a correalation plot

library(stats)

library(corrplot)

donors %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

#Check the VIF

library(car)
vif(donors_mod1)

#Excluding some of the variables with high VIF

donors_mod2 <- glm(data = donors_train, family = binomial, formula = respondedMailing ~ 
                     incomeRating + wealthRating + mailOrderPurchases + numberGifts +
                     yearsSinceFirstDonation + monthsSinceLastDonation + sweepstakesDonor +
                     state + urbanicity + socioEconomicStatus + isHomeowner + gender)
summary(donors_mod2)

#Check that there is no multicollinearity

vif(donors_mod2)

#Make predictions using new model

donors_pred2 <- predict(donors_mod2, donors_test, type = "response")
head(donors_pred2)

#Determining ideal cutoff value

library(InformationValue)

ideal_cutoff <- optimalCutoff(actuals = donors_test$respondedMailing, predictedScores = donors_pred2, optimiseFor = "Both")

ideal_cutoff

#Check new model's accuaracy with ideal cutoff

donors_pred2 <- ifelse(donors_pred2 >= ideal_cutoff, 1, 0)

donors_pred2_table <- table(donors_test$respondedMailing, donors_pred2)

sum(diag(donors_pred2_table)) / nrow(donors_test)

#CASE STUDY: INCOME PREDICTION

#Loading necessary data

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 5/Data/income.csv", col_types = "nffnfffffnff")

#View the data

glimpse(income)

#Statistical summary of categorical variables

income %>%
  keep(is.factor) %>%
  summary()

#Find the distributions for variables with more than six values

table(select(income, workClassification))
table(select(income, educationLevel))
table(select(income, occupation))
table(select(income, nativeCountry))

#Replacing ? with UNK

income <- income %>%
  mutate(workClassification = dplyr::recode(workClassification, "?" = "UNK")) %>%
  mutate(nativeCountry = dplyr::recode(nativeCountry, "?" = "UNK")) %>%
  mutate(occupation = dplyr::recode(occupation, "?" = "UNK"))

#Turning response variables into 0/1

income <- income %>%
  mutate(income = dplyr::recode(income, "<=50K" = "0")) %>%
  mutate(income = dplyr::recode(income, ">50K" = "1"))

#Summarize response variable

summary(income[, "income"])

#Split the data

set.seed(1234)

sample_set <- sample(nrow(income), round(nrow(income) * .75), replace = FALSE)

income_train <- income[sample_set,]
income_test <- income[-sample_set,]

#Check class distributions

round(prop.table(table(select(income, income), exclude = NULL)), 4) * 100

round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100

round(prop.table(table(select(income_test, income), exclude = NULL)), 4) * 100

#Balance the training data

library(DMwR)

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#Ensure the split is even

round(prop.table(table(select(income_train, income), exclude = NULL)), 4) * 100

#Build the model

income_mod1 <- income_train %>%
  keep(is.factor) %>%
  glm(formula = income ~ ., family = binomial)

#View the model summary

summary(income_mod1)

#Generating predictions

income_pred1 <- predict(income_mod1, income_test, type = "response")
head(income_pred1)

#Find the ideal cutoff value

library(InformationValue)

ideal_cutoff <- optimalCutoff(actuals = income_test$income, predictedScores = income_pred1, optimiseFor = "Both")

ideal_cutoff

#Recode predictions using cutoff value

income_pred1 <- ifelse(income_pred1 >= ideal_cutoff, 1, 0)
head(income_pred1)

#Create a confusion matrix to test the model

income_pred1.table <- table(income_test$income, income_pred1)

sum(diag(income_pred1.table)) / nrow(income_test)