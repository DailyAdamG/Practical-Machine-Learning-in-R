#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 5")

#Loading necessary library and data set

library(tidyverse)

income <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 5/Data/income.csv", col_types = "nffnfffffnff")

#Exercise 1

#Consider each one of the following problems. Would this problem best be approached
#as a regression problem or a classification problem?

#a) Predicting the restaurant chain that someone is most likely to visit based upon
#their age , number of children, ZIP code, and income level.

#This is a classification problem

#b) Predicting the number of visitors that a restaurant is likely to see on a given
#day based upon the day of the week, the outdoor temperature, and whether the
#restaurant is running a promotion.

#This is a regression problem

#c) Predicting the baseball team that an individual is likely to cheer for based
#upon their place of birth, current residence, age, and gender.

#This is a classification problem

#d) Predicting the price of a used car based upon the make, model, age, odometer
#reading, condition, and color.

#This is a regression problem

#Exercise 2

#You are working with a healthcare provider who provides patients with a free annual
#health screening. The provider would like to better understand the factors that
#drive participation in the screening program. You use logistic regression to
#develop a model that predicts participation based upon an individual's marital
#status and ethnicity. The results of the model are shown here:

"Call:
  glm(formula = participated ~ age + maritalStatus + ethnicity,
      family = binomial, data = patients_train)
      
Deviance Residuals:
  Min 1Q Median 3Q Max
-1.739 -1.256 1.018 1.027 1.590

Coefficients:

  Estimate Std. Error z value Pr(>|z|)
  
(Intercept) 1.424848 0.567979 2.509 0.0121 *
  age 0.000498 0.002121 0.235 0.8144
maritalStatusMarried -0.195182 0.159257 -1.226 0.2204
maritalStatusNot Known -1.150035 0.175621 -6.548 5.82e-11 ***
  maritalStatusSingle -0.770244 0.168187 -4.580 4.66e-06 ***
  maritalStatusWidowed -0.441739 0.290676 -1.520 0.1286
ethnicityAsian -1.019093 0.543590 -1.875 0.0608 .
ethnicityBlack or African American -1.187287 0.544551 -2.180 0.0292 *
  ethnicityHispanic -0.984501 0.545999 -1.803 0.0714 .
ethnicityNative Hawaiian or Other Pacific Islander -12.230119 196.968421 -0.062 0.9505
ethnicityTwo or More -1.060614 0.561182 -1.890 0.0588 .
ethnicityUnknown -1.217726 0.554415 -2.196 0.0281 *
  ethnicityWhite -0.880737 0.536667 -1.641 0.1008
---

  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
(Dispersion parameter for binomial family taken to be 1)

Null deviance: 8464.2 on 6111 degrees of freedom
Residual deviance: 8223.3 on 6099 degrees of freedom
AIC: 8249.3
Number of Fisher Scoring iterations: 10"

#a) In this model, which variable has the greatest effect on the outcome?

#The marital status variable has the greatest effect on the outcome.

#b) For that variable, rank-order the levels from the group least likely to
#participate in the assessments to the group most likely to participate in the assessments

#Not known, single, widowed, married

#Exercise 3

#After developing the model in exercise 2, you obtained additional information
#about the individuals in the study. Specifically, you learned how many prior
#times each person participated in the screening program. You incorporate that
#information into your model and obtain these results:

"Call:
  glm(formula = participated ~ age + maritalStatus + ethnicity +
        priorScreenings, family = binomial, data = patients_train)

Deviance Residuals:
  Min 1Q Median 3Q Max
-2.1965 -0.6845 0.2264 0.5264 2.1374

Coefficients:
  Estimate Std. Error z value Pr(>|z|)
  
(Intercept) 0.420756 0.692364 0.608 0.5434
age -0.017940 0.002855 -6.284 3.31e-10***
  maritalStatusMarried 0.078128 0.225397 0.347 0.7289
maritalStatusNot Known 0.205479 0.241209 0.852 0.3943
maritalStatusSingle -0.352247 0.236139 -1.492 0.1358
maritalStatusWidowed -0.035840 0.406231 -0.088 0.9297
ethnicityAsian -1.095094 0.653537 -1.676 0.0938 .
ethnicityBlack or African American -1.151009 0.654967 -1.757 0.0789 .
ethnicityHispanic -0.953887 0.656464 -1.453 0.1462
ethnicityNative Hawaiian or Other Pacific Islander -11.293698 196.968754 -0.057 0.9543
ethnicityTwo or More -1.341665 0.679203 -1.975 0.0482 *
  ethnicityUnknown -1.093776 0.666182 -1.642 0.1006
ethnicityWhite -1.076935 0.644631 -1.671 0.0948 .
priorScreenings 1.619062 0.040467 40.010 < 2e-16***
  ---
  Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
(Dispersion parameter for binomial family taken to be 1)

Null deviance: 8464.2 on 6111 degrees of freedom
Residual deviance: 5267.5 on 6098 degrees of freedom
AIC: 5295.5

Number of Fisher Scoring iterations: 10"

#a) Are individuals who participated in a past screening more likely to participate
#in future screenings, less likely to participate in future screenings, or is it
#not possible to determine a difference?

#Individuals who participated in a past screening are more likely to participate
#in future screenings, since the coefficient of the variable is positive.

#b) For each time an individual participated in a past screening, by what factor
#do the odds change that they will participate in the next screening?

#In order to find the change in odds ratio, we compute the exponential of the coefficient.

exp(1.619)

#Which is 5.05. Therefore, each time a person participated in a past screening, they
#are 5 times more likely to participate in the next screening.

#c) The new model fits better, because the AIC for the second model is 5295.5
#compared to 8249.3 in the first model.

#Exercise 4

#After improving your model in exercise 3, you use the model to make predictions
#for employees that were not in the original training set. You obtain the following
# 10 predictions:

"0.1465268 0.9588654 0.9751363 0.4956821 0.8601916

0.3984430 0.2268064 0.8490515 0.9527210 0.4642998"

#a) Interpret these results. How many of these ten employees are likely to participate
#in the wellness assessment?

#There are 5 results that are higher than .5 probability, so that is how many people
#are likely to participate in the wellness assessment.

#b) How could you improve your predictions?

#We could calculate the optimal cutoff value using the optimalCutoff() function
#from the InformationValue package.

#Exercise 5

#Extend the logistic regression model from the income prediction use case to include
#the continuous variables as well.

#a) Create and examine a correlation plot for these variables. Do they exhibit multicollinearity?

income %>%
  keep(is.numeric) %>%
  cor() %>%
  corrplot()

#There is only a slight positive correlation between these three variables. No they
#do not exhibit multicollinearity.

#b) Examine the summary statistics for the continuous variables. Do you observe
#any outliers? If so, address them appropriately.

income %>%
  keep(is.numeric) %>%
  summary()

#The summary data looks reasonable. There is some concern about the workHours maximum
#being 99, but the data is only slightly right skewed.

#c) Fit a logistic regression model to the data set. This time, include both the
#continuous and categorical variables. Use the same training/test data set split as
# the use case.

#Split the data

set.seed(1234)

sample_set <- sample(nrow(income), round(nrow(income) * .75), replace = FALSE)

income_train <- income[sample_set,]
income_test <- income[-sample_set,]

#Balance the training data

library(DMwR)

set.seed(1234)

income_train <- SMOTE(income ~ ., data.frame(income_train), perc.over = 100, perc.under = 200)

#Train the new model

income_mod2 <- income_train %>%
  glm(formula = income ~ ., family = binomial)

#d) Examine the summary of the model. Were the continuous variables significant?
#How does this model compare to the model without the continuous variables?

summary(income_mod2)

#All three continuous variables are significant. This model is more favorable than
#the previous model, because the AIC is 20164 and this is lower than the AIC of 20968
#in the previous model.

#e) Generate predictions for the test data set using a .5 threshold and create a
#confusion matrix of your results. Compare these results to the model from earlier
#in the chapter.

#Generating predictions

income_pred2 <- predict(income_mod2, income_test, type = "response")
head(income_pred2)

#Recode predictions using .5 cutoff value

income_pred2 <- ifelse(income_pred2 >= .5, 1, 0)
head(income_pred2)

#Create a confusion matrix to test the model

income_pred2.table <- table(income_test$income, income_pred2)

sum(diag(income_pred2.table)) / nrow(income_test)

#The new model is 78.6% accurate compared to the first model which was 77.5% accurate
#using .5 as the cutoff. The new model is more accurate than the previous model.