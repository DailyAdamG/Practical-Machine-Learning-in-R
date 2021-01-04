#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 4")

#Loading necessary library and data set

library(tidyverse)

bikes <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 4/Data/bikes.csv", col_types = "Dffffddddd")

#Calculating covariance between humidity and rentals

cov(bikes$humidity, bikes$rentals)

#Calculating standard deviation of humidity and rentals

sd(bikes$humidity)

sd(bikes$rentals)

#Calculating Pearson's correlation of humidity and rentals

pearson <- cov(bikes$humidity, bikes$rentals) / (sd(bikes$humidity) * sd(bikes$rentals))

pearson

#Calculating Pearson's correlation of humidity and rentals using the cor() function instead

cor(bikes$humidity, bikes$rentals)

#Calculating Pearson's correlation between rentals and wind speed and rentals and temperature

cor(bikes$windspeed, bikes$rentals)

cor(bikes$temperature, bikes$rentals)

#Creating correlation plots between all variables

library(corrplot)

bikenumeric <- bikes %>%
  select(-date)

bike_correlations <- cor(bikenumeric)

corrplot(bike_correlations)

corrplot(bike_correlations, type = "upper")

corrplot.mixed(bike_correlations)

#Finding the coefficient values using OLS methods

B1 <- cov(bikes$temperature, bikes$rentals) / var(bikes$temperature)

B1

B0 <- mean(bikes$rentals) - B1 * mean(bikes$temperature)

B0

#Simple regression model using lm function

bikes_mod1 <- lm(data = bikes, rentals ~ temperature)

bikes_mod1

#Get more info on the model

summary(bikes_mod1)

#Multiple regression model using humidity, wind speed, and temperature to predict rentals

bikes_mod2 <- lm(data = bikes, rentals ~ humidity + windspeed + temperature)

#View the results

summary(bikes_mod2)

#Calculate the model's residual mean

mean(bikes_mod2$residuals)

#Checking normality of residuals

library(olsrr)

ols_plot_resid_hist(bikes_mod2)

#Checking homoscedasticity of residuals

ols_plot_resid_fit(bikes_mod2)

#Checking residual autocorrelation

library(car)

durbinWatsonTest(bikes_mod2)

#Checking for influential points

ols_plot_cooksd_chart(bikes_mod2)

cooks_outliers <- ols_plot_cooksd_chart(bikes_mod2)$outliers

arrange(cooks_outliers, desc(cooks_distance))

#Observe data point 69 to see why it is an outlier

bikes[69, c("rentals", "humidity", "windspeed", "temperature")]

#Look at the summary statistics for the data set not including point 69

summary(bikes[-69, c("rentals", "humidity", "windspeed", "temperature")])

#Look at all the outliers and compare summary statistics to the other data summary

outlier_index <- as.numeric(unlist(cooks_outliers[, "observation"]))

#Summary statistics for outliers

summary(bikes[outlier_index, c("rentals", "humidity", "windspeed", "temperature")])

#Summary statistics for data not including outliers

summary(bikes[-outlier_index, c("rentals", "humidity", "windspeed", "temperature")])

#Summary statistics for the entire data set

summary(bikes[, c("rentals", "humidity", "windspeed", "temperature")])

#Create a new data set without outliers

bikes2 <- bikes[-outlier_index,]

#Checking for multicollinearity

ols_vif_tol(bikes_mod2)

#Creating new variables by squaring them

bikes2 <- bikes2 %>%
  mutate(humidity2 = humidity^2) %>%
  mutate(windspeed2 = windspeed^2) %>%
  mutate(temperature2 = temperature^2)

#Creating a new model with newly transformed predictors

bikes_mod3 <- lm(data = bikes2, rentals ~ humidity + windspeed + temperature + humidity2 + windspeed2 + temperature2)

#View summary statistics

summary(bikes_mod3)

#Removing windspeed2 from model, since it is not significant

bikes_mod3 <- lm(data = bikes2, rentals ~ humidity + windspeed + temperature + humidity2 + temperature2)

#View summary statistics

summary(bikes_mod3)

#View summary of categorical variables

summary(bikes2[, c("season", "holiday", "weekday", "weather")])

library(plyr)

library(dplyr)

#Renaming categorical values

bikes2 <- bikes2 %>%
  mutate(season = revalue(season, c("1" = "Winter", "2" = "Spring", "3" = "Summer", "4" = "Fall"))) %>%
  mutate(holiday = revalue(holiday, c("0" = "No", "1" = "Yes"))) %>%
  mutate(weekday = revalue(weekday, c("0" = "Sunday", "1" = "Monday", "2" = "Tuesday", "3" = "Wednesday", "4" = "Thursday", "5" = "Friday", "6" = "Saturday"))) %>%
  mutate(weather = revalue(weather, c("1" = "Clear", "2" = "Light precipitation", "3" = "Heavy precipitation")))

#Adding only the season variable to the equation

bikes_mod4 <- lm(data = bikes2, rentals ~ humidity + windspeed + temperature + humidity2 + temperature2 + season)

#View the new model summary statistics

summary(bikes_mod4)

#New model with interaction between windspeed and weather

bikes_mod5 <- lm(data = bikes2, rentals ~ humidity + temperature + humidity2 + temperature2 + season + windspeed * weather)

#View the new model summary statistics

summary(bikes_mod5)

#Separating date column into day, month, and year columns

library(lubridate)

bikes2 <- bikes2 %>%
  mutate(day = as.numeric(date-min(date))) %>%
  mutate(month = as.factor(month(date))) %>%
  mutate(year = as.factor(year(date))) %>%
  select(-date)

#Stepwise function to pick best model

ols_step_both_p(model = lm(data = bikes2, rentals ~ humidity + weekday + holiday + temperature + humidity2 + temperature2 + season + windspeed * weather +
                             realfeel + day + month + year), pent = .2, prem = .01, details = FALSE)

#Looking at new model using the selected variables

summary(lm(data = bikes2, rentals ~ windspeed * weather + month + weekday + season + holiday + temperature2 + temperature +
             year + windspeed + humidity + humidity2))

#CASE STUDY: PREDICTING BLOOD PRESSURE

#Loading the health data

library(tidyverse)

health <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 4/Data/health.csv")

#Brief overview of health data

glimpse(health)

#Changing diabetes and smoker from numeric to factor

health %>%
  mutate(diabetes = as.factor(diabetes)) %>%
  mutate(smoker = as.factor(smoker))

#Exploring summary data

summary(health)

#Histogram of systolic variable

health %>%
  ggplot() +
  geom_histogram(mapping = aes(x = systolic), fill = "lightblue", color = "black")

#Looking at the distribution of the predictor variables

health %>%
  select(-systolic) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x = value, fill = key), color = "black") +
  facet_wrap(~ key, scales = "free")

#Checking correlation between variables

cor(health[,c("systolic", "weight", "height", "bmi", "waist", "age", "fastfood")])

#Building model using age, since it's the highest correlation

health_mod1 <- lm(data = health, systolic ~ age)

#View model summary

summary(health_mod1)

#Fitting multiple regression model

health_mod2 <- lm(data = health, systolic ~ .)

#View summary

summary(health_mod2)

#Test for zero mean of residuals

mean(health_mod2$residuals)

#View histogram of residuals

library(olsrr)

ols_plot_resid_hist(health_mod2)

#Test for heteroskedasticity

ols_plot_resid_fit(health_mod2)

#Test for autocorrelation

library(car)

durbinWatsonTest(health_mod2)

#Check for influential data points

ols_plot_cooksd_chart(health_mod2)

#Check data point 1358 and compare to the entire data set

health[1358,]

summary(health)

#List the outliers

outlier_index <- as.numeric(unlist(ols_plot_cooksd_chart(health_mod2)$outliers[, "observation"]))

outlier_index

#View the summary of outliers

summary(health[outlier_index,])

#Create a new data set excluding outliers

health2 <- health[-outlier_index,]

#Check for multicollinearity

ols_vif_tol(health_mod2)

#Build a new model dropping several of the variables

health_mod3 <- lm(data = health2, systolic ~ weight + age + diabetes)

#View new model summary

summary(health_mod3)

#Create two more predictors

health2 <- health2 %>%
  mutate(age2 = age^2) %>%
  mutate(log_age = log(age))

#Building a new model with interactions and non-linear predictors

ols_step_both_p(model = lm(data=health2, systolic ~ weight * diabetes + age * diabetes + age2 * diabetes + log_age * diabetes),
                pent = .2, prem = .01, details = FALSE)