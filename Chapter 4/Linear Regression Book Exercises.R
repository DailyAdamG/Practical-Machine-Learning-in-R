#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 4")

#Loading necessary library and data set

library(tidyverse)


health <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 4/Data/health.csv")

#Exercise 2

#Using the blood pressure data set from the use case in this chapter, produce a
#correlation plot. Use the corrplot.mixed function and generate a plot that shows
#the correlation coefficients visually above the diagonal and numerically below
#the diagonal. Provide an interpretaion of your results.

library(corrplot)

health_cor <- cor(health)

corrplot.mixed(health_cor)

#Exercise 3

#You are working with college admission data and trying to determine whether you
#can predict a student's future GPA based upon their college admission test score.
#The test is scored on a scale of 0-100, while GPA is measured on a scale of 0.0-4.0.
#When you build your regression model, you receive the following results:

#Coefficients: Intercept = .695, test = .033
#Adj. R-squared: .7958

#a) According to this model, what impact would a single point increase in admissions
#test score have on the prediction of a student's GPA?

#A single point increase in test score would increase the GPA by .033

#b) If a student scored 82 on the test, what would be your prediction of their GPA?

.695 + .033 * 82

#c) If another student scored 97 on the admissions test, what would be your 
#prediction of their GPA?

.695 + .033 * 97

#d) How well does the model fit the data based upon the Adjusted R-squared?

# The model explains over 79% of the variability in the GPA. This means that the
#model is a good fit.

#Exercise 4

#Returning to the bicycle rental data set, use R to create a simple regression 
#model designed to predict the realfell temperature based upon the air temperature.
#Explain your model and describe how well if fits the data.

bikes <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 4/Data/bikes.csv", col_types = "Dffffddddd")

rf_model <- lm(data = bikes, realfeel ~ temperature)

summary(rf_model)

#The model uses temperature to predict the realfeel temperature. With an 
#Adjusted R-squared of .9834, the model explains over 98% of the variability in temperature.

#Exercise 5

#After building the regression model in exercise 3, you return to the same data set
#and want to know whether the age of a student at application time is also a contributing
#factor to their GPA. You add this element to a multiple regression model and receive
#the results shown here:

#Coefficients: Intercept = -1.900439, test = .025702, age = .182456
#Adj. R-squared: .9059

#a) According to this model, what impact would a single point increase in admissions
#test score have on the prediction of a student's GPA? How about a single year
#increase in age?

#A single point increase in test score would increase the GPA by .025702. A single
#year increase in age would increase the GPA by .182456.

#b) If a student scored 82 on the admissions test and was 17 years old at the time
#of application, what would be your prediction of their GPA?

-1.900439 + .025702 * 82 + .182456 * 17

#c) If another student scored 97 on the admissions test and was 19 years old at the 
#time of application, what would be your prediction of their GPA?

-1.900439 + .025702 * 97 + .182456 * 19

#d) How well does this model fit the data based upon the Adjusted R-squared? How
#does that compare to the model from exercise 3?

# The model explains over 90% of the variability in the GPA. This means that the
#model is a very good fit and a better fit than the model from exercise 3.

#Exercise 6

#Returning to the bicycle rental data set, convert your simple regression model
#from exercise 4 to a multiple regression model that predicts realfeel based upon
#temperature, windspeed, and humidity. Explain your model and describe how well it
#fits the data, compared to the model you created in exercise 4.

rf_model2 <- lm(data = bikes, realfeel ~ temperature + windspeed + humidity)

summary(rf_model2)

#This model demonstrates that windspeed also plays a role in realfeel with increasing
#winds causing realfeel temperature to decrease. The humidity variable is not under
# the .05 threshold. The Adjusted R-squared is just slightly higher than the value
#found in exercise 4. This means that this model is a bit better than the previous one.
