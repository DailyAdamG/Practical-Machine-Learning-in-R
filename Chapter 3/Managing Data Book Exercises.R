#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 3")

#Loading necessary library and dataset

library(tidyverse)

vehicles <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 3/Data/vehicles.csv", col_types = "nnnfnfffffnn")

#Exercise 1

#For all manual transmission vehicles in the vehicles data set, list the 
#descriptive statistics for the drive, make, model, and class variables only.

vehicles %>%
  filter(transmissiontype == "Manual") %>%
  select(drive, make, model, class) %>% 
  summary()

#Exercise 2

#Using the min-max normalization approach, normalize the values of the co2emissions
#variable in the vehicles data set so that they fall between the values of 1 and 10.
#Show the descriptive statistics for the original and normalized variables.

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_n = ((co2emissions - min(co2emissions))/ (max(co2emissions) - min(co2emissions))) * (10 - 1) + 1) %>%
  summary()

#Exercise 3

#In the vehicles data set, discretize the co2emissions variable using the High 
#value for emission levels at or above 500 grams per mile and Low for emission
#levels below this mark. Using the discretized variable for the strata, generate
#a stratified random sample of 2 percent of the data set. Show the proportional
#distribution of values for the discretized variable for the original popualation
#and the sample.

vehicle_emissions <- vehicles %>%
  mutate(co2emission_level = ifelse(co2emissions >= 500, "High", "Low"))

library(caTools)

set.seed(1234)

sample_set <- sample.split(vehicle_emissions$co2emission_level, SplitRatio = .02)

stratified_vehicle_emissions <- subset(vehicle_emissions, sample_set == TRUE)

vehicle_emissions %>%
  select(co2emission_level) %>%
  table() %>%
  prop.table()

stratified_vehicle_emissions %>%
  select(co2emission_level) %>%
  table() %>%
  prop.table()