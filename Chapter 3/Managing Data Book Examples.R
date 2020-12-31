#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 3")

#Loading necessary library and dataset

library(tidyverse)

vehicles <- read_csv(file = "C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 3/Data/vehicles.csv", col_types = "nnnfnfffffnn")

#Double-checking the data types to ensure they are the same as the book

glimpse(vehicles)

#Getting data summary

summary(vehicles)

#View only the class column

select(vehicles, class)

#View both class and cylinders column

select(vehicles, class, cylinders)

#Getting summary of class and cylinder columns

summary(select(vehicles, class, cylinders))

#Get a complete list of values and counts for the class column

table(select(vehicles, class))

#Find the proportional distribution instead of the count

prop.table(table(select(vehicles,class)))

#Rewriting the code to use pipes

vehicles %>%
  select(class) %>%
  table() %>%
  prop.table()

#Filter to only include 2-Wheel Drive

vehicles %>%
  filter(drive == "2-Wheel Drive") %>%
  select(co2emissions) %>%
  summary()

#Creating a box plot of CO2 emissions by class

vehicles %>%
  ggplot() +
  geom_boxplot(mapping = aes(x = class, y = co2emissions), fill = "red") +
  labs(title = "Boxplot of CO2 Emissions by Vehicle Class", x = "Class", y = "CO2 Emissions")

#Creating a scatter plot of  CO2 emissions by City Miles per Gallon

vehicles %>%
  ggplot() +
  geom_point(mapping = aes(x = citympg, y = co2emissions), color = "blue", size = 2) +
  labs(title = "Scatterplot of CO2 Emissions vs. City Miles per Gallon", x = "City MPG", y = "CO2 Emissions")

#Creating a histogram of CO2 emissions

vehicles %>%
  ggplot() +
  geom_histogram(mapping = aes(x = co2emissions), bins = 30, fill = "yellow", color = "black") +
  labs(title = "Histogram of CO2 Emissions", x = "CO2 Emissions", y = "Frequency")

#Creating a stacked bar chart of drive type by model year

vehicles %>%
  ggplot() +
  geom_bar(mapping = aes(x = year, fill = drive), color = "black") +
  labs(title = "Stacked Bar Chart of Drive Type Composition by Year", x = "Model Year", y = "Number of Cars") +
  coord_flip()

# Summary statistics for city mpg, displacement, and highway mpg

vehicles %>%
  select(citympg, displacement, highwaympg) %>%
  summary()

#Replacing missing values with median imputation

vehicles <- vehicles %>%
  mutate(citympg = ifelse(is.na(citympg), median(citympg, na.rm = TRUE), citympg)) %>%
           mutate(highwaympg = ifelse(is.na(highwaympg), median(highwaympg, na.rm = TRUE), highwaympg))

#Replacing missing values with mean imputation

vehicles <- vehicles %>%
  mutate(displacement = ifelse(is.na(displacement), mean(displacement, na.rm = TRUE), displacement))

#Double-checking the summary statistics to ensure there are no longer any missing values

vehicles %>%
  select(citympg, displacement, highwaympg) %>%
  summary()

#Summary statistics for CO2 emissions

vehicles %>%
  select(co2emissions) %>%
  summary()

#Create a normalized variable of co2emissions by decimal scaling method

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_d = co2emissions / (10^4)) %>%
  summary()

#Create a normalized variable of co2emissions by Z-score method

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_z = (co2emissions - mean(co2emissions)) / sd(co2emissions)) %>%
  summary()

# Create a normalized variable of co2emissions by Min-Max method

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_n = ((co2emissions - min(co2emissions))/ (max(co2emissions) - min(co2emissions))) * (1 - 0) + 0) %>%
  summary()

# Create a normalized variable of co2emissions by Log Transformation method

vehicles %>%
  select(co2emissions) %>%
  mutate(co2emissions_b = log10(co2emissions)) %>%
  summary()

#View the frequency of different drive classifications

vehicles %>%
  select(drive) %>%
  summary()