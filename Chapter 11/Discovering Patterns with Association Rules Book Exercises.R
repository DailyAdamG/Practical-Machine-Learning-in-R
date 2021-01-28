#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 11")

#Importing the data

library(arules)

groceries <- read.transactions("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 11/Data/groceries.csv", sep =",")

#Exercise 3

#Continue to explore the Groceries data set presented in the case study of this
#chapter. Answer the following questions:

#a) What are the 10 least frequently purchased items?

#Find the frequency for each item and create a tibble

library(tidyverse)

groceries_frequency <- tibble(Items = names(itemFrequency(groceries)),
                              Frequency = itemFrequency(groceries))

#The ten least frequent items

groceries_frequency %>%
  arrange(Frequency) %>%
  slice(1:10)

#b) If you change the minimum rule length to 3, how many rules do you generate?
#What if you change it to 4?

#Generate the rules

groceryrules <- apriori(groceries, parameter = list(support = .015, confidence = .25, minlen = 3))

summary(groceryrules)

#There are 16 rules using minimum rule length of three.

groceryrules <- apriori(groceries, parameter = list(support = .015, confidence = .25, minlen = 4))

summary(groceryrules)

#There are no rules using minimum rule length of four.

#c) Change the minimum rule length back to 2 and produce a list of rules involving
#either soda or whipped/sour cream.

#Generate the rules

groceryrules <- apriori(groceries, parameter = list(support = .015, confidence = .25, minlen = 2))

groceryrules %>%
  subset(items %in% c("soda", "whipped/sour cream")) %>%
  sort(by = "lift") %>%
  inspect()