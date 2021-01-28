#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 11")

#Loading data

library(arules)

supermart <- read.transactions("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 11/Data/retail.txt", sep ="")

#View the data summary

summary(supermart)

#View the first five transactions in the data

inspect(supermart[1:5])

#Check the frequency of item 39

itemFrequency(supermart[, "39"])

#Create a tibble, so we can have a more standard data frame

library(tidyverse)

supermart_frequency <- tibble(Items = names(itemFrequency(supermart)),
                              Frequency = itemFrequency(supermart))

#View the first six rows of the newly created tibble

head(supermart_frequency)

#View the ten most popular items purchased

supermart_frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

#Build the association rules using the apriori function

supermartrules <- apriori(supermart, parameter = list(support = .0085, confidence = .5, minlen = 2))

#View the results summary

summary(supermartrules)

#View the first ten rules

inspect(supermartrules[1:10])

#View top ten rules in terms of lift

supermartrules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()

#View all rules that contain item 41

supermartrules %>%
  subset(items %in% "41") %>%
  inspect()

#View the top ten rules in terms of lift for item 41

supermartrules %>%
  subset(items %in% "41") %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()

#CASE STUDY: IDENTIFYING GROCERY PURCHASE PATTERNS

#Importing the data

library(arules)

groceries <- read.transactions("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 11/Data/groceries.csv", sep =",")

#View summary data

summary(groceries)

#Find the frequency for each item and create a tibble

library(tidyverse)

groceries_frequency <- tibble(Items = names(itemFrequency(groceries)),
                              Frequency = itemFrequency(groceries))

#View the ten most frequent items

groceries_frequency %>%
  arrange(desc(Frequency)) %>%
  slice(1:10)

#View summary for item frequency

groceries_frequency %>%
  select(Frequency) %>%
  summary()

#Generate the rules

groceryrules <- apriori(groceries, parameter = list(support = .015, confidence = .25, minlen = 2))

#View rules summary

summary(groceryrules)

#View top ten rules in terms of confidence

groceryrules %>%
  sort(by = "confidence") %>%
  head(n = 10) %>%
  inspect()

#View top ten rules in terms of lift

groceryrules %>%
  sort(by = "lift") %>%
  head(n = 10) %>%
  inspect()

#Look for associations with less popular items, since whole milk and other vegetables
#are so prevalent

groceryrules %>%
  subset(!items %in% c("whole milk", "other vegetables")) %>%
  sort(by = "lift") %>%
  inspect()