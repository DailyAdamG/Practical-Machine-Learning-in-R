#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 12")

#Importing the data

library(tidyverse)

college <- read_csv("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 12/Data/college.csv", col_types = "nccfffffnnnnnnnnn")

#View the data

glimpse(college)

#Limit analysis to only Maryland colleges

maryland_college <- college %>%
  filter(state == "MD") %>%
  column_to_rownames(var = "name")

#Limit analysis to two features, admission_rate and sat_avg

maryland_college %>%
  select(admission_rate, sat_avg) %>%
  summary()

#Need to normalize the data, because we are dealing with distance algorithms

maryland_college_scaled <- maryland_college %>%
  select(admission_rate, sat_avg) %>%
  scale()

#View the summary statistics

summary(maryland_college_scaled)

#Clustering the data using kmeans function

library(stats)

set.seed(1234)

k_3 <- kmeans(maryland_college_scaled, centers = 3, nstart = 25)

#View the number of observations in each cluster

k_3$size

#View the centers of the three clusters

k_3$centers

#Visualize the clusters

library(factoextra)

fviz_cluster(k_3, data = maryland_college_scaled, repel = TRUE)

#See how other attributes vary across clusters

maryland_college %>%
  mutate(cluster = k_3$cluster) %>%
  select(cluster, undergrads, tuition, faculty_salary_avg, loan_default_rate, median_debt) %>%
  group_by(cluster) %>%
  summarise_all("mean")

#Determine the optimal number of clusters using the elbow method

fviz_nbclust(maryland_college_scaled, kmeans, method = "wss")

#Determine the optimal number of clusters using the average silhouette method

fviz_nbclust(maryland_college_scaled, kmeans, method = "silhouette")

#Determine the optimal number of clusters using the gap statistic

fviz_nbclust(maryland_college_scaled, kmeans, method = "gap_stat")

#Clustering the data using 4 clusters

k_4 <- kmeans(maryland_college_scaled, centers = 4, nstart = 25)

#Visualize the clusters

fviz_cluster(k_4, data = maryland_college_scaled, 
main = "Maryland Colleges Segmented by SAT Scores and Admission Rates", repel = TRUE)

#CASE STUDY: SEGMENTING SHOPPING MALL CUSTOMERS

#Importing the data

library(tidyverse)

mallcustomers <- read_csv("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 12/Data/mallcustomers.csv")

#View the data

glimpse(mallcustomers)

#Turn Income variable into a numeric column

library(stringr)

mallcustomers <- mallcustomers %>%
  mutate(Income = str_replace_all(Income, " USD", "")) %>%
  mutate(Income = str_replace_all(Income, ",", "")) %>%
  mutate(Income = as.numeric(Income))

#View summary of data

summary(mallcustomers)

#Need to standardize the data

mallcustomers_scaled <- mallcustomers %>%
  select(-CustomerID, -Gender, -Age) %>%
  scale()

#View summary of data

summary(mallcustomers_scaled)

#Determine the optimal number of clusters using three common approaches

fviz_nbclust(mallcustomers_scaled, kmeans, method = "wss")

fviz_nbclust(mallcustomers_scaled, kmeans, method = "silhouette")

fviz_nbclust(mallcustomers_scaled, kmeans, method = "gap_stat")

#k=6 seems to be the appropriate choice.

set.seed(1234)

k_clust <- kmeans(mallcustomers_scaled, centers = 6, nstart = 25)

#Visualize the clusters

fviz_cluster(k_clust, data = mallcustomers_scaled, 
             main = "Mall Customers Segmented by Income and Spending Score", repel = TRUE)

#Evaluate the clusters by gender and age

mallcustomers %>%
  mutate(cluster = k_clust$cluster) %>%
  mutate(Male = ifelse(Gender == "Male", 1, 0)) %>%
  mutate(Female = ifelse(Gender == "Female", 1, 0)) %>%
  select(cluster, Male, Female, Age) %>%
  group_by(cluster) %>%
  summarise_all("mean")