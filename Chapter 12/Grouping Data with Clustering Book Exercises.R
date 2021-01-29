#Setting directory

setwd("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 12")

#Importing the data

library(tidyverse)

college <- read_csv("C:/Users/daily/Desktop/Repositories/Practical-Machine-Learning-in-R/Chapter 12/Data/college.csv", col_types = "nccfffffnnnnnnnnn")

#Exercise 1

#Using the college data set from this chapter, perform clustering that looks at the
#average faculty salary and annual tuition rates for schools located in Indiana.
#Choose k=3 and produce a visualization of your clusters.

#Limit analysis to only Indiana colleges and the two features and scale the data

indiana_college <- college %>%
  filter(state == "IN") %>%
  column_to_rownames(var = "name") %>%
  select(faculty_salary_avg, tuition) %>%
  scale()

#Clustering the data using kmeans function

library(stats)

set.seed(1234)

k_3 <- kmeans(indiana_college, centers = 3, nstart = 25)

#View size and centers

k_3$size

k_3$centers

#Visualize the clusters

library(factoextra)

fviz_cluster(k_3, data = indiana_college, main = "Indiana Colleges Segmented by Faculty Salary and Tuition", repel = TRUE)

#Exercise 2

#Use the techniques described in this chapter to select two possible optimal values
#for k for the clustering problem you coded in Exercise 1. Justify your answer.

#Determine the optimal number of clusters using the elbow method

fviz_nbclust(indiana_college, kmeans, method = "wss")

#Determine the optimal number of clusters using the average silhouette method

fviz_nbclust(indiana_college, kmeans, method = "silhouette")

#Determine the optimal number of clusters using the gap statistic

fviz_nbclust(indiana_college, kmeans, method = "gap_stat")

#I choose four as the first optimal k value, since there appears to be the start of an elbow and
#the silhouette method seems to agree as well. I will choose nine for the second value, because of 
#the high gap statistic. I am skeptical that nine clusters will work given that there are so few colleges.

#Exercise 3

#Generate cluster diagrams for the two values of k that you selected in Exercise 2.
#Which one of these do you believe is the best result? Why?

#Creating clusters

set.seed(1234)

k_4 <- kmeans(indiana_college, centers = 4, nstart = 25)

k_9 <- kmeans(indiana_college, centers = 9, nstart = 25)

#Creating visuals

library(factoextra)

fviz_cluster(k_4, data = indiana_college, main = "Indiana Colleges Segmented by Faculty Salary and Tuition", repel = TRUE)

fviz_cluster(k_9, data = indiana_college, main = "Indiana Colleges Segmented by Faculty Salary and Tuition", repel = TRUE)

#I believe that four clusters is a better result, because there are not enough 
#colleges in each cluster when there are nine clusters.