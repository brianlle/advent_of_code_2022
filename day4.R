# Advent of Code 2022, Day 4

library(tidyverse)

data <- read.csv("input/day4.txt", header = FALSE)

# Original solution using for-loop
# num <- 0
# for (i in 1:nrow(data)){
#   set1 <- data[i,1]
#   set2 <- data[i,2]
#   
#   set11 <- gsub("-.*", "", set1)
#   set12 <- gsub(".*-", "", set1)
#   set21 <- gsub("-.*", "", set2)
#   set22 <- gsub(".*-", "", set2)
#   
#   if (all(set11:set12 %in% set21:set22) | all(set21:set22 %in% set11:set12)){
#     num <- num + 1
#   }
# }
# 
# print(num)

# Cleaner solution adding columns to dataframe

data$s11 <- gsub("-.*", "", data[,1])
data$s12 <- gsub(".*-", "", data[,1])
data$s21 <- gsub("-.*", "", data[,2])
data$s22 <- gsub(".*-", "", data[,2])

data$subset <- apply(data, 1, 
                     function(x){all(x[3]:x[4] %in% x[5]:x[6]) | all(x[5]:x[6] %in% x[3]:x[4])})

print(sum(data$subset))

# part 2

# num <- 0
# 
# for (i in 1:nrow(data)){
#   set1 <- data[i,1]
#   set2 <- data[i,2]
#   
#   set11 <- gsub("-.*", "", set1)
#   set12 <- gsub(".*-", "", set1)
#   
#   set21 <- gsub("-.*", "", set2)
#   set22 <- gsub(".*-", "", set2)
#   
#   if (length(intersect(set11:set12, set21:set22)) > 0){
#     num <- num + 1
#   }
# }
# 
# print(num)

data$intersect <- apply(data, 1, 
                     function(x){length(intersect(x[3]:x[4], x[5]:x[6])) > 0})

print(sum(data$intersect))
