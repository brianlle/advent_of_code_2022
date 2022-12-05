# Advent of Code 2022, Day 3

library(tidyverse)

data <- readLines("input/day3.txt")

shared <- c()
for (i in 1:length(data)){
  cur_data <- data[i]
  
  left <- substr(cur_data, 1, nchar(cur_data)/2)
  right <- substr(cur_data, nchar(cur_data)/2 + 1, nchar(cur_data))
  
  left <- unique(strsplit(left, split = ""))
  right <- unique(strsplit(right, split = ""))
  
  shared_char <- intersect(left[[1]], right[[1]])
  
  shared <- c(shared, shared_char)
}

# create a wannabe dict
mapping <- data.frame(letter = c(letters[1:26], LETTERS[1:26]),
                      value = 1:52)

total <- 0
for (i in 1:length(shared)){
  total <- total + mapping[mapping$letter == shared[i], "value"]
}

print(total)

#part 2

badges <- c()
for (i in 0:(length(data)/3-1)){
  row1 <- data[3*i + 1]
  row2 <- data[3*i + 2]
  row3 <- data[3*i + 3]
  
  
  row1 <- unique(strsplit(row1, split = ""))
  row2 <- unique(strsplit(row2, split = ""))
  row3 <- unique(strsplit(row3, split = ""))
  
  shared_char <- intersect(row1[[1]], row2[[1]])
  shared_char <- intersect(shared_char, row3[[1]])
  
  badges <- c(badges, shared_char)
}

total2 <- 0
for (i in 1:length(badges)){
  total2 <- total2 + mapping[mapping$letter == badges[i], "value"]
}

print(total2)