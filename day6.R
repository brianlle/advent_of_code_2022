# Advent of Code 2022, Day 6

library(tidyverse)

data <- readLines("input/day6.txt")

for (cur_pos in 4:nchar(data)){
  chars <- substr(data, cur_pos-3, cur_pos)
  chars <- strsplit(chars, split = "")[[1]]
  if (length(table(chars)) == 4){
    break
  }
  cur_pos <- cur_pos + 1
}

print(cur_pos)

# part 2

for (cur_pos in 14:nchar(data)){
  chars <- substr(data, cur_pos-13, cur_pos)
  chars <- strsplit(chars, split = "")[[1]]
  if (length(table(chars)) == 14){
    break
  }
  cur_pos <- cur_pos + 1
}

print(cur_pos)