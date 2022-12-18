# Advent of Code 2022, Day 11


library(tidyverse)
library(stringr)
data <- readLines("input/day11.txt")

num_monkeys <- ceiling(length(data)/7)

monkeys_held <- list()
monkey_operations <- c()
monkey_test <- c() # tests are always divisible by factor
monkey_true_to <- c()
monkey_false_to <- c()
for (i in 1:num_monkeys){
  start_i <- 7*i - 6
  monkeys_held[[i]] <- as.numeric(strsplit(gsub(".*: ", "", data[start_i + 1]), split = ", ")[[1]])
  monkey_operations <- c(monkey_operations, 
                         data[start_i + 2])
  monkey_test <- c(monkey_test, 
                   parse_number(data[start_i + 3]))
  monkey_true_to <- c(monkey_true_to, 
                      parse_number(data[start_i + 4]) + 1)
  monkey_false_to <- c(monkey_false_to, 
                       parse_number(data[start_i + 5]) + 1)
}

worry_update <- function(worry, operation){
  if (str_detect(operation, "\\+")){
    worry <- worry + parse_number(operation)
  } else if (str_detect(operation, "old \\* old")) {
    worry <- worry * worry
  } else {
    worry <- worry * parse_number(operation)
  }
  
  worry <- floor(worry / 3)
  return(worry)
}

monkey_inspect_count <- rep(0, 8)
for (round in 1:20){
  for (i in 1:num_monkeys){
    if (length(monkeys_held[[i]]) == 0){
      next
    }
    for (item in 1:length(monkeys_held[[i]])){
      monkey_inspect_count[i] <- monkey_inspect_count[i] + 1
      monkeys_held[[i]][item] <- worry_update(monkeys_held[[i]][item], monkey_operations[i])
      if ((monkeys_held[[i]][item] %% monkey_test[i]) == 0){ #test is true
        monkeys_held[[monkey_true_to[i]]] <- c(monkeys_held[[monkey_true_to[i]]], 
                                               monkeys_held[[i]][item])
      } else { #test is false
        monkeys_held[[monkey_false_to[i]]] <- c(monkeys_held[[monkey_false_to[i]]], 
                                                monkeys_held[[i]][item])
      }
    }
    monkeys_held[[i]] <- numeric()
  }
}

monkey_inspect_count <- sort(monkey_inspect_count, decreasing = TRUE)
print(monkey_inspect_count[1] * monkey_inspect_count[2])

## part 2

worry_update <- function(worry, operation, mod_value){
  
  if (str_detect(operation, "\\+")){
    worry <- worry + parse_number(operation)
  } else if (str_detect(operation, "old \\* old")) {
    worry <- worry * worry
  } else {
    worry <- worry * parse_number(operation)
  }
  
  worry <- worry %% prod(mod_value)
  #worry <- floor(worry / 3)
  
  return(worry)
}

monkeys_held <- list()
monkey_operations <- c()
monkey_test <- c() # tests are always divisible by factor
monkey_true_to <- c()
monkey_false_to <- c()
for (i in 1:num_monkeys){
  start_i <- 7*i - 6
  monkeys_held[[i]] <- as.numeric(strsplit(gsub(".*: ", "", data[start_i + 1]), split = ", ")[[1]])
  monkey_operations <- c(monkey_operations, 
                         data[start_i + 2])
  monkey_test <- c(monkey_test, 
                   parse_number(data[start_i + 3]))
  monkey_true_to <- c(monkey_true_to, 
                      parse_number(data[start_i + 4]) + 1)
  monkey_false_to <- c(monkey_false_to, 
                       parse_number(data[start_i + 5]) + 1)
}

monkey_inspect_count <- rep(0, 8)
for (round in 1:10000){
  print(round)
  for (i in 1:num_monkeys){
    if (length(monkeys_held[[i]]) == 0){
      next
    }
    for (item in 1:length(monkeys_held[[i]])){
      monkey_inspect_count[i] <- monkey_inspect_count[i] + 1
      monkeys_held[[i]][item] <- worry_update(monkeys_held[[i]][item], monkey_operations[i], monkey_test)
      if ((monkeys_held[[i]][item] %% monkey_test[i]) == 0){ #test is true
        monkeys_held[[monkey_true_to[i]]] <- c(monkeys_held[[monkey_true_to[i]]], 
                                               monkeys_held[[i]][item])
      } else { #test is false
        monkeys_held[[monkey_false_to[i]]] <- c(monkeys_held[[monkey_false_to[i]]], 
                                                monkeys_held[[i]][item])
      }
    }
    monkeys_held[[i]] <- numeric() # monkey throws all items
  }
}

monkey_inspect_count <- sort(monkey_inspect_count, decreasing = TRUE)
print(monkey_inspect_count[1] * monkey_inspect_count[2])
