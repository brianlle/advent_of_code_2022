### Advent of Code 2022: Day 2

library(tidyverse)

data <- read.table("input/day2.txt", sep = " ")

score <- 0

get_score <- function(left, right){
  if (left == "A"){
    if (right == "X"){
        shape_score = 1
        outcome_score = 3
      } else if (right == "Y"){
        shape_score = 2
        outcome_score = 6
      } else {
        shape_score = 3
        outcome_score = 0
      }
  } else if (left == "B"){
    if (right == "X"){
      shape_score = 1
      outcome_score = 0
    } else if (right == "Y"){
      shape_score = 2
      outcome_score = 3
    } else {
      shape_score = 3
      outcome_score = 6
    }
  } else {
    if (right == "X"){
      shape_score = 1
      outcome_score = 6
    } else if (right == "Y"){
      shape_score = 2
      outcome_score = 0
    } else {
      shape_score = 3
      outcome_score = 3
    }
  }
  return(shape_score + outcome_score)
}


score <- 0
for (i in 1:nrow(data)){
  score <- score + get_score(data[i,1], data[i,2])
}

# Part 2
# X = lose, Y = draw, Z = win

choose_shape <- function(left, outcome){
  if (left == "A"){
    if (outcome == "X"){
      right <- "Z"
    } else if (outcome == "Y"){
      right <- "X"
    } else {
      right <- "Y"
    }
  } else if (left == "B"){
    if (outcome == "X"){
      right <- "X"
    } else if (outcome == "Y"){
      right <- "Y"
    } else {
      right <- "Z"
    }
  } else {
    if (outcome == "X"){
      right <- "Y"
    } else if (outcome == "Y"){
      right <- "Z"
    } else {
      right <- "X"
    }
  }
  
  return(right)
}

score <- 0
for (i in 1:nrow(data)){
  shape <- choose_shape(data[i,1], data[i,2])
  score <- score + get_score(data[i,1], shape)
}
