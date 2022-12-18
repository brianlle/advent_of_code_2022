# Advent of Code 2022 - Day 8

library(tidyverse)

data <- matrix(as.numeric(readLines("input/day8.txt") %>% 
  str_split('', simplify = TRUE)), ncol = 99, nrow = 99)

visible_top <- function(data, nrotate){
  for (idx in 1:nrotate){
    data <- rotate(data)
  }
  matrix_seen <- matrix(0, ncol = ncol(data), nrow = nrow(data))
  for (i in 1:nrow(data)){
    current_column <- data[i,]
    
    current_max <- 0
    for (j in 1:ncol(data)){
      if (data[i,j] > current_max | j == 1 | j == ncol(data) | i == 1 | i == nrow(data)){
        matrix_seen[i,j] <- 1
        current_max <- data[i,j]
      }
    }
  }
  
  if(nrotate != 4){
    for (idx in 1:(4-nrotate)){
      matrix_seen <- rotate(matrix_seen)
    }
  }
  return(matrix_seen)
}

rotate <- function(x) t(apply(x, 2, rev))

visible <- visible_top(data, 1) +
  visible_top(data, 2) +
  visible_top(data, 3) +
  visible_top(data, 4)
length(visible[visible > 0])

### part 2

# assume no edge trees, search each direction]=
get_scenic_score <- function(data, tree_x, tree_y){
  home_tree <- data[tree_x, tree_y]
  x1_score <- 0
  x2_score <- 0
  y1_score <- 0
  y2_score <- 0
  
  for (i in (tree_x-1):1){
      x1_score <- x1_score + 1
      if (data[i, tree_y] >= home_tree){
        break
      }
  }
  
  for (i in (tree_x+1):ncol(data)){
      x2_score <- x2_score + 1
      if (data[i, tree_y] >= home_tree){
        break
      }
  }
  
  for (i in (tree_y-1):1){
      y1_score <- y1_score + 1
      if (data[tree_x, i] >= home_tree){
      break
      }
  }
  
  for (i in (tree_y+1):ncol(data)){
      y2_score <- y2_score + 1
      if (data[tree_x, i] >= home_tree){
      break
      }
  }
  
  return(x1_score * x2_score * y1_score * y2_score)
}

max <- 0
for (i in 2:98){
  for (j in 2:98){
    max <- max(max, get_scenic_score(data, i, j))
  }
}
