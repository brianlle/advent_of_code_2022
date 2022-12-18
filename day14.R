# Advent of Code 2022, Day 14

library(tidyverse)

# pouring in from 500,0
# sand falls down, then down-left, then down-right

# data <- readLines("input/day14.txt") %>% 
#   str_split('', simplify = TRUE)
# 
# data <- matrix((data))

data <- readLines("input/day14.txt")

check_no_movement <- function(matrix, coords){
  x <- coords[1]
  y <- coords[2]
  if (matrix[y+1,x] >= 1){
     if (matrix[y+1,x-1] >= 1){
       if (matrix[y+1,x+1] >= 1){
         return(TRUE)
       }
     }
  }
  return(FALSE)
}

increment_sand <- function(matrix, coords){
  x <- coords[1]
  y <- coords[2]
  
  if (matrix[y+1,x] == 0){
    coords <- c(x,y+1)
  } else if (matrix[y+1,x-1] == 0){
    coords <- c(x-1,y+1)
  } else if (matrix[y+1,x+1] == 0){
    coords <- c(x+1, y+1)
  } else {
    print("help")
  }
  
  return(coords)
}

add_line_to_matrix <- function(matrix, line){
  coords <- strsplit(line, split = " -> ")[[1]]
  cur_x <- as.numeric(gsub(",.*", "", coords[1]))
  cur_y <- as.numeric(gsub(".*,", "", coords[1]))
  for (i in 2:(length(coords))){
    to_x <- as.numeric(gsub(",.*", "", coords[i]))
    to_y <- as.numeric(gsub(".*,", "", coords[i]))
    
    matrix[cur_y:to_y, cur_x:to_x] <- 2
    
    cur_x <- to_x
    cur_y <- to_y
  }
  
  return(matrix)
}

# max dimensions such that we don't worry about sand leaving
# probably would've been cleaner to use a dict() from collections library
matrix <- matrix(0, nrow = 205, ncol = 600)
for (i in 1:length(data)){
  matrix <- add_line_to_matrix(matrix, data[i])
}

coord <- c(500,1)
sand_count <- 0
while(TRUE){
  coord <- increment_sand(matrix, coord)
  
  if (coord[2] > 200){
    break
  }
  
  if (check_no_movement(matrix, coord)){
    matrix[coord[2], coord[1]] <- 1
    coord <- c(500,1)
    sand_count <- sand_count + 1
    print(sand_count)
  }
}

## part 2

# max is 167, floor is at 169
# need to adjust matrix because sand starts at 0, so add 1 to all y
# such that floor is at 170

add_line_to_matrix <- function(matrix, line){
  coords <- strsplit(line, split = " -> ")[[1]]
  cur_x <- as.numeric(gsub(",.*", "", coords[1]))
  cur_y <- as.numeric(gsub(".*,", "", coords[1])) + 1
  for (i in 2:(length(coords))){
    to_x <- as.numeric(gsub(",.*", "", coords[i]))
    to_y <- as.numeric(gsub(".*,", "", coords[i])) + 1
    
    matrix[cur_y:to_y, cur_x:to_x] <- 2
    
    cur_x <- to_x
    cur_y <- to_y
  }
  
  return(matrix)
}


matrix <- matrix(0, nrow = 170, ncol = 800)
for (i in 1:length(data)){
  matrix <- add_line_to_matrix(matrix, data[i])
}
matrix[170, 1:800] <- 2

coord <- c(500,1)
sand_count <- 0
while(TRUE){
  coord <- increment_sand(matrix, coord)
  
  if (matrix[1, 500] == 1){
    break
  }
  
  if (check_no_movement(matrix, coord)){
    matrix[coord[2], coord[1]] <- 1
    coord <- c(500,1)
    sand_count <- sand_count + 1
    print(sand_count)
  }
}