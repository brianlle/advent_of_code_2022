# Advent of Code 2022, Day 22
library(tidyverse)


data <- readLines("input/day22.txt")

path <- data[length(data)]
path_turns <- strsplit(path, split = "")[[1]]
path_turns <- str_detect("[L|R]", path_turns)
path_turns <- seq.int(length(path_turns))[path_turns]

directions <- c()
cur_start <- 1
for (i in 1:length(path_turns)){
  dir <- substr(path, cur_start, path_turns[i]-1)
  dir2 <- substr(path, path_turns[i], path_turns[i])
  
  directions <- c(directions, dir, dir2)
  cur_start <- path_turns[i] + 1
}
directions <- c(directions, substring(path, cur_start))

tiles <- data[1:(length(data)-2)] %>% str_split('', simplify = TRUE)
tiles[tiles == ""] <- " "  #replace " " with ""

#test <- matrix(tiles, nrow= 12)

# start is left-most "." in [1,]


follow_dir <- function(dir, cur_x, cur_y, facing, tiles){
  
  # let's do 0 = right, 1 = down, 2 = left, 3 = up (from directions)
  if (dir == "R"){
    facing <- (facing + 1) %% 4
  } else if (dir == "L"){
    facing <- (facing - 1) %% 4
  } else {
    number_advance <- as.numeric(dir)
    
    for (idx in 1:number_advance){
      if (facing == 0){ #facing right, e.g. [1,12] is to the right of [1,11]
        
         x_check <- cur_x + 1
         if (x_check > ncol(tiles)){
           x_check <- 1
         }
         y_check <- cur_y
        
         while (tiles[y_check, x_check] == " "){
           x_check <- (x_check %% ncol(tiles)) + 1 # 1->2, ncol(tiles) -> 1
         }
        
         if (tiles[y_check, x_check] == "#"){
           break
         } else {
           cur_x <- x_check
           cur_y <- y_check
         }
         
      } else if (facing == 1){
        x_check <- cur_x
        y_check <- cur_y + 1
        if (y_check > nrow(tiles)){
          y_check <- 1
        }
        
        while (tiles[y_check, x_check] == " "){
          y_check <- (y_check %% nrow(tiles)) + 1 # 1->2, ncol(tiles) -> 1
        }
        
        if (tiles[y_check, x_check] == "#"){
          break
        } else {
          cur_x <- x_check
          cur_y <- y_check
        }
      } else if (facing == 2){
        
        x_check <- cur_x - 1
        if (x_check < 1){
          x_check <- ncol(tiles)
        }
        y_check <- cur_y
        
        while (tiles[y_check, x_check] == " "){
          x_check <- ((x_check - 2) %% ncol(tiles)) + 1 
        }
        
        if (tiles[y_check, x_check] == "#"){
          break
        } else {
          cur_x <- x_check
          cur_y <- y_check
        }
        
      } else if (facing == 3){
        
        x_check <- cur_x
        y_check <- cur_y - 1
        if (y_check < 1){
          y_check <- nrow(tiles)
        }
        
        while (tiles[y_check, x_check] == " "){
          y_check <- ((y_check - 2) %% nrow(tiles)) + 1 
        }
        
        if (tiles[y_check, x_check] == "#"){
          break
        } else {
          cur_x <- x_check
          cur_y <- y_check
        }
      }
    }
  }
  
  out_list <- c(cur_x, cur_y, facing)
  return(out_list)
  
}

# pos <- c(9,1) # for test
pos <- c(51,1) # storing x-index [,x] first, and y-index [y,] second
facing <- 0 # let's do 0 = up, 1 = right, 2 = down, 3 = left

for (i in 1:length(directions)){
  cur_x <- pos[1]
  cur_y <- pos[2]
  dir <- directions[i]
  
  out_list <- follow_dir(dir, cur_x, cur_y, facing, tiles)
  
  cur_x <- out_list[1]
  cur_y <- out_list[2]
  facing <- out_list[3]
  pos <- c(cur_x, cur_y)
  print(pos)
}

print(1000*cur_y + 4*cur_x + facing)

### Part 2

# need to identify the six cube-face boundaries and identify wrapping conditions
# for each of them...

# visually, from the input, cubes are left-to-right, top-to-bottom: (x range, y range)
# 1: 51-100, 1-50
# 2: 101-150, 1-50
# 3: 51-100, 51-100
# 4: 1-50, 101-150
# 5: 51-100, 101-150
# 6: 1-50, 151-200

#in order to avoid rotations, keep the tiles as currently presented, which preserves:
# left-right movement from 1 <> 2
# up-down movement from 1 <> 3 <> 5
# left-right movement fron 4 <> 5
# up-down movement from 4 <>6

# thinking about this in my head:
# 1 connects to 2/3 (preserved) and 4/6
# 1 left becomes 4 right, 1 up becomes 6 right
# 2 right becomes 5 left
# 3 R 2 U, 3 L 4 D

# need to be careful about order updating things

increment_dir <- function(cur_x, cur_y, facing, tiles){
  if (facing == 0){ # going to the right
    if (cur_x == 150 & cur_y %in% 1:50){ # 2 right becomes 5 left
      cur_y <- 150 - cur_y + 1
      cur_x <- 100
      facing <- 2
    } else if (cur_x == 100 & cur_y %in% 51:100){ # 3 right becomes 2 up
      cur_x <- cur_y + 50
      cur_y <- 50
      facing <- 3
    } else if (cur_x == 100 & cur_y %in% 101:150){ # 5 right becomes 2 left
      cur_y <- 150 - cur_y + 1
      cur_x <- 150
      facing <- 2
    } else if (cur_x == 50 & cur_y %in% 151:200){ # 6 right becomes 5 up
      cur_x <- cur_y - 100
      cur_y <- 150
      facing <- 3
    } else {
      cur_x <- cur_x + 1
      # cur_y <- cur_y
      # facing <- facing
    }
  } else if (facing == 1){ # going down
    if (cur_x %in% 101:150 & cur_y == 50){ # 2 down becomes 3 left
      cur_y <- cur_x - 50
      cur_x <- 100
      facing <- 2
    } else if (cur_x %in% 51:100 & cur_y == 150){ # 5 down becomes 6 left
      cur_y <- cur_x + 100
      cur_x <- 50
      facing <- 2
    } else if (cur_x %in% 1:50 & cur_y == 200){ # 6 down become 2 down
      cur_x <- cur_x + 100
      cur_y <- 1
      facing <- 1
    } else {
      # cur_x <- cur_x
      cur_y <- cur_y + 1
      # facing <- facing
    }
  } else if (facing == 2){ #going left
    if (cur_x == 51 & cur_y %in% 1:50){ # 1 left becomes 4 right
      cur_y <- 150 - cur_y + 1
      cur_x <- 1
      facing <- 0
    } else if (cur_x == 51 & cur_y %in% 51:100){ # 3 left becomes 4 down
      cur_x <- cur_y - 50
      cur_y <- 101
      facing <- 1
    } else if (cur_x == 1 & cur_y %in% 101:150){ # 4 left becomes 1 right
      cur_y <- 150 - cur_y + 1
      cur_x <- 51
      facing <- 0
    } else if (cur_x == 1 & cur_y %in% 151:200){ # 6 left becomes 1 down
      cur_x <- cur_y - 100
      cur_y <- 1
      facing <- 1
    } else {
      cur_x <- cur_x - 1
      # cur_y <- cur_y
      # facing <- facing
    }
  } else if (facing == 3){ # going up
    if (cur_x %in% 51:100 & cur_y == 1){ # 1 up becomes 6 right
      cur_y <- cur_x + 100
      cur_x <- 1
      facing <- 0
    } else if (cur_x %in% 101:150 & cur_y == 1){ # 2 up becomes 6 up
      cur_x <- cur_x - 100
      cur_y <- 200
      facing <- 3
    } else if (cur_x %in% 1:50 & cur_y == 101){ # 4 up becomes 3 right
      cur_y <- cur_x + 50
      cur_x <- 51
      facing <- 0
    } else {
      # cur_x <- cur_x
      cur_y <- cur_y - 1
      # facing <- facing
    }
  }
  
  return(c(cur_x, cur_y, facing))
}

cube_dir <- function(dir, cur_x, cur_y, facing, tiles){
  
  # let's do 0 = right, 1 = down, 2 = left, 3 = up (from directions)
  if (dir == "R"){
    facing <- (facing + 1) %% 4
  } else if (dir == "L"){
    facing <- (facing - 1) %% 4
  } else {
    number_advance <- as.numeric(dir)
    
    for (idx in 1:number_advance){
      next_step <- increment_dir(cur_x, cur_y, facing, tiles)
      #print(next_step)
      next_x <- next_step[1]
      next_y <- next_step[2]
      
      if (tiles[next_y, next_x] == " "){
        print(paste("broken - ", cur_x, ":", cur_y, "-", facing))
      }
      if (tiles[next_y, next_x] == "#"){
        break
      } else {
        cur_x <- next_x
        cur_y <- next_y
        facing <- next_step[3]
      }
    }
  }
  
  out_list <- c(cur_x, cur_y, facing)
  return(out_list)
}

# write a function to get final facing, just parsing through directions

pos <- c(51,1) # storing x-index [,x] first, and y-index [y,] second
facing <- 0 # let's do 0 = up, 1 = right, 2 = down, 3 = left

for (i in 1:length(directions)){
  cur_x <- pos[1]
  cur_y <- pos[2]
  dir <- directions[i]
  
  out_list <- cube_dir(dir, cur_x, cur_y, facing, tiles)
  
  cur_x <- out_list[1]
  cur_y <- out_list[2]
  facing <- out_list[3]
  pos <- c(cur_x, cur_y)
  #print(paste(c(pos, tiles[as.numeric(cur_y), as.numeric(cur_x)]), collaose = " "))
}

print(1000*cur_y + 4*cur_x + facing)


