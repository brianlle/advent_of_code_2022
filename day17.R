# AOC 2022 day 17

data <- readLines("input/day17.txt")
data <- strsplit(data, split = "")[[1]]

### Shapes below 

#### 1

.#. 2
###
.#.

..# 3
..#
###

#   4
#
#
#

##  5
## 

# jet happens, then falls one, repeat
# new rock appears: left edge is 2 away from left wall (i.e. 00111100)
#                   and bottom edge is 3 units above highest rock (or floor)

cur_shape <- c()
occupied_spots <- c()
cur_top <- 0

text_coords <- function(x, y){
  return(paste0(x, "-", y))
}

get_shape <- function(idx, cur_top){
  if (idx %% 5 == 1){
    shape <- list("1" = c(3, cur_top+4),
                  "2" = c(4, cur_top+4),
                  "3" = c(5, cur_top+4),
                  "4" = c(6, cur_top+4))
  } else if (idx %% 5 == 2){
    shape <- list("1" = c(3, cur_top+5),
                  "2" = c(4, cur_top+4),
                  "3" = c(5, cur_top+5),
                  "4" = c(4, cur_top+5),
                  "5" = c(4, cur_top+6))
  } else if (idx %% 5 == 3){
    shape <- list("1" = c(3, cur_top+4),
                  "2" = c(4, cur_top+4),
                  "3" = c(5, cur_top+4),
                  "4" = c(5, cur_top+5),
                  "5" = c(5, cur_top+6))
  } else if (idx %% 5 == 4){
    shape <- list("1" = c(3, cur_top+4),
                  "2" = c(3, cur_top+5),
                  "3" = c(3, cur_top+6),
                  "4" = c(3, cur_top+7))
  } else if (idx %% 5 == 0){
    shape <- list("1" = c(3, cur_top+4),
                  "2" = c(3, cur_top+5),
                  "3" = c(4, cur_top+4),
                  "4" = c(4, cur_top+5))
  }
  
  return(shape)
}

move_shape <- function(cur_shape, direction, occupied_spots){
  length_shape <- length(cur_shape)
  x_coords <- c()
  y_coords <- c()
  for (i in 1:length(cur_shape)){
    x_coords <- c(x_coords, cur_shape[[i]][1])
    y_coords <- c(y_coords, cur_shape[[i]][2])
  }
  
  out_shape <- list()
  if (direction == ">"){
    if (as.numeric(max(x_coords)) == 7){
      return(cur_shape)
    } else {
      x_coords <- x_coords + 1
      
      for (i in 1:length(cur_shape)){
        if (text_coords(x_coords[i], y_coords[i]) %in% occupied_spots){
          return(cur_shape) #exit if potential new spot occupied
        }
      }
      
      for (i in 1:length_shape){
        out_shape[[i]] <- c(x_coords[i], y_coords[i])
      }
      return(out_shape)
    }
  } else if (direction == "<"){
    if (as.numeric(min(x_coords)) == 1){
      return(cur_shape)
    } else {
      x_coords <- x_coords - 1
      
      for (i in 1:length(cur_shape)){
        if (text_coords(x_coords[i], y_coords[i]) %in% occupied_spots){
          return(cur_shape) #exit if potential new spot occupied
        }
      }
      
      for (i in 1:length_shape){
        out_shape[[i]] <- c(x_coords[i], y_coords[i])
      }
      return(out_shape)
    }
  } else {
    print("help")
  }
}

fall_shape <- function(cur_shape, occupied_spots){
  x_coords <- c()
  y_coords <- c()

  for (i in 1:length(cur_shape)){
    x_coords <- c(x_coords, cur_shape[[i]][1])
    y_coords <- c(y_coords, cur_shape[[i]][2])
  }
  
  if (as.numeric(min(y_coords)) == 0){
    return(cur_shape) # exit if on floor
  }
  
  new_y <- y_coords - 1
  
  for (i in 1:length(cur_shape)){
    if (text_coords(x_coords[i], new_y[i]) %in% occupied_spots){
      return(cur_shape) #exit if potential new spot occupied
    }
  }
  
  for (i in 1:length(cur_shape)){
    cur_shape[[i]][2] <- new_y[i]
  }
  return(cur_shape)
}
  
# body

keep_shape = TRUE
cur_top <- -1
cur_direction <- 1
occupied_spots <- c()

for (idx in 1:2022){
  print(idx)
  cur_shape <- get_shape(idx, cur_top)
  
  while(keep_shape){
    cur_shape <- move_shape(cur_shape, data[cur_direction], occupied_spots)
    
    cur_direction <- cur_direction + 1
    
    if (cur_direction == (length(data) + 1)){
      cur_direction <- 1
    }
    
    cur_shape2 <- fall_shape(cur_shape, occupied_spots)
    
    if (identical(cur_shape, cur_shape2)){ #shape can't fall anymore
      x_coords <- c()
      y_coords <- c()

      for (i in 1:length(cur_shape)){
        x_coords <- c(x_coords, cur_shape[[i]][1])
        y_coords <- c(y_coords, cur_shape[[i]][2])
        occupied_spots <- c(occupied_spots, text_coords(cur_shape[[i]][1],
                                                        cur_shape[[i]][2]))
      }
      
      cur_top <- max(c(as.numeric(y_coords), cur_top))
      
      keep_shape <- FALSE
    } else {
      cur_shape <- cur_shape2
    }
  }
  keep_shape <- TRUE
}


print(cur_top + 1)



