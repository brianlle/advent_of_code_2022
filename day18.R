# AoC 2022, Day 18

library(tidyverse)
library(collections)


data <- readLines("input/day18.txt") %>% 
  str_split(',', simplify = TRUE)
data <- matrix(as.numeric(data), ncol = 3)


count_sides <- function(cubes, xyz){
  x <- xyz[1]
  y <- xyz[2]
  z <- xyz[3]
  
  sides <- 0
  
  x1 <- c(x+1, y, z)
  x2 <- c(x-1, y, z)
  y1 <- c(x, y+1, z)
  y2 <- c(x, y-1, z)
  z1 <- c(x, y, z+1)
  z2 <- c(x, y, z-1)
  
  if (!cubes$has(x1)){
    sides <- sides + 1
  }
  if (!cubes$has(x2)){
    sides <- sides + 1
  }
  if (!cubes$has(y1)){
    sides <- sides + 1
  }
  if (!cubes$has(y2)){
    sides <- sides + 1
  }
  if (!cubes$has(z1)){
    sides <- sides + 1
  }
  if (!cubes$has(z2)){
    sides <- sides + 1
  }
  
  return(sides)
}

cubes <- dict()

for (i in 1:nrow(data)){
  cubes$set(data[i,], TRUE)
}

total_sides <- 0
for (i in 1:nrow(data)){
  total_sides <- total_sides + count_sides(cubes, data[i,])
}

print(total_sides)

## Part 2

# need to exclude interior surface area
# part two description suggests approaching this by
# counting cubes of air inside lava droplet, and subtracting those resultant sides
# the R implementation of dicts() doesn't seem to allow checking nested loops,
# so switch back to janky string lists

air_cubes <- c()

x_min <- min(data[,1])
x_max <- max(data[,1])
y_min <- min(data[,2])
y_max <- max(data[,2])
z_min <- min(data[,3])
z_max <- max(data[,3])

actual_points <- c()
for (i in 1:nrow(data)){
  actual_points <- c(actual_points, paste0(data[i,1], "-", data[i,2], "_", data[i,3]))
}

test_points <- dict()
test_vector <- c()
for (i in x_min:x_max){
  for (j in y_min:y_max){
    for (k in z_min:z_max){
      test_points$set(c(i,j,k), TRUE)
      test_vector <- c(test_vector, paste0(i, "-", j, "_", k))
    }
  }
}
  
air_cubes <- c()

for (point in test_vector){
  i <- as.numeric(gsub("-.*", "", point))
  j <- gsub("_.*", "", point)
  j <- as.numeric(gsub(".*-", "", j))
  k <- as.numeric(gsub(".*_", "", point))

  # check for x-cubes in both directions
  x_max_bound <- FALSE
  for (i2 in (i+1):x_max){
    #if (cubes$has(c(i2, j, k))){
    if (paste0(i2, "-", j, "_", k) %in% actual_points){
      x_max_bound <- TRUE
      break
    }
  }
  x_min_bound <- FALSE
  for (i2 in (i-1):x_min){
    #if (cubes$has(c(i2, j, k))){
    if (paste0(i2, "-", j, "_", k) %in% actual_points){
      x_min_bound <- TRUE
      break
    }
  }
  
  # check for y-cubes in both directions
  y_max_bound <- FALSE
  for (j2 in (j+1):y_max){
   # if (cubes$has(c(i, j2, k))){
    if (paste0(i, "-", j2, "_", k) %in% actual_points){
      y_max_bound <- TRUE
      break
    }
  }
  y_min_bound <- FALSE
  for (j2 in (j-1):y_min){
    #if (cubes$has(c(i, j2, k))){
    if (paste0(i, "-", j2, "_", k) %in% actual_points){
      y_min_bound <- TRUE
      break
    }
  }
  
  # check for z-cubes in both directions
  z_max_bound <- FALSE
  for (k2 in (k+1):z_max){
    #if (cubes$has(c(i, j, k2))){
    if (paste0(i, "-", j, "_", k2) %in% actual_points){
      z_max_bound <- TRUE
      break
    }
  }
  z_min_bound <- FALSE
  for (k2 in (k-1):z_min){
    #if (cubes$has(c(i, j, k2))){
    if (paste0(i, "-", j, "_", k2) %in% actual_points){
      z_min_bound <- TRUE
      break
    }
  }
  
  if (x_max_bound & x_min_bound & y_max_bound & y_min_bound & z_max_bound & z_min_bound){
    air_cubes <- c(air_cubes, paste0(i, "-", j, "_", k))
  }
}

# keep only identified air cubes that aren't actual lava cubes
air_cubes <- air_cubes[!air_cubes %in% actual_points]

# construct dict to use already created function from part 1 to count up
# air cube sides
air_cube_dict <- dict()
for (air_cube in air_cubes){
  x <- as.numeric(gsub("-.*", "", air_cube))
  y <- gsub("_.*", "", air_cube)
  y <- as.numeric(gsub(".*-", "", y))
  z <- as.numeric(gsub(".*_", "", air_cube))
  
  air_cube_dict$set(c(x,y,z), TRUE)
}

total_air_sides <- 0
for (air_cube in air_cubes2){
  x <- as.numeric(gsub("-.*", "", air_cube))
  y <- gsub("_.*", "", air_cube)
  y <- as.numeric(gsub(".*-", "", y))
  z <- as.numeric(gsub(".*_", "", air_cube))
  
  total_air_sides <- total_air_sides + count_sides(air_cube_dict, c(x,y,z))
}

print(total_sides - total_air_sides)