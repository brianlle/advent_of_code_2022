# AoC 2022, Day 15

library(tidyverse)

data <- readLines("input/day15.txt")

beacon_not_here <- function(sensor_x, sensor_y, max_distance, y){
  if (((sensor_y + max_distance) < y) | ((sensor_y - max_distance) > y)){
    return(c())
  }
  
  # want the values in sensor_y + j = y
  j_bound <- abs(y - sensor_y)
  leftover_distance = max_distance - j_bound
  
  not_at_y <- (sensor_x-leftover_distance):(sensor_x+leftover_distance)
  
  return(not_at_y)
}

cannot_exist <- c()
beacons <- c()
y <- 2000000

for (i in 1:length(data)){
  print(i)
  sensor <- gsub(".*or at x=", "", data[i])
  sensor_x <- as.numeric(gsub(",.*", "", sensor))
  sensor_y <- gsub("\\:.*", "", sensor)
  sensor_y <- as.numeric(gsub(".*y=", "", sensor_y))
  
  beacon <- gsub(".*beacon is at x=", "", data[i])
  beacon_x <- as.numeric(gsub(",.*", "", beacon))
  beacon_y <- as.numeric(gsub(".*y=", "", beacon))
  
  max_distance <- abs(beacon_y - sensor_y) + abs(beacon_x - sensor_x)
  
  cannot_exist <- c(cannot_exist, beacon_not_here(sensor_x, sensor_y, max_distance, y))
  cannot_exist <- unique(cannot_exist)
  
  beacons <- c(beacons, beacon_y)
}

print(length(cannot_exist) - length(beacons[beacons == y]))


# part 2

sensors_x <- c()
sensors_y <- c()
beacons_x <- c()
beacons_y <- c()

for (i in 1:length(data)){
  #print(i)
  sensor <- gsub(".*or at x=", "", data[i])
  sensor_x <- as.numeric(gsub(",.*", "", sensor))
  sensor_y <- gsub("\\:.*", "", sensor)
  sensor_y <- as.numeric(gsub(".*y=", "", sensor_y))
  
  beacon <- gsub(".*beacon is at x=", "", data[i])
  beacon_x <- as.numeric(gsub(",.*", "", beacon))
  beacon_y <- as.numeric(gsub(".*y=", "", beacon))
  
  sensors_x <- c(sensors_x, sensor_x)
  sensors_y <- c(sensors_y, sensor_y)
  beacons_x <- c(beacons_x, beacon_x)
  beacons_y <- c(beacons_y, beacon_y)
}


for (y in 1:4000000){
  cur <- array(rep(0, 4000000))
  print(y)
  
  for (i in 1:length(data)){
    sensor_x <- sensors_x[i]
    sensor_y <- sensors_y[i]
    beacon_x <- beacons_x[i]
    beacon_y <- beacons_y[i]
    
    max_distance <- abs(beacon_y - sensor_y) + abs(beacon_x - sensor_x)
    if (((sensor_y + max_distance) < y) | ((sensor_y - max_distance) > y)){
      next
    }
    
    j_bound <- abs(y - sensor_y)
    leftover_distance <- max_distance - j_bound
    
    lower <- max(1, sensor_x - leftover_distance)
    upper <- min(4000000, sensor_x + leftover_distance)
    
    cur[lower:upper] <- 1

  }

  if (sum(cur) != 4000000){
    out_x <- match(0, cur)
    out_y <- y
    break
  }
}

print(out_x*4000000 + out_y)
