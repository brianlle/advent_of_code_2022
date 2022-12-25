# Advent of Code 2022, Day 23
library(tidyverse)

# first half: see no adjacent, no move
# If there is no Elf in the N, NE, or NW adjacent positions, the Elf proposes moving north one step.
# If there is no Elf in the S, SE, or SW adjacent positions, the Elf proposes moving south one step.
# If there is no Elf in the W, NW, or SW adjacent positions, the Elf proposes moving west one step.
# If there is no Elf in the E, NE, or SE adjacent positions, the Elf proposes moving east one step.

# second half:
# if only 1 elf proposed location, thye move there
# if 2 or more proposed a location, none move

# round end: cycle directions to consider:
# starting: north, south, west, east

# after 10 rounds, find smallest rectangle containing all elves; how many empty spots are there
# i.e. area - number_elves

propose_move <- function(x, y, elves, directions){
  N <- paste0(x, "_", y+1)
  NW <- paste0(x-1, "_", y+1)
  NE <- paste0(x+1, "_", y+1)
  E <- paste0(x+1, "_", y)
  W <- paste0(x-1, "_", y)
  SE <- paste0(x+1, "_", y-1)
  S <- paste0(x, "_", y-1)
  SW <- paste0(x-1, "_", y-1)
  
  # check if positions are empty of elves
  N <- !(N %in% elves)
  NW <- !(NW %in% elves)
  NE <- !(NE %in% elves)
  W <- !(W %in% elves)
  E <- !(E %in% elves)
  SE <- !(SE %in% elves)
  S <- !(S %in% elves)
  SW <- !(SW %in% elves)
  
  if (N & NW & NE & W & E & SE & S & SW){
    return(paste0(x, "_", y))
  } else {
    for (dir_i in 1:length(directions)){
      dir <- directions[dir_i]
      if (dir == "N"){
        if (N & NW & NE){
          return(paste0(x, "_", y+1))
        }
      } else if (dir == "W"){
        if (W & NW & SW){
          return(paste0(x-1, "_", y))
        }
      } else if (dir == "E"){
        if (E & NE & SE){
          return(paste0(x+1, "_", y))
        }
      } else if (dir == "S"){
        if (S & SW & SE){
          return(paste0(x, "_", y-1))
        }
      } else {
        print("help")
      }
    }
  }
  
  #no valid moves, so stay in place
  return(paste0(x, "_", y))
}

data <- readLines("input/day23.txt") %>% 
     str_split('', simplify = TRUE)

data <- matrix(data, nrow = nrow(data), ncol = ncol(data))

elves <- c()

for (i in 1:nrow(data)){
  for (j in 1:ncol(data)){
    if (data[j, i] == "#"){
      elves <- c(elves, paste0(i, "_", ncol(data) - j + 1))
    }
  }
}

directions <- c("N", "S", "W", "E")

for (round in 1:10){
  #first half, generate proposed moves
  proposed <- c()
  
  for (elf in elves){
    x <- as.numeric(gsub("_.*", "", elf))
    y <- as.numeric(gsub(".*_", "", elf))
    proposed <- c(proposed, propose_move(x, y, elves, directions))
  }
  
  # second half, execute moves if: no other elves proposed; and not equal to self
  # although moving to self is honestly probably ok
  
  duplicated <- proposed[duplicated(proposed)]
  
  out_elves <- c()
  for (idx in 1:length(elves)){
    if (proposed[idx] %in% duplicated){
      out_elves <- c(out_elves, elves[idx]) # elf stays in place
    } else {
      out_elves <- c(out_elves, proposed[idx]) # elf moves to proposed
    }
  }
  
  # clean up
  elves <- out_elves
  print(paste("round", round))
  directions <- directions[c(2:4, 1)]
}

# get area of rectangle

min_x <- 9999999
min_y <- 9999999
max_x <- -9999999
max_y <- -9999999

for (elf in elves){
  x <- as.numeric(gsub("_.*", "", elf))
  y <- as.numeric(gsub(".*_", "", elf))
  
  min_x <- min(min_x, x)
  min_y <- min(min_y, y)
  max_x <- max(max_x, x)
  max_y <- max(max_y, y)
}

print((max_y - min_y + 1)*(max_x - min_x + 1) - length(elves))


### Part 2

data <- readLines("input/day23.txt") %>% 
  str_split('', simplify = TRUE)

data <- matrix(data, nrow = nrow(data), ncol = ncol(data))

elves <- c()

for (i in 1:nrow(data)){
  for (j in 1:ncol(data)){
    if (data[j, i] == "#"){
      elves <- c(elves, paste0(i, "_", ncol(data) - j + 1))
    }
  }
}

directions <- c("N", "S", "W", "E")

# convert for loop to while loop with break condition
round <- 1
while (TRUE){
  #first half, generate proposed moves
  proposed <- c()
  
  for (elf in elves){
    x <- as.numeric(gsub("_.*", "", elf))
    y <- as.numeric(gsub(".*_", "", elf))
    proposed <- c(proposed, propose_move(x, y, elves, directions))
  }
  
  #second half, execute moves if: no other elves proposed; and not equal to self
  # although moving to self is honestly probably ok?
  
  duplicated <- proposed[duplicated(proposed)]
  
  out_elves <- c()
  elves_still <- 0
  for (idx in 1:length(elves)){
    if (proposed[idx] %in% duplicated){
      out_elves <- c(out_elves, elves[idx]) # elf stays in place
    } else {
      out_elves <- c(out_elves, proposed[idx]) # elf moves to proposed
    }
    if (out_elves[idx] == elves[idx]){
      elves_still <- elves_still + 1
    }
  }

  if (elves_still == length(elves)){
    print(round)
    break
  }
  
  # clean up
  elves <- out_elves
  print(paste("round", round))
  round <- round + 1
  directions <- directions[c(2:4, 1)]
}