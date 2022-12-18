# Advent of Code 2022, Day 9
# storing coordinates of knot ends as a list in the form:
# [x1, y1, x2, y2, ...]

data <- readLines("input/day9.txt")

update_tail <- function(input){
  head_x <- input[1]
  head_y <- input[2]
  tail_x <- input[3]
  tail_y <- input[4]
  
  if ((abs(head_x - tail_x) <= 1) & (abs(head_y - tail_y) <= 1)){
    #do nothing
  } else if ((head_x == tail_x) & (head_y != tail_y)){
    tail_y <- tail_y + sign(head_y - tail_y)
  } else if ((head_x != tail_x) & (head_y == tail_y)){
    tail_x <- tail_x + sign(head_x - tail_x)
  } else if ((head_x != tail_x) & (head_y != tail_y)){
    tail_y <- tail_y + sign(head_y - tail_y)
    tail_x <- tail_x + sign(head_x - tail_x)
  }
  
  out <- list(head_x, head_y, tail_x, tail_y)
  return(out)
}

update_head <- function(input, move_dir){

  head_x <- input[1]
  head_y <- input[2]
  tail_x <- input[3]
  tail_y <- input[4]
  
  if (move_dir == "D"){
    head_y <- head_y - 1
  } else if (move_dir == "U"){
    head_y <- head_y + 1
  } else if (move_dir == "L"){
    head_x <- head_x - 1
  } else if (move_dir == "R"){
    head_x <- head_x + 1
  }
  
  out <- list(head_x, head_y, tail_x, tail_y)
  return(out)
}

positions_visited <- c("0-0")
pos <- c(0,0,0,0)
for (i in 1:length(data)){
  move_dir <- substring(data[i], 1, 1)
  move_amount <- as.numeric(substring(data[i], 3))
  
  for (idx in 1:move_amount){
    pos <- update_head(pos, move_dir)
    pos <- update_tail(pos)
    positions_visited <- c(positions_visited,
                           paste0(pos[[3]], "-", pos[[4]]))
 }
}

print(length(unique(positions_visited)))

## part 2

update_head <- function(input, move_dir){
  
  head_x <- input[1]
  head_y <- input[2]
  # tail_x <- input[[3]]
  # tail_y <- input[[4]]
  
  if (move_dir == "D"){
    head_y <- head_y - 1
  } else if (move_dir == "U"){
    head_y <- head_y + 1
  } else if (move_dir == "L"){
    head_x <- head_x - 1
  } else if (move_dir == "R"){
    head_x <- head_x + 1
  }
  
  input[1] <- head_x
  input[2] <- head_y
  #out <- list(head_x, head_y, tail_x, tail_y)
  return(input)
}

update_tail <- function(input, lead_knot){
  start_pos <- lead_knot*2 - 1
  
  head_x <- input[start_pos]
  head_y <- input[start_pos+1]
  tail_x <- input[start_pos+2]
  tail_y <- input[start_pos+3]
  # head_x <- gsub("-.*", "", head)
  # head_y <- gsub(".*-", "", head)
  # tail_x <- gsub("-.*", "", tail)
  # tail_y <- gsub(".*-", "", tail)
  
  if ((abs(head_x - tail_x) <= 1) & (abs(head_y - tail_y) <= 1)){
    #do nothing
  } else if ((head_x == tail_x) & (head_y != tail_y)){
    tail_y <- tail_y + sign(head_y - tail_y)
  } else if ((head_x != tail_x) & (head_y == tail_y)){
    tail_x <- tail_x + sign(head_x - tail_x)
  } else if ((head_x != tail_x) & (head_y != tail_y)){
    tail_y <- tail_y + sign(head_y - tail_y)
    tail_x <- tail_x + sign(head_x - tail_x)
  }
  
  input[start_pos] <- head_x
  input[start_pos+1] <- head_y
  input[start_pos+2] <- tail_x
  input[start_pos+3] <- tail_y

  return(input)
}

positions_visited <- c("0-0")
pos <- rep(0, 20)
for (i in 1:length(data)){
  move_dir <- substring(data[i], 1, 1)
  move_amount <- as.numeric(substring(data[i], 3))
  
  for (idx in 1:move_amount){
    pos <- update_head(pos, move_dir)
    for (j in 1:9){
      pos <- update_tail(pos, j)
    }
    positions_visited <- c(positions_visited,
                           paste0(pos[[19]], "-", pos[[20]]))
  }
}

print(length(unique(positions_visited)))