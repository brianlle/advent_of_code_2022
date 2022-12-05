### Advent of Code 2022: Day 1

library(tidyverse)

parse_data <- function(path) {
  data <- readLines(path)
  
  return(data)
}

data <- parse_data("input/day1.txt")

values <- c()

while (length(data) > 1){
  pos_1 <- 1
  cur_pos <- 2
  
  if (cur_pos != length(data)){
  while (data[cur_pos] != "")
    {
    cur_pos <- cur_pos + 1
    if (cur_pos > length(data)){
      break
      }
    }
  }

  pos_2 <- cur_pos - 1
  values <- c(values, sum(as.numeric(data[pos_1:pos_2])))

  if ((cur_pos - 1) != length(data)){
    data <- (data[(pos_2+2):length(data)])
  } else {
    data <- (data[1])
  }
}

print(max(values))

# Part 2

values <- sort(values, decreasing = TRUE)
print(sum(values[1:3]))
