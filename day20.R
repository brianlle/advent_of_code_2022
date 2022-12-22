# Advent of Code 2022, Day 20

move_number <- function(data, order_pos){
  
  cur_num <- data[as.character(order_pos)]
  
  if (cur_num == 0){
    return(data)
  }
  
  # track iteration order using characters; using numeric keeps breaking
  cur_pos <- which(as.character(order_pos) == names(data))
  move_to <- (cur_pos + cur_num - 1) %% (N-1) + 1 # R is 1 -indexed
  
  if (move_to > cur_pos){
    data <- c(data[setdiff(1:move_to, cur_pos)],
              cur_num,
              data[move_to + 1:(N - move_to)])
  } else if (move_to < cur_pos){
    data <- c(data[seq_len(move_to - 1)],
              cur_num,
              data[setdiff(move_to:N, cur_pos)])
  }
  
  return(data)
}



data <- as.numeric(readLines("input/day20.txt"))
names(data) <- as.character(1:length(data))
N <- length(data)

for (i in 1:N){
  data <- move_number(data, i)
  
  # if (length(data) != N){
  #   break
  # }
}

# sort data such that 0 is the first value
zero_pos <- match(0, data)
data <- data[c(zero_pos, (zero_pos+1):length(data), 1:(zero_pos-1))]
print(sum(data[c(1001, 2001, 3001)]))


### Part 2

data <- as.numeric(readLines("input/day20.txt"))
names(data) <- as.character(1:length(data))
N <- length(data)

dkey <- 811589153
data <- data * (dkey)

for(n_loop in 1:10){
  print(n_loop)
  for(i in asc(1:N)){
    data <- move_number(data, i)
    cur_pos <- which(i == names(data))
  }   
}

zero_pos <- match(0, data)
data <- data[c(zero_pos, (zero_pos+1):length(data), 1:(zero_pos-1))]
options(scipen = 99999)
print(sum(data[c(1001, 2001, 3001)]))

sum(get_n(data, c(1000, 2000, 3000), which(data == 0)))

