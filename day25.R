# Advent of Code 2022, Day 25

library(tidyverse)

data <- readLines("input/day25.txt")


convert <- function(string){
  n <- nchar(string)
  num_out <- 0
  
  for (idx in 1:n){
    char <- substring(string, idx, idx)
    
    if (char == "2") num <- 2
    else if (char == "1") num <- 1
    else if (char == "0") num <- 0
    else if (char == "-") num <- -1
    else if (char == "=") num <- -2
    
    pow5 <- n - idx
    num_out <- num_out + num*(5^pow5)
  }
  
  return(num_out)
}

rev_convert <- function(number){
  pow <- 1
  
  while(TRUE){
    if (2*5^pow + 2*5^(pow-1) > number){
      break
    } else {
      pow <- pow + 1
    }
  }
  
  str_out <- paste0(rep("0", pow+1), collapse = "")
  
  for (idx in 1:(pow+1)){
    cur_num <- "="
    
    temp_str <- str_out
    
    while(TRUE){
      substr(temp_str, idx, idx) <- cur_num
      if (idx == pow+1){
        print(temp_str)
      }
      
      if (convert(temp_str) == number){
        return(temp_str)
      }
      
      if (convert(temp_str) + 2*5^(max(pow-idx, 0)) >= number){
        str_out <- temp_str
        break
      } else {
        if (cur_num == "=") cur_num <- "-"
        else if (cur_num == "-") cur_num <- "0"
        else if (cur_num == "0") cur_num <- "1"
        else if (cur_num == "1") cur_num <- "2"
        else if (cur_num == "2") print('help')
      }
    }
  }
  
  return(str_out)
}

sum <- sum(sapply(data, convert))
sum_conv <- rev_convert(sum)
print(sum_conv)
