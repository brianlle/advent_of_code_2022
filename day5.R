# Advent of Code 2022, Day 5

library(tidyverse)

data <- readLines("input/day5.txt")
crates <- data[1:8]
moves <- data[11:nrow(data)]

get_stack <- function(crates, stack){
  letter_pos <- -2 + 4*stack
  stack_out <- substr(crates, letter_pos, letter_pos)
  return(stack_out[stack_out != " "])
}

stacks <- list("1" = get_stack(crates, 1),
               "2" = get_stack(crates, 2),
               "3" = get_stack(crates, 3),
               "4" = get_stack(crates, 4),
               "5" = get_stack(crates, 5),
               "6" = get_stack(crates, 6),
               "7" = get_stack(crates, 7),
               "8" = get_stack(crates, 8),
               "9" = get_stack(crates, 9)
               )


for (i in 1:length(moves)){
  cur_move <- moves[i]
  
  num_to_move <- as.numeric(gsub(" from .*" ,"", gsub("move ", "", cur_move)))
  from_crate <- gsub(".*from " ,"", gsub(" to.*", "", cur_move))
  to_crate <- gsub(".*to ", "", cur_move)
  
  while (num_to_move > 0){
    stacks[[to_crate]] <- c(stacks[[from_crate]][1], stacks[[to_crate]])
    stacks[[from_crate]] <- stacks[[from_crate]][-1]
    
    num_to_move <- num_to_move - 1
  }
}

string <- ""
for (i in 1:9){
  string <- paste0(string, stacks[[i]][1])
}
print(string)

# part 2

stacks <- list("1" = get_stack(crates, 1),
               "2" = get_stack(crates, 2),
               "3" = get_stack(crates, 3),
               "4" = get_stack(crates, 4),
               "5" = get_stack(crates, 5),
               "6" = get_stack(crates, 6),
               "7" = get_stack(crates, 7),
               "8" = get_stack(crates, 8),
               "9" = get_stack(crates, 9)
)

for (i in 1:length(moves)){
  cur_move <- moves[i]
  
  num_to_move <- as.numeric(gsub(" from .*" ,"", gsub("move ", "", cur_move)))
  from_crate <- gsub(".*from " ,"", gsub(" to.*", "", cur_move))
  to_crate <- gsub(".*to ", "", cur_move)
  

  stacks[[to_crate]] <- c(stacks[[from_crate]][1:num_to_move], stacks[[to_crate]])
  stacks[[from_crate]] <- stacks[[from_crate]][-(1:num_to_move)]
    

}

string <- ""
for (i in 1:9){
  string <- paste0(string, stacks[[i]][1])
}
print(string)
