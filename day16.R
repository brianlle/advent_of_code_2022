# AoC 2022, Day 16

library(tidyverse)

data <- readLines("input/day16.txt")

all_valves <- gsub(" has flow.*", "", data)
all_valves <- gsub("Valve ", "", all_valves)

flow_rates <- gsub("\\;.*", "", data)
flow_rates <- as.numeric(gsub(".*rate\\=", "", flow_rates))

to_valves <- gsub(".*to valves ", "", data)
to_valves <- gsub(".*to valve ", "", to_valves)


get_flow_rate <- function(valves){
  opened <- valves[str_detect(valves, "open")]
  multiplier <- 30:0
  multiplier <- multiplier[str_detect(valves, "open")]
  
  if (length(opened) == 0){
    return(0)
  }
  opened <- as.numeric(gsub("open", "", opened))
  
  sum <- 0
  for (i in 1:length(opened)){
    sum <- sum + flow_rates[opened[i]]*multiplier[i]
  }
  
  return(sum)
}

check_circle <- function(path, poss){
  valves <- strsplit(path, split = ",")[[1]]
  
  if (!poss %in% valves){
    return(FALSE) # poss not already visited
  } else {
    previous_step <- which(valves %in% poss)
    previous_step <- previous_step[length(previous_step)]
    
    if (get_flow_rate(c(valves[previous_step:length(valves)], poss)) == 0){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  return(FALSE)
}

# start at AA

valve_paths <- c("AA")
valve_paths_new <- rep("", 6)
cur_idx <- 1
cur_scores <- c()

for (idx in 1:30){
  print(idx)
  for (path in valve_paths){
    
    valves <- strsplit(path, split = ",")[[1]]
    if (str_detect(valves[length(valves)], "open")){
      current_valve <- as.numeric(match(valves[length(valves)-1], all_valves))
    } else {
      current_valve <- as.numeric(match(valves[length(valves)], all_valves))
    }
    
    if (flow_rates[current_valve] == 0){
      possible_actions <- strsplit(to_valves[current_valve], split = ", ")[[1]]
    } else if (paste0(current_valve, "open") %in% valves){
      possible_actions <- strsplit(to_valves[current_valve], split = ", ")[[1]]
    } else {
      possible_actions <- c(paste0(current_valve, "open"),
                            strsplit(to_valves[current_valve], split = ", ")[[1]])
    }
    
    for (poss in possible_actions){
      if (check_circle(path, poss)){
        next
      } else {
        new_path <- paste0(path, ",", poss)
        new_valves <- strsplit(new_path, split = ",")[[1]]
        
        cur_scores[cur_idx] <- get_flow_rate(new_valves)
        valve_paths_new[cur_idx] <- new_path
        cur_idx <- cur_idx + 1
      }
    }
  }
  
  # cut-off to keep number of paths to check manageable
  valve_paths_new <- valve_paths_new[rank(-cur_scores) < 50000]
  
  if (idx == 30){
    print(max(cur_scores))
  }
  
  valve_paths <- valve_paths_new[valve_paths_new != ""]
  valve_paths_new <- rep("", 2*length(valve_paths)+10000)
  cur_scores <- rep(0, 2*length(valve_paths)+10000)
  cur_idx <- 1
  print(length(valve_paths))
}

cur_max <- 0

for (path in valve_paths){
  valves <- strsplit(path, split = ",")[[1]]
  
  cur_max <- max(cur_max, get_flow_rate(valves))
}

print(cur_max)

### part 2
# redo everything, with 26 steps instead (26:0 in score call)
# get top one, then a top one that doesn't use any of the alreayd used valves

get_flow_rate2 <- function(valves, flow_rates){
  opened <- valves[str_detect(valves, "open")]
  multiplier <- 26:0
  multiplier <- multiplier[str_detect(valves, "open")]
  
  if (length(opened) == 0){
    return(0)
  }
  opened <- as.numeric(gsub("open", "", opened))
  
  sum <- 0
  for (i in 1:length(opened)){
    sum <- sum + flow_rates[opened[i]]*multiplier[i]
  }
  
  return(sum)
}


# start at AA

valve_paths <- c("AA")
valve_paths_new <- rep("", 6)
cur_idx <- 1
cur_scores <- c()

for (idx in 1:26){
  print(idx)
  for (path in valve_paths){
    
    valves <- strsplit(path, split = ",")[[1]]
    if (str_detect(valves[length(valves)], "open")){
      current_valve <- as.numeric(match(valves[length(valves)-1], all_valves))
    } else {
      current_valve <- as.numeric(match(valves[length(valves)], all_valves))
    }
    
    if (flow_rates[current_valve] == 0){
      possible_actions <- strsplit(to_valves[current_valve], split = ", ")[[1]]
    } else if (paste0(current_valve, "open") %in% valves){
      possible_actions <- strsplit(to_valves[current_valve], split = ", ")[[1]]
    } else {
      possible_actions <- c(paste0(current_valve, "open"),
                            strsplit(to_valves[current_valve], split = ", ")[[1]])
    }
    
    
    for (poss in possible_actions){
      if (check_circle(path, poss)){
        next
      } else {
        new_path <- paste0(path, ",", poss)
        new_valves <- strsplit(new_path, split = ",")[[1]]
        
        cur_scores[cur_idx] <- get_flow_rate2(new_valves, flow_rates)
        valve_paths_new[cur_idx] <- new_path
        cur_idx <- cur_idx + 1
        #}
      }
    }
  }
  
  valve_paths_new <- valve_paths_new[rank(-cur_scores) < 50000]
  
  
  if (idx == 26){
    print(max(cur_scores))
    max1 <- max(cur_scores)
    path1 <- valve_paths_new[match(max1, cur_scores)]
  }
  
  valve_paths <- valve_paths_new[valve_paths_new != ""]
  valve_paths_new <- rep("", 2*length(valve_paths)+10000)
  cur_scores <- rep(0, 2*length(valve_paths)+10000)
  cur_idx <- 1
  print(length(valve_paths))
}

cur_max <- 0

valves1 <- strsplit(path1, split = ",")[[1]]
valves1 <- valves1[str_detect(valves1, "open")]
valves1 <- as.numeric(gsub("open", "", valves1))

flow_rates2 <- flow_rates
flow_rates2[valves1] <- 0


valve_paths <- c("AA")
valve_paths_new <- rep("", 6)
cur_idx <- 1
cur_scores <- c()

for (idx in 1:26){
  print(idx)
  for (path in valve_paths){
    
    valves <- strsplit(path, split = ",")[[1]]
    if (str_detect(valves[length(valves)], "open")){
      current_valve <- as.numeric(match(valves[length(valves)-1], all_valves))
    } else {
      current_valve <- as.numeric(match(valves[length(valves)], all_valves))
    }
    
    if (flow_rates[current_valve] == 0){
      possible_actions <- strsplit(to_valves[current_valve], split = ", ")[[1]]
    } else if (paste0(current_valve, "open") %in% valves){
      possible_actions <- strsplit(to_valves[current_valve], split = ", ")[[1]]
    } else {
      possible_actions <- c(paste0(current_valve, "open"),
                            strsplit(to_valves[current_valve], split = ", ")[[1]])
    }
    
    
    for (poss in possible_actions){
      if (check_circle(path, poss)){
        next
      } else {
        new_path <- paste0(path, ",", poss)
        new_valves <- strsplit(new_path, split = ",")[[1]]
        
        cur_scores[cur_idx] <- get_flow_rate2(new_valves, flow_rates2)
        valve_paths_new[cur_idx] <- new_path
        cur_idx <- cur_idx + 1
        #}
      }
    }
  }
  
  valve_paths_new <- valve_paths_new[rank(-cur_scores) < 50000]
  
  
  if (idx == 26){
    print(max(cur_scores))
    max2 <- max(cur_scores)
    path2 <- valve_paths_new[match(max1, valve_paths_new)]
  }
  
  valve_paths <- valve_paths_new[valve_paths_new != ""]
  valve_paths_new <- rep("", 2*length(valve_paths)+10000)
  cur_scores <- rep(0, 2*length(valve_paths)+10000)
  cur_idx <- 1
  print(length(valve_paths))
}

print(max1 + max2)