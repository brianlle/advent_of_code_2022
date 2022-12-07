# Advent of Code 2022, Day 7

library(tidyverse)
library(stringr)

data <- readLines("input/day7.txt")

add_files <- function(data, name, start, finish){
  subset <- data[(start + 2):finish] # two lines after "$ cd", one line before next "$ cd"
  
  dirs_contained <- c()
  sizes_contained <- 0
  for (i in 1:length(subset)){
    if (str_detect(subset[i], "^dir")){
      temp_dir <- gsub("dir ", "", subset[i])
      temp_dir <- paste0(name, "-", temp_dir)
      dirs_contained <- c(dirs_contained, temp_dir)
    } else if (str_detect(subset[i], "^[0-9]")){
      sizes_contained <- sizes_contained + as.numeric(gsub(" .*", "", subset[i]))
    }
  }
  
  out_df <- data.frame(name = name, dirs = paste(dirs_contained, collapse = ","), size = sizes_contained)
  return(out_df)
}

first = TRUE
names <- c()
starts <- c()
ends <- c()
for (i in 1:length(data)){
  if (str_detect(data[i], "^.*cd ") & data[i] != "$ cd .."){
    if (first == TRUE){
      cur_name <- c("/")
      names <- c(names, gsub(".*cd ", "", data[i]))
      starts <- c(starts, i)
      first = FALSE
    } else if (first == FALSE){
      cur_name <- c(cur_name,  gsub(".*cd ", "", data[i]))
      names <- c(names, paste0(cur_name, collapse = "-"))
      ends <- c(ends, i-1)
      starts <- c(starts, i)
    }
  }
  if (data[i] == "$ cd .."){
    cur_name <- cur_name[1:(length(cur_name)-1)]
    print(paste0(cur_name, collapse = "-"))
  }
  if (i == length(data)){
    ends <- c(ends, i)
  }
}

df <- data.frame(name = character(), dirs = character(), size = numeric())
for (i in 1:length(starts)){
  df <- rbind(df,
              add_files(data, names[i], starts[i], ends[i])
              )
}

while(length(unique(df$dirs)) > 1){
  for (i in 1:nrow(df)){
    
    cur_name <- df[i, "name"]
    cur_size <- df[i, "size"]
    cur_dirs <- strsplit(df[i, "dirs"], ",")[[1]]
    
    if (length(cur_dirs) > 0){
      size_get <- 0
      dirs_get <- ""
      for (j in 1:length(cur_dirs)){
        temp_name <- cur_dirs[j]
        cur_row <- df[df$name == temp_name,]
        if (df[df$name == temp_name, "dirs"] != ""){
          dirs_get <- paste0(c(dirs_get, df[df$name == temp_name, "dirs"]), collapse = ",")
        }
        size_get <- size_get + df[df$name == temp_name,"size"] + 0
      }
      dirs_get <- substring(dirs_get, 2)
      df[i, "size"] <- cur_size + size_get
      if (length(dirs_get) > 0){
        df[i, "dirs"] <- dirs_get
      } else {
        df[i, "dirs"] <- ""
      }
    }
  }
}

print(sum(df[df$size < 100000, "size"]))

# Part 2

# need 30000000
# total 70000000
currently_used <- df[df$name == "/", "size"]
min(df[df$size > (30000000 - (70000000 - currently_used)), "size"])

