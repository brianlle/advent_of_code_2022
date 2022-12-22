# Advent of Code 2022, Day 21
library(tidyverse)


data <- readLines("input/test.txt")

number_monkeys <- data[str_detect(data, "[0-9]")]
oper_monkeys <- data[!str_detect(data, "[0-9]")]

number_monkeys <- tibble(name = gsub(":.*", "", number_monkeys),
                         type = "number",
                         value1 = gsub(".*: ", "", number_monkeys),
                         operation = NA,
                         value2 = NA)
oper_monkeys <- tibble(name = gsub(":.*", "", oper_monkeys),
                       type = "operation",
                       value1 = gsub(" [-+*/].*", "", gsub(".*: ", "", oper_monkeys)),
                       operation = str_extract(oper_monkeys, "[-+*/]"),
                       value2 = gsub(".*[-+*/] ", "", oper_monkeys))

get_value <- function(number_monkeys, name){
  return(as.numeric(number_monkeys[number_monkeys$name == name, "value1"]))
}

execute_op <- function(value1, op, value2){
  if (op == "+"){
    return(value1 + value2)
  } else if (op == "-"){
    return(value1 - value2)
  } else if (op == "*"){
    return(value1 * value2)
  } else if (op == "/"){
    return(value1 / value2)
  } else {
    print("help")
    return(NA)
  }
}

sm <- function(solvable_monkeys, number_monkeys){
  names = c()
  types = "number"
  value1 = c()
  operation = NA
  value2 = NA
  
  for (i in 1:nrow(solvable_monkeys)){
    names <- c(names, solvable_monkeys$name[i])
    value1 <- c(value1, 
                as.character(execute_op(get_value(number_monkeys, solvable_monkeys$value1[i]), 
                                        solvable_monkeys$operation[i], 
                                        get_value(number_monkeys, solvable_monkeys$value2[i]))))
  }
  
  out_tibble <- tibble(name = names,
                       type = types,
                       value1 = value1,
                       operation = operation,
                       value2 = value2)
  
  return(out_tibble)
}

# evaluate monkeys from ground up, converting from oper_monkeys to number_monkeys
oper_monkeys_remaining <- nrow(oper_monkeys)

while(oper_monkeys_remaining > 0){
  solvable_monkeys <- subset(oper_monkeys, value1 %in% number_monkeys$name & value2 %in% number_monkeys$name)
  
  solvable_monkeys <- sm(solvable_monkeys, number_monkeys)
  
  number_monkeys <- rbind(number_monkeys, solvable_monkeys)
  
  oper_monkeys <- subset(oper_monkeys, !name %in% solvable_monkeys$name)
  
  oper_monkeys_remaining <- nrow(oper_monkeys)
}

print(number_monkeys[number_monkeys$name == "root", "value1"])

# part 2

data <- readLines("input/day21.txt")

number_monkeys <- data[str_detect(data, "[0-9]")]
oper_monkeys <- data[!str_detect(data, "[0-9]")]

number_monkeys <- tibble(name = gsub(":.*", "", number_monkeys),
                         type = "number",
                         value1 = gsub(".*: ", "", number_monkeys),
                         operation = NA,
                         value2 = NA)
oper_monkeys <- tibble(name = gsub(":.*", "", oper_monkeys),
                       type = "operation",
                       value1 = gsub(" [-+*/].*", "", gsub(".*: ", "", oper_monkeys)),
                       operation = str_extract(oper_monkeys, "[-+*/]"),
                       value2 = gsub(".*[-+*/] ", "", oper_monkeys))

oper_monkeys[oper_monkeys$name == "root", "operation"] <- "="
root_monkey <- subset(oper_monkeys, name == "root")
oper_monkeys <- subset(oper_monkeys, name != "root")

humn_monkey <- subset(number_monkeys, name == "humn")
number_monkeys <- subset(number_monkeys, name != "humn")

# we can presolve anything that doesn't rely on "humn"

while(TRUE){
  solvable_monkeys <- subset(oper_monkeys, value1 %in% number_monkeys$name & value2 %in% number_monkeys$name)
  
  if (nrow(solvable_monkeys) == 0){
    break
  }
  
  solvable_monkeys <- sm(solvable_monkeys, number_monkeys)
  number_monkeys <- rbind(number_monkeys, solvable_monkeys)
  oper_monkeys <- subset(oper_monkeys, !name %in% solvable_monkeys$name)
}

number_monkeys <- rbind(number_monkeys,
                        humn_monkey)

for (humn_test in 1:1000){
  #print(humn_test)
  humn_test <- 3769656000000
  
  number_monkeys2 <- number_monkeys
  oper_monkeys2 <- oper_monkeys
  
  number_monkeys2[number_monkeys2$name == "humn", "value1"] <- as.character(humn_test)
  
  oper_monkeys2_remaining <- nrow(oper_monkeys2)
  
  while(oper_monkeys2_remaining > 0){
    solvable_monkeys <- subset(oper_monkeys2, value1 %in% number_monkeys2$name & value2 %in% number_monkeys2$name)
    
    solvable_monkeys <- sm(solvable_monkeys, number_monkeys2)
    
    number_monkeys2 <- rbind(number_monkeys2, solvable_monkeys)
    
    oper_monkeys2 <- subset(oper_monkeys2, !name %in% solvable_monkeys$name)
    
    oper_monkeys2_remaining <- nrow(oper_monkeys2)
  }
  
  root1 <- subset(number_monkeys2, name == root_monkey$value1, select = "value1")
  root2 <- subset(number_monkeys2, name == root_monkey$value2, select = "value1")
  
  print(as.numeric(c(humn_test, root1, root2)))
  
  if (root1 == root2){
    break
  }
}


test_humn <- function(number_monkeys, oper_monkeys, humn_test){
  
  number_monkeys2 <- number_monkeys
  oper_monkeys2 <- oper_monkeys
  
  number_monkeys2[number_monkeys2$name == "humn", "value1"] <- as.character(humn_test)
  
  oper_monkeys2_remaining <- nrow(oper_monkeys2)
  
  while(oper_monkeys2_remaining > 0){
    solvable_monkeys <- subset(oper_monkeys2, value1 %in% number_monkeys2$name & value2 %in% number_monkeys2$name)
    
    solvable_monkeys <- sm(solvable_monkeys, number_monkeys2)
    
    number_monkeys2 <- rbind(number_monkeys2, solvable_monkeys)
    
    oper_monkeys2 <- subset(oper_monkeys2, !name %in% solvable_monkeys$name)
    
    oper_monkeys2_remaining <- nrow(oper_monkeys2)
  }
  
  root1 <- subset(number_monkeys2, name == root_monkey$value1, select = "value1")
  root2 <- subset(number_monkeys2, name == root_monkey$value2, select = "value1")
  
  return(as.numeric(root1) - as.numeric(root2))
}

# from pattern, notice each increment decreases root1 by approximately 20, doesn't decrease root2
# so we can manually search by changing biggest digit to smallest digit, noting the value where
# it's still a positive difference just before becoming a negative difference
test_humn(number_monkeys, oper_monkeys, 3769668716709)
