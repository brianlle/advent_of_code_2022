# Advent of Code 2022, Day 10

data <- readLines("input/day10.txt")

# addx V takes two cycles to complete. 
# After two cycles, the X register is increased by the value V. (V can be negative.)
# noop takes one cycle to complete. It has no other effect.
# want 20, 60, 100, 140, 180, 220 cycles
# value is: cycle number * value

values <- c(1)
cycles <- c(1)
cur_value <- 1
cur_cycle <- 1

for (i in 1:length(data)){
  cur_inst <- data[i]
  
  if (cur_inst == "noop"){
    cur_cycle <- cur_cycle + 1
    cycles <- c(cycles, cur_cycle)
    values <- c(values, cur_value)
  } else {
    stored_value <- as.numeric(gsub(".* ", "", cur_inst))
    cur_cycle <- cur_cycle + 1
    cycles <- c(cycles, cur_cycle)
    values <- c(values, cur_value)
    
    cur_value <- cur_value + stored_value
    cur_cycle <- cur_cycle + 1
    cycles <- c(cycles, cur_cycle)
    values <- c(values, cur_value)
  }
}

print(cycles[20]*values[20] + cycles[60]*values[60] + cycles[100]*values[100] +
  cycles[140]*values[140] + cycles[180]*values[180] + cycles[220]*values[220])

# part 2

# if position +/- 1 matches current cycle (mod 40), lit as #; otherwise unlit as .

lit <- "#"
values <- c(1)
cycles <- c(1)
cur_value <- 1
cur_cycle <- 1

for (i in 1:length(data)){
  cur_inst <- data[i]

  if (cur_inst == "noop"){
    cur_cycle <- cur_cycle + 1
    cycles <- c(cycles, cur_cycle)
    values <- c(values, cur_value)
    
    cur_mod <- (cur_cycle-1) %% 40
    if (abs(cur_mod - cur_value) <= 1){
      lit <- paste0(lit, "#")
    } else {
      lit <- paste0(lit, ".")
    }
  } else {
    stored_value <- as.numeric(gsub(".* ", "", cur_inst))
    cur_cycle <- cur_cycle + 1
    cycles <- c(cycles, cur_cycle)
    values <- c(values, cur_value)
    
    cur_mod <- (cur_cycle-1) %% 40
    if (abs(cur_mod - cur_value) <= 1){
      lit <- paste0(lit, "#")
    } else {
      lit <- paste0(lit, ".")
    }
    
    cur_value <- cur_value + stored_value
    cur_cycle <- cur_cycle + 1
    cycles <- c(cycles, cur_cycle)
    values <- c(values, cur_value)
    
    cur_mod <- (cur_cycle-1) %% 40
    if (abs(cur_mod - cur_value) <= 1){
      lit <- paste0(lit, "#")
    } else {
      lit <- paste0(lit, ".")
    }
  }
}

cat(substr(lit, 1, 40), "\n",
    substr(lit, 41, 80), "\n",
    substr(lit, 81, 120), "\n",
    substr(lit, 121, 160), "\n",
    substr(lit, 161, 200), "\n",
    substr(lit, 201, 240))

####...##..##..####.###...##..#....#..#.
#.......#.#..#.#....#..#.#..#.#....#..#.
###.....#.#....###..#..#.#....#....####.
#.......#.#....#....###..#.##.#....#..#.
#....#..#.#..#.#....#....#..#.#....#..#.
####..##...##..#....#.....###.####.#..#.
