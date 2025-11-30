library(tidyverse)
options(scipen=999)

# day 6: calculate number of spawning fish
# Is this a fibonacci sequence????

# Given is a sequence of cycle days. At day 0 a new fish is spawned. A new fish is produced every 6 day for an adult, and every 8 day for a newborn.
# Calculate the number of fish after N days

# import sequence
d <- readLines("day6.txt") %>% strsplit(., split=",") %>% unlist %>% as.numeric

state <- d
num_days <- 80
spawn_time <- 8

# part one: days = 80
dirty_forloop <- function(state, num_days, spawn_time=8) {
  for (i in 1:num_days) {
    print(i)

    # all 0 = create new 8 value and reset 0 value to 6
    if (any(state %in% 0)) {
      zeros <- state %in% 0
      state[zeros] <- 6

      state[!zeros] <- state[!zeros] - 1

      state <- append(state, rep(spawn_time, sum(zeros)))

    } else {
      state <- state - 1
    }
  }

  print(length(state))
}
dirty_forloop(state, num_days) # answer: after 80 days = 365862 fish


# part two: days = 256

# age representation in a vector of 9 of counts (table)
# days: 8, 7, 6, 5, 4, 3, 2, 1, 0

pop <- as.vector(rev(table(d)))
pop <- c(rep(0, 3), pop, 0) # generate a vector matching the table-form of the population (dirty)

num_days <- 256
for (i in 1:num_days) {
  print(i)
  spawn <- pop[9]
  pop <- c(spawn, pop[1:8])
  pop[3] <- pop[3] + spawn
}
sum(pop) # 1653250886439
