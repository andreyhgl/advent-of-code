# day1
library(tidyverse)
d <- readLines("day1.txt") %>% strsplit(., "") %>% unlist %>% gsub("[(]", 1, .) %>% gsub("[)]", -1, .) %>% as.numeric

# part one:
sum(d) # answer: 138

# part two: at which position is the sum -1 first time?
floor <- 0
for (i in 1:length(d)) {
  floor <- d[i] + floor
  if (floor == -1) {print(i); break}
}

# answer: 1771
