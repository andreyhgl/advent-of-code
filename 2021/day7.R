# day7

# Find the most central point between all points:
# median + sum

library(tidyverse)

d <- readLines("day7.txt") %>% strsplit(., split=",") %>% unlist %>% as.numeric

abs(d - median(d)) %>% sum # answer: 328262



# part two: find the shortest distance between all points and(!) add calc the distance by adding each point:
# mean + aritmetic sum
# Note, some values are 0, which messes with the mean. Add +1 to everthing
foo <- function(x) {
  if (x==1) return(1)
  else return(x + foo(x-1))
}

d2 <- d+1
mean <- round(mean(d2))
dist <- abs(d2 - mean)

sapply(dist, foo) %>% sum # answer: 90040997

#abs(d - round(mean(d))) %>% sum
#abs(d2 - round(mean(d2))) %>% sum




## From reddit: https://www.reddit.com/r/adventofcode/comments/rar7ty/comment/hnl7l9d/?utm_source=share&utm_medium=web2x&context=3
# answer part 1
y <- d
y1 <- lapply(0:max(y$value), \(x) sum(y[, abs(value - x)]))
y1[which.min(y1)]
# answer part 2
y2 <- lapply(0:max(y$value), \(x) sum(y[, abs(value - x)] * (y[, abs(value - x)] + 1) / 2))
y2[which.min(y2)]
