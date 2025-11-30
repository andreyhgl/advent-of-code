# day 2

library(tidyverse)
d <- readLines("day2.txt") %>% data.frame(dim=.)

# part one: calculate the wrapping paper for each present
# formula:

# l, w, h
d$l <- 0
d$w <- 0
d$h <- 0

d[, 2:4] <- sapply(d$dim, function(x) {
  strsplit(x, split="x") %>% unlist %>% as.numeric
}) %>% t

f <- function(dim) {
  l <- dim[1]
  w <- dim[2]
  h <- dim[3]

  a <- l*w
  b <- l*h
  c <- w*h

  package <- c(a, b, c)

  extra <- min(package)

  return(sum(package*2, extra))
}

apply(d[,2:4], 1, f) %>% sum # answer: 1586300

# part two: calculate ribbon for each present
# formula: distance of the two smallest sides, twice
# plus the volume of the present, l x w x h

ribbon <- function(dim) {
  l <- dim[1]
  w <- dim[2]
  h <- dim[3]

  smallest1 <- dim[which.min(dim)]
  smallest2 <- min(dim[-which.min(dim)])

  ribbon_foot <- l * w * h
  ribbon_len <- smallest1 + smallest1 + smallest2 + smallest2
  return(sum(c(ribbon_foot, ribbon_len)))
}
apply(d[,2:4], 1 ,ribbon) %>% sum # answer: 3737498











#
