library(tidyverse)

# octopus flashes
# flash = number reaching 9, each number is increased every step and if an adjacent number reaches 9 (diagonals included)
d <- readLines("day11.txt") %>% strsplit(., split="") %>% Reduce(function(x,y) rbind(x,y),.) %>% apply(., c(1,2), as.numeric)
rownames(d) <- NULL

row <- 2
col <- 2

get_adjacent <- function(row, col) {
  top <- d[row - 1, c(col-1, col, col+1)]
  middle <-
  bot <-
}

# translate matrix into vector positions, two dimenstions into one. Make a function that makes this translation

# give a matrix cordinate, return vector position
convert <- function(x) {
  ncols <- ncol(d)
  nrows <- nrow(d)
}


c(t(x))


d %>% t %>% c
