# syntax checker
# part one: identify incomplete code chunks. Save the parser. Apply penalty score. Sum

# Parser conversion index
# 1 = (
# 2 = )
# 3 = [
# 4 = ]
# 5 = {
# 6 = }
# 7 = <
# 8 = >

library(tidyverse)
d <- readLines("day10.txt") %>% strsplit(., split="")

# Functions
f <- function(x) {
  x <- x %>% gsub("[(]", 1, .)
  x <- x %>% gsub("[)]", 2, .)
  x <- x %>% gsub("[[]", 3, .)
  x <- x %>% gsub("[]]", 4, .)
  x <- x %>% gsub("[{]", 5, .)
  x <- x %>% gsub("[}]", 6, .)
  x <- x %>% gsub("<", 7, .)
  x <- x %>% gsub(">", 8, .)

  x %>% as.numeric
}
foo <- function(v) {
  # iterate through the vector, if odd number (opener): store id, if even number (closer): the number need to match it's chunk opener
  counter <- numeric()
  for (i in 1:length(v)) {
    val <- v[i]

    # if odd: store
    if (val %% 2 == 1) counter <- c(counter, val)

    # if even: check the previous value
    if (val %% 2 == 0) {
      prev_val <- counter[length(counter)]
      if (val == (prev_val + 1)) {
        counter <- counter[-length(counter)]
      } else {
        return(val)
        break
      }
    }
  }
}
penalty_scores <- function(error) {
  scores <- table(error)
  scores[1] * 3 + scores[2] * 57 + scores[3] * 1197 + scores[4] * 25137
}



v <- lapply(d, f)
lapply(v, foo) %>% unlist %>% penalty_scores # answer: 323613

# part two: discard corrupted lines. Identify missing closer-parsers in their respective order order. Calculate the penalty score. Find median
corrupt <- lapply(v, foo) %>% lapply(., function(x) !is.null(x)) %>% unlist
v <- v[!corrupt]

foo <- function(v) {
  # iterate through the vector, if odd number (opener): store id, if even number (closer): the number need to match it's chunk opener
  counter <- numeric()
  for (i in 1:length(v)) {
    val <- v[i]

    # if odd: store
    if (val %% 2 == 1) counter <- c(counter, val)

    # if even: check the previous value
    if (val %% 2 == 0) {
      prev_val <- counter[length(counter)]
      if (val == (prev_val + 1)) {
        counter <- counter[-length(counter)]
      }
    }
  }
  out <- counter %>% rev + 1
  out / 2
}
calc_pen <- function(x) {
  score <- 0
  for (i in 1:length(x)) {
    val <- x[i]
    score <- score * 5 + val
  }
  return(score)
}

penalties <- lapply(v, foo)
q <- lapply(penalties, calc_pen) %>% unlist
median(q) # answer: 3103006161
