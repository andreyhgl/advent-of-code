d <- read.csv("day4.txt", header = FALSE)

range_it <- function(string) {
  val <- as.numeric(strsplit(string, "-")[[1]])
  unlist(val[1]:val[2])
}

l1 <- apply(d, 1, function(x) {
  list(range_it(x[1]), range_it(x[2]))
})

foo <- function(x) {
  # full overlap
  area1 <- setdiff(unlist(x[1]), unlist(x[2]))
  area2 <- setdiff(unlist(x[2]), unlist(x[1]))
  
  length(area1) == 0 | length(area2) == 0
}

# answer 1
sum(unlist(lapply(l1, foo))) # 582


foo <- function(x) {
  # partial overlap
  area1 <- intersect(unlist(x[1]), unlist(x[2]))
  #area2 <- intersect(unlist(x[2]), unlist(x[1]))
  
  length(area1) > 0
}



# answer 2
sum(unlist(lapply(l1, foo))) # 893
