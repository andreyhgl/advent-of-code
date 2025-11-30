d <- readLines("day3.txt")
d <- strsplit(d, "")

foo <- function(x) {
  len <- length(x)
  end <- len / 2
  start2 <- end + 1
  x1 <- x[1:end]
  x2 <- x[start2:len]
    
  char <- unique(x1[x1 %in% x2])
  
  which(c(letters, LETTERS) %in% char)
}

# answer 1
sum(sapply(d, foo)) # 7746


start <- seq(1, length(d), 3)
end <- seq(3, length(d), 3)

num <- numeric()
for (i in seq_along(start)) {
  char <- Reduce(intersect, d[start[i]:end[i]])
  out <- which(c(letters, LETTERS) %in% char)
  num <- append(num, out)
}

# answer 2
sum(num) # 2604
