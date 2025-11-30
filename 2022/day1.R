d <- as.numeric(readLines("day1.txt"))

# make index table, col1 = id, col2 = rows to sum
# start, end, break, sum
a <- matrix(
  nrow = sum(is.na(d)),
  ncol = 4
)

# break = all NAs
a[, 3] <- which(is.na(d))

# end = one element prior to break
a[, 2] <- a[, 3] - 1

# start = one element prior to previous break
# remove last value
a[, 1] <- c(1, a[, 3] + 1)[1:nrow(a)]

# sum the values, use start and end from matrix
a[, 4] <- apply(a[, 1:2], 1, function(x) {
  start <- x[1]
  end <- x[2]

  sum(d[start:end])
})

max(a[, 4]) # puzzle 1: 72602


sum(sort(a[, 4], decreasing = TRUE)[1:3]) # puzzle 2: 207410
