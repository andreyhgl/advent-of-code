d <- readLines("day5.txt")

positions <- d[1:8]
ordering <- d[11:length(d)]

# extract crate ID.
foo <- function(x) {
  i <- seq(2, 34, 4)
  out <- strsplit(x, "")[[1]][i]
  out <- sub(" ", NA, out)
  return((out))
}
crates <- sapply(positions, foo)

res <- vector(mode = "list", length = 9)
