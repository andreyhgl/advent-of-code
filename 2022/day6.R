data <- readLines("day6.txt")
data <- strsplit(data, "")[[1]]


for (i in seq_along(data)) {
  range <- i:(i + 3)
  marker <- data[range]
  if (length(unique(marker)) < 4) next
  if (length(unique(marker)) == 4) {
    print(i + 3)
    break
  }
}

# answer 1 : 1929
len <- 14

for (i in seq_along(data)) {
  range <- i:(i + len - 1)
  marker <- data[range]
  if (length(unique(marker)) < len) next
  if (length(unique(marker)) == len) {
    print(i + len - 1)
    break
  }
}
