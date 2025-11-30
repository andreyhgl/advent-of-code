d <- as.vector(read.table("day1.txt")[,1])


# Find 2 values that sum up to 2020
foo <- function(x, value) {
  sum(value, x)
}

value <- d[i]
l1 <- sapply(vec, foo, value)
any(l1 %in% 2020)

values_to_save <- c()
for (i in 1:length(d)) {
  value <- d[i]
  vec <- d[-i]

  l1 <- lapply(vec, foo, value)
  if (any(l1 %in% 2020)) {
    print(value)
    values_to_save <- c(values_to_save, value)
  }

}

# Find 3 values that sum up to 2020
val_to_save <- c()
for (i in 1:length(d)) {
  val1 <- d[i]
  vec1 <- d[-i]

  for (j in 1:length(vec1)) {
    val2 <- vec1[j]
    vec2 <- vec1[-j]

    new_vec <- vec2 + val1 + val2

    if (any(new_vec %in% 2020)) {
      print(c(val1, val2))
      val_to_save <- c(val_to_save, val1)
      break
    }
  }
}
prod(val_to_save)



for (i in 1:length(d)) {
  value <- d[i]
  vec <- d[-i]

  for (j in 1:length(vec)) {
    value2 <- vec[j]
    vec2 <- vec[-j]
  }
}
