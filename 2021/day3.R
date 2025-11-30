# part one: submarine power consumption
d <- readLines("day3.txt")
x <- strsplit(d, split="")
x <- data.frame(x)
colnames(x) <- paste0("col", 1:length(x))

gamma <- numeric()
epsilon <- numeric()
for (i in 1:nrow(x)) {
  tmp <- as.vector(as.numeric(x[i,]))
  gamma <- append(gamma, ifelse(sum(tmp)>500, 1, 0)) # most common
  epsilon <- append(epsilon, ifelse(sum(tmp)<500, 1, 0))
}

# Collapse vector into string
gamma <- paste(gamma, collapse="")
epsilon <- paste(epsilon, collapse="")

# Convert binary into integer
gamma_rate <- strtoi(gamma, 2)
epsilon_rate <- strtoi(epsilon, 2)

gamma_rate * epsilon_rate # answer: 1082324


# part two: life support rating
# oxygen gererator rating
# CO2 scrubber rating

# Bit criteria:
# - find the most common bit for the first position, if = 1, keep only the strings with 1 in that position
# - continue with the remaining strings, now using the second bit. Again, find the most common, keep the strings with that number


# Oxygen
dat <- x
for (i in 1:nrow(x)) {
  tmp <- as.vector(as.numeric(dat[i,]))

  if (sum(tmp) >= length(dat)/2) { # save 1
    dat <- dat[, tmp %in% 1]
  } else { # save 0
    dat <- dat[, tmp %in% 0]
  }
  if (is.character(dat)) break
  print(i)
}
oxygen <- paste(dat, collapse="")

# CO2
dat <- x
for (i in 1:nrow(x)) {
  tmp <- as.vector(as.numeric(dat[i,]))

  if (sum(tmp) >= length(dat)/2) { # save 0
    dat <- dat[, tmp %in% 0]
  } else {
    dat <- dat[, tmp %in% 1]
  }

  if (is.character(dat)) break
  print(i)
}
CO2 <- paste(dat, collapse="")

oxygen_rating <- strtoi(oxygen, 2)
CO2_rating <- strtoi(CO2, 2)

oxygen_rating * CO2_rating # answer: 1353024
