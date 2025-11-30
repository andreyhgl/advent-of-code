library(tidyverse)

# heapmap: find the lowest value compared to the adjacent values. Above, below, left and right of the value
d <- readLines("day9.txt") %>% strsplit(., "") %>% Reduce(function(x,y) rbind(x,y), .) %>% as.matrix %>% apply(., 1, as.numeric)
colnames(d) <- NULL

get_adjacent_values <- function(y, x) {
  # bounderies to stay within the array
  if (y > 1) {above <- d[y-1, x]; above_pos <- c(y-1, x)} else {above <- NA; above_pos <- NA}
  #above <- ifelse(y > 1, d[y-1, x], NA)

  if (x < ncol(d)) {right <- d[y, x+1]; right_pos <- c(y, x+1)} else {right <- NA; right_pos <- NA}
  #right <- ifelse(x < ncol(d), d[y, x+1],  NA)
  if (y < nrow(d)) {below <- d[y+1, x]; below_pos <- c(y+1, x)} else {below <- NA; below_pos <- NA}
  #below <- ifelse(y < nrow(d), d[y+1, x], NA)
  if (x > 1) {left <- d[y, x-1]; left_pos <- c(y, x-1)} else {left <- NA; left_pos <- NA}
  #left <- ifelse(x > 1, d[y, x-1], NA)

  #c(left, right, above, below)
  out <- list()
  out$order <- c("above", "right", "below", "left")
  out$values <- c(above, right, below, left)
  out$pos <- c(above_pos, right_pos, below_pos, left_pos)

  out
}

risk <- 0
for (y in 1:ncol(d)) {
  for (x in 1:nrow(d)) {
    val <- d[y,x]
    adjacent_values <- get_adjacent_values(y,x)$values

    if (all(val < adjacent_values, na.rm=T)) risk <- risk + (val + 1)
  }
}
print(risk) # 539

# part two: identify the top 3 basins
# Basin = the lowest point and it's surrounding values (above, below, left, right) that increases w/ +1, but != 9
# iterate over all values in the matrix, check adjacent values, mark if +1 & < 9,











# OLD
# part two: identify the top 3 basins. Basin = the lowest point and it's surrounding values (above, below, left, right) that increases w/ +1
# submit: the product of the sizes of the top 3 basins
ocean_floor <- d
ocean_floor[] <- 0

for (y in 1:ncol(d)) {
  for (x in 1:nrow(d)) {
    val <- d[y,x]
    adjacent_values <- get_adjacent_values(y,x)$values

    if (all(val < adjacent_values, na.rm=T)) ocean_floor[y,x] <- 1
  }
}

# for every lowest point use the function basin_crawler to calc the size
lowest_points <- which(ocean_floor == 1)


i <- 31
d[40:45,10:14]

for (i in 1:length(lowest_points)) {

}
point <- lowest_points[i]
x <- ifelse(point > 100, point %/% 100, point) + 1
y <- ifelse(point > 100, point - (point %/% 100 * 100), point)

val <- d[y,x]

basin <- d
basin[] <- 0
basin[y, x] <- 1

av <- get_adjacent_values(y,x) # adjacent_values
check <- val + 1 == av$values

# above
if (check[1]) {
  y2 <- av$pos[1]
  x2 <- av$pos[2]
  basin[y2, x2] <- 1

}

# right
if (check[1])

# below
if (check[1])

# left
if (check[1])






basin > 0 %>% sum # calculates basin size

basin_crawler <- function(y, x) {

}
