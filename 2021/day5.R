library(tidyverse)

# vents, make a grid of 1:1000 rows and 1:1000 cols
d <- read.delim("day5.txt", header=F)

dat <- d[,1] %>% strsplit(., split=" -> ") %>% unlist %>% matrix(., ncol=2, byrow=T)
dat <- apply(dat, 1, strsplit, split=",") %>% unlist %>% as.numeric %>% matrix(., ncol=4, byrow=T) %>% data.frame
colnames(dat) <- c("x1", "y1", "x2", "y2")

# part 1: only consider vertical and horizontal lines
grid <- matrix(0, nrow=1000, ncol=1000)
for (i in 1:nrow(dat)) {
  x <- dat$x1[i]:dat$x2[i]
  y <- dat$y1[i]:dat$y2[i]

  # only consider horisontal or vertical lines: x1=x2 or y1=y2
  if (dat$x1[i] == dat$x2[i] | dat$y1[i] == dat$y2[i]) {
    grid[x,y] <- grid[x,y] + 1
  }
}

sum(grid > 1) # 6397


# part two: include also the diagonal lines
grid <- matrix(0, nrow=1000, ncol=1000)
for (i in 1:nrow(dat)) {
  x <- dat$x1[i]:dat$x2[i]
  y <- dat$y1[i]:dat$y2[i]

  # only consider horisontal or vertical lines: x1=x2 or y1=y2
  if (dat$x1[i] == dat$x2[i] | dat$y1[i] == dat$y2[i]) {
    grid[x,y] <- grid[x,y] + 1
  } else {
    # also considor diagonal lines: crop the matrix and use the diag-function
    diag(grid[x,y]) <- diag(grid[x,y]) + 1
  }
}
sum(grid > 1) # 22335
