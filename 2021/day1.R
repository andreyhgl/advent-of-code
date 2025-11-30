# Sonar measurments
d <- as.vector(read.table("day1.txt")[,1])

# Exercies 1: How many measurements are larger than the previous measurement?
foo <- function(d) {
  dat <- data.frame(input=c(NA, d), skewed=c(d, NA), diff=0)
  dat$diff <- dat$skewed - dat$input
  dat$diff <- ifelse(dat$diff > 0, 1, 0)
  return(sum(dat$diff, na.rm=T))
}
foo(d) # 1367

# Cleaner version:
sum(diff(d, lag=1) > 0)





# How many measurements are on average larger? This average being a sliding window of 3 values
sliding_window <- function(d, num=3) {
  out <- numeric()
  while (i <= length(d) - num + 1) {
    range <- c(i:(i+num-1))
    val <- sum(d[range])
    out <- append(out, val)
    #print(sum(d[range]))
    i <- i + 1
  }
  return(out)
}
d2 <- sliding_window(d, 3)

foo(d2) # 1362



# Solution with packages??
#library(gtools)
#running(d, 3, )
