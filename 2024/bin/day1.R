dat <- readLines("data/day1.txt")

l1 <- sapply(dat, function(x){
	x <- strsplit(x, " ")[[1]][1]
	as.numeric(x)
})
l1 <- unname(l1)

l2 <- sapply(dat, function(x){
	x <- strsplit(x, " ")[[1]]
	x <- x[length(x)]
	as.numeric(x)
})
l2 <- unname(l2)

l1 <- sort(l1)
l2 <- sort(l2)

# ~~ part 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
sum(abs(l1 - l2)) # 1341714


# ~~ part 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

numbers <- l1[l1 %in% l2]

out <- sapply(numbers, function(number){
	fac <- sum(l2 %in% number)
	number * fac
})

sum(out) # 27384707