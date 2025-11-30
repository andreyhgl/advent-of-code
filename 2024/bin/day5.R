#!/usr/bin/env Rscript

dat <- readLines("data/day5.txt", warn = FALSE)
#dat <- readLines("data/tmp.txt", warn = FALSE)

rules <- dat[grepl("\\|", dat)]
manuals <- dat[grepl(",", dat)]

rules <- lapply(rules, function(x){
	as.numeric(strsplit(x, "\\|")[[1]])
})

manuals <- lapply(manuals, function(x){
	as.numeric(strsplit(x, ",")[[1]])
})

unsafe <- numeric()
for (i in seq_along(manuals)){
	for (j in seq_along(rules)){
		res <- match(rules[[j]], manuals[[i]])

		if (sum(is.na(res)) <= 2 & sum(is.na(res)) != 0) next

		if (res[1] > res[2]) unsafe <- c(unsafe, i)
	}
}
unsafe <- unique(unsafe)
safe <- !seq_along(manuals) %in% unsafe

middle <- sapply(manuals[safe], function(x){
	x <- unlist(x)
	middle <- median(seq_along(x))
	x[middle]
})

sum(middle)


# part 2

for (i in seq_along(manuals[unsafe])){
	for (j in seq_along(rules)){
		res <- match(rules[[j]], manuals[unsafe][[i]])

		if (sum(is.na(res)) <= 2 & sum(is.na(res)) != 0) next

		if (res[1] > res[2]){
			x <- manuals[unsafe][[i]]
			val1 <- x[res[1]]
			val2 <- x[res[2]]
			x[res] <- c(val2, val1)
			manuals[unsafe][[i]] <- x
		}
	}
}

middle <- sapply(manuals[unsafe], function(x){
	x <- unlist(x)
	middle <- median(seq_along(x))
	x[middle]
})

sum(middle)

for (i in seq_along(manuals[unsafe])){
	for (j in seq_along(rules)){
		res <- match(rules[[j]], manuals[unsafe][[i]])

		if (sum(is.na(res)) <= 2 & sum(is.na(res)) != 0) next

		if (res[1] > res[2]){
			x <- manuals[unsafe][[i]]
			val1 <- x[res[1]]
			val2 <- x[res[2]]
			x[res] <- c(val2, val1)
			manuals[unsafe][[i]] <- x
		}
	}
}

middle <- sapply(manuals[unsafe], function(x){
	x <- unlist(x)
	middle <- median(seq_along(x))
	x[middle]
})

sum(middle)

for (i in seq_along(manuals[unsafe])){
	for (j in seq_along(rules)){
		res <- match(rules[[j]], manuals[unsafe][[i]])

		if (sum(is.na(res)) <= 2 & sum(is.na(res)) != 0) next

		if (res[1] > res[2]){
			x <- manuals[unsafe][[i]]
			val1 <- x[res[1]]
			val2 <- x[res[2]]
			x[res] <- c(val2, val1)
			manuals[unsafe][[i]] <- x
		}
	}
}

middle <- sapply(manuals[unsafe], function(x){
	x <- unlist(x)
	middle <- median(seq_along(x))
	x[middle]
})

sum(middle)

for (i in seq_along(manuals[unsafe])){
	for (j in seq_along(rules)){
		res <- match(rules[[j]], manuals[unsafe][[i]])

		if (sum(is.na(res)) <= 2 & sum(is.na(res)) != 0) next

		if (res[1] > res[2]){
			x <- manuals[unsafe][[i]]
			val1 <- x[res[1]]
			val2 <- x[res[2]]
			x[res] <- c(val2, val1)
			manuals[unsafe][[i]] <- x
		}
	}
}

middle <- sapply(manuals[unsafe], function(x){
	x <- unlist(x)
	middle <- median(seq_along(x))
	x[middle]
})

sum(middle)

for (i in seq_along(manuals[unsafe])){
	for (j in seq_along(rules)){
		res <- match(rules[[j]], manuals[unsafe][[i]])

		if (sum(is.na(res)) <= 2 & sum(is.na(res)) != 0) next

		if (res[1] > res[2]){
			x <- manuals[unsafe][[i]]
			val1 <- x[res[1]]
			val2 <- x[res[2]]
			x[res] <- c(val2, val1)
			manuals[unsafe][[i]] <- x
		}
	}
}

middle <- sapply(manuals[unsafe], function(x){
	x <- unlist(x)
	middle <- median(seq_along(x))
	x[middle]
})

sum(middle)