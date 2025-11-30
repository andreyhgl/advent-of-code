#!/usr/bin/env Rscript

dat <- readLines("data/day6.txt", warn = FALSE)
#dat <- readLines("data/tmp.txt", warn = FALSE)

mat <- t(unname(sapply(dat, function(x) strsplit(x, "")[[1]])))
mat <- as.matrix(mat)

col <- 0
row <- 0
for (i in seq_len(nrow(mat))){
	val <- paste(mat[, i], collapse = "")
	val <- regexpr("\\^", val)[[1]]

	if (val > 1){
		col <- i
		row <- val
	}
}

print(paste("Start position =", paste(c(row,col), collapse = ",")))

# start by going north, then east, south, west, repeat

north <- function(row, col){
	val <- paste(mat[, col], collapse = "")
	res <- gregexpr("\\#\\.+\\^", val)[[1]]
	return(res)
}

east <- function(row, col){
	val <- paste(mat[row, ], collapse = "")
	res <- gregexpr("\\^\\.+\\#", val)[[1]]
	return(res)
}

trip <- 0
#while (any(pos)){
	res <- north(row, col)

	len <- attributes(res)$match.length - 2 # remove the characters pos
	trip <- trip + len

	mat[row, col] <- "." # replace old pos
	row <- res[1] + 1
	mat[row, col] <- "^" # update new pos

	print(c(row, col))


	res <- east(row, col)

	len <- attributes(res)$match.length - 2 # remove the characters pos
	trip <- trip + len

	mat[row, col] <- "." # replace old pos
	col <- res[1] + len
	mat[row, col] <- "^" # update new pos

	print(c(row, col))



}


............................#.................^.................#