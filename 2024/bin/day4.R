#!/usr/bin/env Rscript

dat <- readLines("data/day4.txt", warn = FALSE)

mat <- t(unname(sapply(dat, function(x) strsplit(x, "")[[1]])))
mat <- as.matrix(mat)

# locate XMAS or SAMX either: (1) horizontally, (2) vertically, (3) diagonally

foo <- function(x){
	x <- paste(x, collapse = "")

	words <- c("XMAS", "SAMX")

	res <- sapply(words, function(word){
		out <- gregexpr(word, x)[[1]]
		res <- 0
		if (out[1] != -1) res <- length(out)
		return(res)
	})

	return(sum(res))
}

horizontal <- apply(mat, 1, foo)

vertical <- apply(mat, 2, foo)

diagonal <- numeric()

for (col in seq_len(ncol(mat))){
	if (col == ncol(mat)) next

	x <- diag(mat[, col:ncol(mat)])
	diagonal <- c(diagonal, foo(x))
}
for (row in seq_len(nrow(mat))){
	if (row == nrow(mat) | row == 1) next

	x <- diag(mat[row:nrow(mat), ])
	diagonal <- c(diagonal, foo(x))
}

# rotate 90 degree
rotate <- function(mat) t(apply(mat, 2, rev))

mat <- rotate(mat)
for (col in seq_len(ncol(mat))){
	if (col == ncol(mat)) next

	x <- diag(mat[, col:ncol(mat)])
	diagonal <- c(diagonal, foo(x))
}

for (row in seq_len(nrow(mat))){
	if (row == nrow(mat) | row == 1) next
	
	x <- diag(mat[row:nrow(mat), ])
	diagonal <- c(diagonal, foo(x))
}

print(sum(horizontal, vertical, diagonal)) # 2569



# part 2