dat <- readLines("data/day3.txt", warn = FALSE)
dat <- paste(dat, collapse = "")

library(stringr) # https://stringr.tidyverse.org/index.html

foo <- function(string){
	string <- str_extract_all(dat, "mul\\([0-9]+,[0-9]+\\)")
	string <- unlist(string)
	string <- gsub("mul\\(", "", string)
	string <- gsub("\\)", "", string)

	val1 <- as.numeric(sapply(string, function(x) strsplit(x, ",")[[1]][1]))
	val2 <- as.numeric(sapply(string, function(x) strsplit(x, ",")[[1]][2]))

	sum(val1 * val2) 
}

foo(dat) # 178794710


# part 2 inspiration: 
# https://github.com/plannapus/Advent_of_Code/blob/master/2024/day03.R

#gregexpr, global regular expression
x <- gregexpr("mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don\\'t\\(\\)", dat)
list <- regmatches(dat, x)[[1]]

foo <- function(x){
	x <- gsub("mul\\(|\\)", "", x)
	x <- strsplit(x, ",")[[1]]
	prod(as.numeric(x))
}

do <- TRUE
res <- 0
for (i in seq_along(list)){
	if (list[i] == "do()") do <- TRUE
	if (list[i] == "don't()") do <- FALSE
	if (grepl("mul\\(", list[i]) & do) res <- res + foo(list[i])
}