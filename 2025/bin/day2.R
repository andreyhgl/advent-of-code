#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=T)

txt <- readLines(args[1], warn=F)

input <- strsplit(txt, ",")[[1]]


make_range <- function(x){
	values <- strsplit(x, "-")[[1]]
	first <- values[1]
	last <- values[2]
	first : last
}

foo <- function(value){
	middle <- nchar(value) / 2
	first <- substr(value, 1, middle)
	last <- substr(value, middle + 1, nchar(value))
	if (first == last) value
}

counter <- 0
for (values in input){
	range <- make_range(values)
	total <- sum(unlist(sapply(range, foo)))
	counter <- counter + total
}

print(counter) # 54641809925