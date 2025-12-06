#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly=T)

txt <- readLines(args[1], warn=F)

#rotation <- substr(txt, 1, 1)
#number <- substr(txt, 2, 100)

pos <- 50
counter <- 0
counter2 <- 0
for (value in txt){
	rotation <- substr(value, 1, 1)
	number <- as.numeric(substr(value, 2, 100))

	if (number > 100) counter2 <- counter2 + floor(number / 100)

	if (rotation == "L") pos <- pos - number %% 100
	if (rotation == "R") pos <- pos + number %% 100
	pass <- "false"
	if (pos == 0 || pos == 100) counter <- counter + 1

	if (pos < 0){
		if (number > 1) counter2 <- counter2 + 1
		pos <- pos + 100

		#print(pos)
		pass <- "true"
	}
	if (pos > 100){
		pos <- pos - 100
		counter2 <- counter2 + 1
		#print(pos)
		pass <- "true"
	}
	print(c(value, pos, pass))

}
print(counter) # 1129
print(counter2)
print(counter2 + counter) # 7205 wrong
#print(txt)