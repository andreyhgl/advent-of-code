dat <- readLines("data/day2.txt", warn = FALSE)

# diff > 0 < 4
#report <- as.numeric(strsplit(dat[4], " ")[[1]])

# ~~ part 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

reports <- sapply(dat, function(report){
	report <- as.numeric(strsplit(report, " ")[[1]])
	report <- diff(report)

	all(report > 0 & report < 4) | all(report < 0 & report > -4)
})
print(sum(reports)) # 332

# ~~ part 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

foo <- function(report, increasing = TRUE){
	steps <- diff(report)
	
	if (increasing) steps > 0 & steps < 4 else steps < 0 & steps > -4
}

reports <- sapply(dat, function(report){
	report <- as.numeric(strsplit(report, " ")[[1]])

	out <- FALSE

	test <- foo(report, increasing = TRUE)
	
	if (all(test)) out <- TRUE

	if (sum(!test) == 1){
		# remove values around the FALSE, add + 1
		index <- which(!test)

		new_report <- report[-index]
		if (all(foo(new_report, increasing = TRUE))) out <- TRUE

		new_report <- report[-(index+1)]
		if (all(foo(new_report, increasing = TRUE))) out <- TRUE
	}

	test <- foo(report, increasing = FALSE)
	
	if (all(test)) out <- TRUE

	if (sum(!test) == 1){
		# remove values around the FALSE, add + 1
		index <- which(!test)

		new_report <- report[-index]
		if (all(foo(new_report, increasing = FALSE))) out <- TRUE

		new_report <- report[-(index+1)]
		if (all(foo(new_report, increasing = FALSE))) out <- TRUE
	}

	return(out)
})
print(sum(reports)) # 388

system.time(reports <- sapply(dat, function(report){
	report <- as.numeric(strsplit(report, " ")[[1]])

	out <- FALSE

	test <- foo(report, increasing = TRUE)
	if (all(test)) out <- TRUE

	test <- foo(report, increasing = FALSE)
	if (all(test)) out <- TRUE

	for (i in seq_along(report)){
		new_report <- report[-i]
		
		test <- foo(new_report, increasing = TRUE)
		if (all(test)) out <- TRUE

		test <- foo(new_report, increasing = FALSE)
		if (all(test)) out <- TRUE

		if (out) break
	}
	return(out)
}))
sum(reports) # 398