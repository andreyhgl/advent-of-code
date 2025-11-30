#!/usr/bin/env Rscript

dat <- readLines("data/day7.txt", warn = FALSE)
dat <- readLines("data/tmp.txt", warn = FALSE)

res <- as.numeric(sapply(dat, function(x) strsplit(x, ":")[[1]][1]))
numbers <- sapply(dat, function(x){
	x <- strsplit(x, ": ")[[1]][2]
	as.numeric(strsplit(x, " ")[[1]])
})

math <- c("", "*", "+")
ans <- 0
for (i in seq_along(res)){
	number <- unlist(unname(numbers[i]))
	if ( i %% 50 == 0 ) print(i)
	
	variants <- expand.grid(
		rep( list(math), (length(number) - 1) ),
		stringsAsFactors = FALSE
	)

	for (j in seq_len(nrow(variants))){
		variant <- variants[j, ]
		val <- eval(parse(text = paste0(number[1], variant[1], number[2])))
		
		for (k in 3:length(number)){
			val <- paste0(val, variant[k-1], number[k])
			val <- eval(parse(text = val))
		}

		print(paste(val, "|", j))

		#if (val > res[i]) break

		if (val == res[i]){
			ans <- ans + res[i]
			#print(j)
			break
		}
	}
}
options(scipen = 999)
print(ans)

# part 1
# 42283209483350