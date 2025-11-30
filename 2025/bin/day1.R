args <- commandArgs(trailingOnly=T)

txt <- readLines(args[1], warn=F)

print(txt)