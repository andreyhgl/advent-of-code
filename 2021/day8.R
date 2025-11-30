#day8

library(tidyverse)
d <- readLines("day8.txt") %>% strsplit(d, split=" [|] ")
output <- d %>% lapply(., function(x) x[2]) %>% unlist %>% strsplit(., split=" ")
input <- d %>% lapply(., function(x) x[1]) %>% unlist %>% strsplit(., split=" ")

# part one: count number of 1, 7, 4, 8 in the dataset. These are unique due to their length of characters, nchar
# [nchar(x) = the_number] 2 = 1, 3 = 7, 4 = 4, 7 = 8
output %>% unlist %>% nchar %in% c(2, 3, 4, 7) %>% sum


# part two: decipher the inputs.
str_sorter <- function(x) paste(sort(unlist(strsplit(x, split=""))), collapse="")
vec_sorter <- function(x) unlist(lapply(x, str_sorter))
find_string <- function(input_string, number, known_number=FALSE) {
  number_string <- strsplit(number, "") %>% unlist

  if (is.character(known_number)) input_string <- input_string[input_string != known_number]

  idx <- input_string %>%
    strsplit(., "") %>%
    sapply(., function(x) {
      x %in% number_string %>% sum == nchar(number)
    })
  input_string[idx]
}
find_five <- function(input_string, number, known_number=FALSE) {
  number_string <- strsplit(number, "") %>% unlist

  if (is.character(known_number)) input_string <- input_string[input_string != known_number]

  idx <- input_string %>%
    strsplit(., "") %>%
    sapply(., function(x) {
      x %in% number_string %>% sum == 3
    })
  input_string[idx]
}
num_identifier <- function(x) {
  six_lines <- x[nchar(x) %in% 6]   # represents: 0, 6, 9
  five_lines <- x[nchar(x) %in% 5]  # represents: 2, 3, 5

  one <- x[nchar(x) %in% 2]
  seven <- x[nchar(x) %in% 3]
  eight <- x[nchar(x) %in% 7]
  four <- x[nchar(x) %in% 4]

  # 3 is five_lines which matches the number 7
  three <- find_string(five_lines, seven)
  # 9 is six_lines which matches the number 4
  nine <- find_string(six_lines, four)

  # 0 is six_lines which matches the number 1, w/ number 9 known
  zero <- find_string(six_lines, one, nine)

  # leftovers
  six <- six_lines[!six_lines %in% c(zero, nine)]

  five <- find_five(five_lines, four, three)
  two <- five_lines[!five_lines %in% c(five, three)]

  data.frame(number=0:9, id=c(zero, one, two, three, four, five, six, seven, eight, nine))
}
decipher <- function(input, output) {
  dat <- num_identifier(input)
  match(output, dat$id) %>% dat$number[.] %>% paste(., collapse="") %>% as.numeric
}


input <- input %>% lapply(., vec_sorter)
output <- output %>% lapply(., vec_sorter)

answer <- 0
for (i in 1:length(input)) {
  answer <- decipher(input[[i]], output[[i]]) + answer
}
print(answer)
