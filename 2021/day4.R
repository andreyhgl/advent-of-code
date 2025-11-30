library(tidyverse)
# submarine bingo

# part one: which board will win first?
# sum of all unmarked numbers on the board * the last number that was drawn

input <- read.delim("day4.txt", header=F)

numbers <- input[1,] %>% strsplit(., split=",") %>% unlist %>% as.numeric

# Array format
grab.boards <- function(input) {
  boards <- numeric()
  size <- 5
  start <- 2
  end <- start+size-1
  i <- 1

  while (end <= nrow(input)) {
    range <- start:end

    vec <- input[range,] %>% strsplit(., split=" ") %>% unlist %>% .[!. %in% ""] %>% as.numeric

    boards <- append(boards, vec)
    #show(mat)

    start <- end+1
    end <- start+size-1
    i <- i + 1
  }
  array(boards, dim=c(5,5,(length(boards)/5/5)))
  #return(boards)
}
boards <- grab.boards(input)
bingos <- boards
bingos[] <- FALSE

stop <- FALSE
for (N in numbers) {
  rows <- boards %in% N
  bingos[rows] <- 1

  for (i in 1:dim(bingos)[3]) {
    colBingo <- bingos[,,i] %>% colSums == 5
    rowBingo <- bingos[,,i] %>% rowSums == 5

    if (any(colBingo | rowBingo)) {
      stop <- TRUE
      print(paste0("i=", i, "      N=", N))
      break

    }
  }
  if (stop) break
}

winner <- array(bingos[,,i]) %in% 0
array(boards[,,i])[winner] %>% sum * N # answer: 10374

# part two: which board will win last?
# again, sum all the unmarked numbers on the boards * the last number that was drawn

bingos <- boards
bingos[] <- FALSE

# iterate over bingo-boards
board_ids <- 1:100

for (N in numbers) {
  rows <- boards %in% N
  bingos[rows] <- 1

  for (i in board_ids) {
    colBingo <- bingos[,,i] %>% colSums == 5
    rowBingo <- bingos[,,i] %>% rowSums == 5

    if (any(colBingo | rowBingo)) {
      board_ids <- board_ids[board_ids!=i] # remove the value of i, representing the bingo id
    }
  }
  if (length(board_ids)==0) break
}

winner <- array(bingos[,,i]) %in% 0
array(boards[,,i])[winner] %>% sum * N # answer: 24742
