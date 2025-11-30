d <- readLines("day2.txt")

# opponent        # mine              # points
# A <- "rock"     # X <- 1 # rock     # lose <- 0
# B <- "paper"    # Y <- 2 # paper    # draw <- 3
# C <- "scissor"  # Z <- 3 # scissor  # win <- 6

# outcomes
outcomes <- data.frame(
  outcome = c(
    "A X", "A Y", "A Z",
    "B X", "B Y", "B Z",
    "C X", "C Y", "C Z"
  ),
  score = c(
    1 + 3, 2 + 6, 3 + 0,
    1 + 0, 2 + 3, 3 + 6,
    1 + 6, 2 + 0, 3 + 3
  ),
  score2 = c(
    0 + 3, 3 + 1, 6 + 2,
    0 + 1, 3 + 2, 6 + 3,
    0 + 2, 3 + 3, 6 + 1
  )
)

sum(table(d) * outcomes$score) # answer 1: 14264


# score2
# opponent        # mine      # points
# A <- "rock"     # X = lose  # lose <- 0
# B <- "paper"    # Y = draw  # draw <- 3
# C <- "scissor"  # Z = win   # win <- 6

sum(table(d) * outcomes$score2)
