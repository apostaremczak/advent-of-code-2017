# --- Day 4: High-Entropy Passphrases ---

library(sets)

puzzle_input <- read.csv("/home/angie/Code/advent-of-code/day4/input.txt", sep = " ", header = FALSE, col.names = 1:11)
# Fill empty cells with NAs
puzzle_input[which(puzzle_input == "", TRUE)] <- NA


# Part 1: a valid passphrase must contain no duplicate words.
is_not_duplicate <- function(row) {
    row_set <- as.set(unlist(row))
    return((length(row_set) - sum(is.na(row_set))) == sum(!is.na(row)))
}

count_valid <- sum(by(puzzle_input, 1:nrow(puzzle_input), is_not_duplicate))


