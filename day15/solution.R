# --- Day 15: Dueling Generators ---

# Puzzle input:
# Generator A starts with 699
# Generator B starts with 124


generator_A <- list(value=699, factor=16807)
generator_B <- list(value=124, factor=48271)
generators <- list(generator_A, generator_B)

next_value <- function(generator) return((generator$value * generator$factor) %% 2147483647)
to_bit <- function(value) return(rev(as.numeric(intToBits(value))))
matching <- function(values) return(all(values[[1]][17:32] == values[[2]][17:32]))

check_next_match <- function(generators) {
    values <- list()
    for(j in 1:2) {
        value <- next_value(generators[[j]])
        values[[j]] <- to_bit(value)
        generators[[j]]$value <- value
        print(generators[[j]])
    }
    print('==========')
    print(values)
    print(matching(values))
    return(generators)
}

match_count <- 0
for (i in 1:40000000) {
    bits <- list()
    for(j in 1:2) {
        value <- next_value(generators[[j]])
        bits[[j]] <- to_bit(value)
        generators[[j]]$value <- value
    }
    if (matching(bits)) match_count <- match_count + 1
}
print(match_count)
