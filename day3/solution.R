# --- Day 3: Spiral Memory ---

test_inputs <- c(1, 12, 23, 1024)
puzzle_input <- 347991

# Create a spiral plane up to a given number field
create_plane <- function(field) {
    # Determine minimal n so that the field is contained in a n x n spiral matrix
    n <- 1
    while(!(field <= n^2)) n <- n + 2

    plane <- matrix(numeric(n^2), n, n, byrow = TRUE)
    centre <- median(seq_len(n))
    x <- y <- centre
    dir <- "R"
    xmx <- ymx <- cnt <- 1
    for(i in 1:n^2) {
        plane[x, y] <- i
        if(dir == "R") {
            if(xmx>0) {
                x=x+1
                xmx=xmx-1
            }
            else {
                dir="U"
                ymx=cnt
                y=y-1
                ymx=ymx-1
            }
            next
        }
        if(dir == "U") {
            if(ymx>0) {
                y=y-1
                ymx=ymx-1
            }
            else {
                dir="L"
                cnt=cnt+1
                xmx=cnt
                x=x-1
                xmx=xmx-1
            }
            next
        }
        if(dir == "L") {
            if(xmx>0) {
                x=x-1
                xmx=xmx-1
            } 
            else {
                dir="D"
                ymx=cnt
                y=y+1
                ymx=ymx-1
                }
            next
        } 
        if(dir == "D") {
            if(ymx>0) {
                y=y+1
                ymx=ymx-1
            } 
            else {
                dir="R"
                cnt=cnt+1
                xmx=cnt
                x=x+1
                xmx=xmx-1
            }
            next
        }
    }

    return(plane)
}


# Calculate the Manhattan distance between two points
manhattan_distance <- function(p, q) {
    return(abs(p[1] - q[1]) + abs(p[2] - q[2]))
}

# How many steps are required to carry the data from the square identified 
# in a puzzle input all the way to the access port?
how_many_steps <- function(field) {
    plane <- create_plane(field)
    start_coord <- which(plane == 1, TRUE)
    point_coord <- which(plane == field, TRUE)
    
    return(manhattan_distance(start_coord, point_coord))
}


for(test_input in test_inputs) {
    print(how_many_steps(test_input))
}

how_many_steps(puzzle_input)


# --- Part Two ---

adj_sum <- function(plane, x, y) {
    n <- nrow(plane)
    res <- 0
    if (x > 1) {
        res <- res + plane[x-1, y]
        if (y > 1) res <- res + plane[x-1, y-1]
        if (y < n) res <- res + plane[x-1, y+1]
    }
    if (x < n) {
        res <- res + plane[x+1, y]
        if (y < n) res <- res + plane[x+1, y+1]
        if (y > 1) res <- res + plane[x+1, y-1]
    }
    if (y > 1) res <- res + plane[x, y - 1]
    if (y < n) res <- res + plane[x, y + 1]

    return(res)
}

create_sum_plane <- function(field) {
        # Determine minimal n so that the field is contained in a n x n spiral matrix
        n <- 1
        while(!(field <= n^2)) n <- n + 2
        
        plane <- matrix(numeric(n^2), n, n, byrow = TRUE)
        centre <- median(seq_len(n))
        x <- y <- centre
        dir <- "R"
        xmx <- ymx <- cnt <- 1
        for(i in 1:n^2) {
            print(x)
            print(x)
            print(plane)
            if (x != centre & y != centre) plane[xmx, ymx] <- adj_sum(plane, xmx, ymx)
            else plane[x, y] <- i
            if(dir=="R") {if(xmx>0) {x=x+1;xmx=xmx-1}
                else {dir="U";ymx=cnt;y=y-1;ymx=ymx-1}; next}; 
            if(dir=="U") {if(ymx>0) {y=y-1;ymx=ymx-1}
                else {dir="L";cnt=cnt+1;xmx=cnt;x=x-1;xmx=xmx-1}; next}; 
            if(dir=="L") {if(xmx>0) {x=x-1;xmx=xmx-1} 
                else {dir="D";ymx=cnt;y=y+1;ymx=ymx-1}; next}; 
            if(dir=="D") {if(ymx>0) {y=y+1;ymx=ymx-1} 
                else {dir="R";cnt=cnt+1;xmx=cnt;x=x+1;xmx=xmx-1}; next}; 
        }
        
        return(plane)
}