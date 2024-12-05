# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 2

# read data
df0 <- read.delim("inputs/day2-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

# part 1
check_fun <- function(x) {
  y <- strsplit(x, " ")|> 
    unlist() |> 
    as.numeric() |> 
    diff()
  
  if (any(y == 0) | any(abs(y) > 3)) # do not differ between 1 and 3
    return(FALSE)
  if (length(unique(sign(y))) != 1) # increasing and decreasing
    return(FALSE)
  # safe
  TRUE
}

checks <- sapply(df0$V1, check_fun)

# answer1
sum(checks)

# part 2

check_fun2 <- function(x) {
  
  internal_check <- function(z, remove = 0) {
    if (remove > 0) {
      z1 <- z[-remove]
    } else {
      z1 <- z
    }
  
    y <- z1 |> 
      diff()
    
    if (remove > length(z))
      return(FALSE)
    
    if (any(y == 0) | any(abs(y) > 3) | length(unique(sign(y))) != 1) { 
      return(internal_check(z, remove + 1))
    }
    
    # safe
    TRUE
  }
  
  z <- strsplit(x, " ")|> 
    unlist() |> 
    as.numeric()
  
  internal_check(z, 0)
}


checks2 <- sapply(df0$V1, check_fun2)


# answer2
sum(checks2)
