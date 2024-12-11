# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 11
library(stringr)

# read data
con <- file("inputs/day11-1.txt")
stones0 <- readLines(con)
close.connection(con)

stones1 <- stringr::str_split(stones0, "\\s") |> 
  unlist() |> 
  as.numeric()

# vec1 <- c(125, 17) # example

# part 1

apply_rules <- function(x) {
  if (x == 0)
    return(1)
  if (nchar(x) %% 2 == 0) {
    y1 <- stringr::str_sub(x, start = 1L, end = nchar(x) / 2) |> 
      as.numeric()
    y2 <- stringr::str_sub(x, start = nchar(x) / 2 + 1, end = -1L) |> 
      as.numeric()
    return(c(y1, y2))
  }
  x * 2024
}

vec2 <- vec1
for (i in 1:25) {
vec2 <- lapply(vec2, apply_rules) |> 
  unlist()
  i <- i + 1
}

# answer 1
length(vec2)

