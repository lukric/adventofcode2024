# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 1

library(dplyr)

# read data
df0 <- read.delim("inputs/day1-1.txt",
                  blank.lines.skip = FALSE, sep = "",
                  header = FALSE)

# part 1
# quick and dirty
x <- sort(df0[, 1])
y <- sort(df0[, 2])

df1 <- data.frame(x = x, y = y) |> 
  dplyr::mutate(diff = abs(x - y))

# answer 1
sum(df1$diff)

# part 2
my_count <- function(x, y) {
  sum(x == y)
}

counts <- sapply(df1$x, my_count, y = y)

df1$count <- counts

df1 <- df1 |> 
  dplyr::mutate(similarity = x * count)

# answer 2
sum(df1$similarity)
