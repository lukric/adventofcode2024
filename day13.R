# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 13
library(stringr)
library(dplyr)
library(tidyr)

# read data
con <- file("inputs/day13-1.txt")
d13 <- readLines(con)
close.connection(con)

chunk_size <- 4
machines <- split(d13, ceiling(seq_along(d13) / chunk_size))

# part 1
solve_machine <- function(z) {
  
  X <- stringr::str_split(z[c(1, 2)], ",") |> 
    unlist() |> 
    stringr::str_extract(pattern = "[0-9]+") |> 
    as.numeric() |> 
    matrix(, nrow = 2)
  
  y <- stringr::str_split(z[3], ",") |> 
    unlist() |> 
    stringr::str_extract(pattern = "[0-9]+") |> 
    as.numeric()
  
  if (det(X) == 0) {
    return(c(A = 0, B = 0))
  } 
  res <- solve(X, y)
  if (!identical(round(res), round(res, 10)))
    res <- c(NA, NA)
  names(res) <- c("A", "B")
  res
}

pushes <- lapply(machines, solve_machine) |> 
  dplyr::bind_rows()

# answer 1
sum(pushes$A, na.rm = TRUE) * 3 + sum(pushes$B, na.rm = TRUE) * 1

# part 2
solve_machine2 <- function(z) {
  
  X <- stringr::str_split(z[c(1, 2)], ",") |> 
    unlist() |> 
    stringr::str_extract(pattern = "[0-9]+") |> 
    as.numeric() |> 
    matrix(, nrow = 2)
  
  y <- stringr::str_split(z[3], ",") |> 
    unlist() |> 
    stringr::str_extract(pattern = "[0-9]+") |> 
    as.numeric() + 10000000000000
  
  if (det(X) == 0) {
    return(c(A = 0, B = 0))
  } 
  res <- solve(X, y)
  names(res) <- c("A", "B")
  res
}

pushes2 <- lapply(machines, solve_machine2) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(
    modA = round(A %% 1, 2),
    modB = round(B %% 1, 2)
  ) |> 
  dplyr::filter(
    trimws(modA) %in% c(0, 1),
    trimws(modB) %in% c(0, 1),
  )

# answer 2
ans2 <- sum(pushes2$A, na.rm = TRUE) * 3 + sum(pushes2$B, na.rm = TRUE) * 1

format(ans2, scientific = FALSE)
