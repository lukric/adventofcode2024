# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 7
library(stringr)
library(gtools)
library(parallel)

# read data
df0 <- read.delim("inputs/day7-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

# part 1
my_add <- function(x) {
  if (length(x) == 1)
    return(x)
  y <- x[1] + x[2]
  c(y, x[-c(1, 2)])
}

my_mult <- function(x) {
  if (length(x) == 1)
    return(x)
  y <- x[1] * x[2]
  c(y, x[-c(1, 2)])
}

check_calcs <- function(x) {
  y <- stringr::str_split(x, "\\:?\\s") |> 
    unlist() |> 
    as.numeric()
  
  lhs <- y[1]
  rhs <- y[-1]
  
  all_permut <- gtools::permutations(2, length(rhs) - 1, c("my_add", "my_mult"),
                                     repeats.allowed = TRUE)
  
  do_calcs <- function(this_permut) {
    z <- rhs
    for (i in seq_along(this_permut)) {
      z <- do.call(this_permut[i], list("x" = z))
    }
    z
  }
  
  v <- apply(all_permut, 1, do_calcs)
  
  if (any(v == lhs))
    return(lhs)
  0
}

all_checks <- apply(df0, 1, check_calcs)

# answer 1
format(sum(all_checks), scientific = FALSE)

# part 2
# kind of brute force solution

check_calcs2 <- function(x) {
  
  my_concat <- function(x) {
    if (length(x) == 1)
      return(x)
    if (is.infinite(x[1])) {
      y <- Inf
    } else {
      y <- as.numeric(paste0(x[1], "", x[2]))
    }
    c(y, x[-c(1, 2)])
  }
  
  my_add <- function(x) {
    if (length(x) == 1)
      return(x)
    y <- x[1] + x[2]
    c(y, x[-c(1, 2)])
  }
  
  my_mult <- function(x) {
    if (length(x) == 1)
      return(x)
    y <- x[1] * x[2]
    c(y, x[-c(1, 2)])
  }
  
  y <- stringr::str_split(x, "\\:?\\s") |> 
    unlist() |> 
    as.numeric()
  
  lhs <- y[1]
  rhs <- y[-1]
  
  all_permut <- gtools::permutations(
    3,
    length(rhs) - 1, c("my_add", "my_mult", "my_concat"),
    repeats.allowed = TRUE)
  
  do_calcs <- function(this_permut) {
    z <- rhs
    for (i in seq_along(this_permut)) {
      z <- do.call(this_permut[i], list("x" = z))
    }
    z
  }
  
  v <- apply(all_permut, 1, do_calcs)
  
  if (any(v == lhs))
    return(lhs)
  0
}

# calculate using parallel
num_cores <- parallel::detectCores()
cl <- parallel::makeCluster(num_cores)

a <- Sys.time()
all_checks2 <- parallel::parApply(cl, df0, 1, check_calcs2)
b <- Sys.time()
b - a

# answer 1
format(sum(all_checks2), scientific = FALSE)
