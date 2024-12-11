# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 10
library(tidyr)
library(dplyr)

# read data
df0 <- read.delim("inputs/day10-1.txt",
                  blank.lines.skip = FALSE,
                  colClasses = "character",
                  header = FALSE)

n_count <- nchar(df0[1, 1])
ws <- rep(1, times = n_count)
names(ws) <- 1:n_count

df1 <- df0 |> 
  tidyr::separate_wider_position(`V1`, widths = ws) |> 
  as.matrix() |> 
  as.numeric() |> 
  matrix(ncol = n_count)

# part 1
# x should have a number and a position
# x <- list(value = 0, row = 7, col = 7)

count_nines <- function(x) {
  find_nines <- function(x) {
    
    check_num <- function(value, x, y) {
      n <- df1[x, y]
      if (n == value + 1)
        return(list(value = n, row = x, col = y))
      NULL
    }
    
    if (x$value == 9)
      return(x)
    
    row <- x$row  
    col <- x$col
    value <- x$value
    
    y <- list()
    
    if (row > 1) {
      y[[1]] <- check_num(value, row - 1, col)
    }
    if (row < nrow(df1)) {
      y[[2]] <- check_num(value, row + 1, col)
    }
    if (col > 1) {
      y[[3]] <- check_num(value, row, col - 1)
    }
    if (col < ncol(df1)) {
      y[[4]] <- check_num(value, row, col + 1)
    }
    y <- y[!sapply(y, is.null)]
    if (length(y) == 0)
      return(NULL)
    
    lapply(y, find_nines)
    
  }
  
  find_nines(x) |> 
    unlist() |> 
    matrix(ncol = 3, byrow = TRUE) |> 
    as.data.frame() |> 
    dplyr::distinct() |> 
    nrow()
}

zeros <- which(df1 == 0, arr.ind = TRUE) |> 
  as.data.frame()
zeros$value <- 0

my_nines <- sapply(1:nrow(zeros), function(x) {
  count_nines(list(value = zeros[x, "value"], 
                   row = zeros[x, "row"],
                   col = zeros[x, "col"]))
  })

# answer 1
sum(my_nines)

# part 2

count_nines2 <- function(x) {
  find_nines <- function(x) {
    
    check_num <- function(value, x, y) {
      n <- df1[x, y]
      if (n == value + 1)
        return(list(value = n, row = x, col = y))
      NULL
    }
    
    if (x$value == 9)
      return(x)
    
    row <- x$row  
    col <- x$col
    value <- x$value
    
    y <- list()
    
    if (row > 1) {
      y[[1]] <- check_num(value, row - 1, col)
    }
    if (row < nrow(df1)) {
      y[[2]] <- check_num(value, row + 1, col)
    }
    if (col > 1) {
      y[[3]] <- check_num(value, row, col - 1)
    }
    if (col < ncol(df1)) {
      y[[4]] <- check_num(value, row, col + 1)
    }
    y <- y[!sapply(y, is.null)]
    if (length(y) == 0)
      return(NULL)
    
    lapply(y, find_nines)
    
  }
  
  find_nines(x) |> 
    unlist() |> 
    matrix(ncol = 3, byrow = TRUE) |> 
    as.data.frame() |> 
    # dplyr::distinct() |> 
    nrow()
}

zeros <- which(df1 == 0, arr.ind = TRUE) |> 
  as.data.frame()
zeros$value <- 0

my_nines2 <- sapply(1:nrow(zeros), function(x) {
  count_nines2(list(value = zeros[x, "value"], 
                    row = zeros[x, "row"], 
                    col = zeros[x, "col"]))
  })

# answer 2
sum(my_nines2)
