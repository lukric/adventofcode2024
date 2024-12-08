# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 8
library(stringr)
library(tidyr)
library(dplyr)

# read data
df0 <- read.delim("inputs/day8-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

n_count <- nchar(df0[1, 1])
ws <- rep(1, times = n_count)
names(ws) <- 1:n_count

df1 <- df0 |> 
  tidyr::separate_wider_position(`V1`, widths = ws) |> 
  as.matrix()

# part 1

get_antinodes <- function(frq) {
  positions <- which(df1 == frq, arr.ind = TRUE)
  
  # all position combinations
  k <- 2
  combinations <- combn(x = 1:nrow(positions),
                        m = k)
  # list of matrices of all position combinations
  poss_combis <- lapply(1:ncol(combinations), function(x) positions[combinations[, x], ])
  
  calc_antinodes <- function(positions) {
    pos1 <- positions[1, ]
    pos2 <- positions[2, ]
    dist <- pos1 - pos2
    rbind(pos1 + dist, pos2 - dist) |> 
      as.data.frame()
  }
  
  lapply(poss_combis, calc_antinodes) |> 
    dplyr::bind_rows()
  
}

frequencies <- unique(as.vector(df1))
frequencies <- frequencies[frequencies != "."]

tt <- lapply(frequencies, get_antinodes)

# answer 1
tt |> 
  dplyr::bind_rows() |> 
  dplyr::distinct() |> 
  dplyr::filter(row > 0,
                row <= n_count,
                col > 0,
                col <= n_count) |> 
  nrow()

# part 2

get_antinodes2 <- function(frq) {
  positions <- which(df1 == frq, arr.ind = TRUE)
  
  # all position combinations
  k <- 2
  combinations <- combn(x = 1:nrow(positions),
                        m = k)
  # list of matrices of all position combinations
  poss_combis <- lapply(1:ncol(combinations), function(x) positions[combinations[, x], ])
  
  calc_antinodes <- function(positions) {
    pos1 <- positions[1, ]
    pos2 <- positions[2, ]
    dist <- rbind(pos1 - pos2, pos2 - pos1)
    
    lapply(0:n_count, function(x) as.data.frame(positions + x * dist)) |> 
      dplyr::bind_rows()
  }
  
  lapply(poss_combis, calc_antinodes) |> 
    dplyr::bind_rows()
  
}

frequencies <- unique(as.vector(df1))
frequencies <- frequencies[frequencies != "."]

tt <- lapply(frequencies, get_antinodes2)

# answer 2
tt |> 
  dplyr::bind_rows() |> 
  dplyr::distinct() |> 
  dplyr::filter(row > 0,
                row <= n_count,
                col > 0,
                col <= n_count) |> 
  nrow()

