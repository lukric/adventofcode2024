# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 5
library(stringr)
library(dplyr)

# read data
df_rules <- read.delim("inputs/day5-2.txt",
                       blank.lines.skip = FALSE, sep = "|",
                       header = FALSE)

df0 <- read.delim("inputs/day5-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

# part 1

get_rules <- function(x, rules = df_rules) {
  df_sub <- df_rules |> 
    dplyr::mutate(V1_in = V1 %in% x,
                  V2_in = V2 %in% x,
                  include = V1_in & V2_in) |> 
    dplyr::filter(include == TRUE)
}

check_protocols <- function(x) {
  # helper
  check_rule <- function(x, rule) {
    which(x %in% rule[1]) < which(x %in% rule[2])
  }
  
  y <- stringr::str_split(x, ",") |> 
    unlist() |> 
    as.numeric()
  
  this_rules <- get_rules(y, df_rules)
  
  checked_rules <- apply(this_rules, 1, check_rule, x = y)
  
  if (sum(!checked_rules) == 0)
    return(y[(length(y) + 1) / 2])
  
  # else
  0
}

middle_pages <- sapply(df0$V1, check_protocols)
names(middle_pages) <- NULL

# answer 1
sum(middle_pages)


# part 2

correct_protocols <- function(x) {
  
  y <- stringr::str_split(x, ",") |> 
    unlist() |> 
    as.numeric()
  
  this_rules <- get_rules(y, df_rules)
  
  new_protocol <- rep(0, length(y))
  
  y2 <- y
  this_rules2 <- this_rules
  for (i in 1:(length(y) - 1)) {
    n <- y2[which(!y2 %in% this_rules2$V2)]
    new_protocol[i] <- n
    
    y2 <- y2[y2 != n]
    this_rules2 <- this_rules2 |> 
      dplyr::filter(V1 != n)
    i <- i + 1
  }
  new_protocol[i] <- y2
  
  return(new_protocol[(length(new_protocol) + 1) / 2])
}

# incorrect protocols
df1 <- df0[middle_pages == 0, ]

middle_pages2 <- sapply(df1, correct_protocols)
names(middle_pages2) <- NULL

# answer 1
sum(middle_pages2)


