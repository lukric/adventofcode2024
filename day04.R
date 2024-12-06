# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 4
library(stringr)

# read data
df0 <- read.delim("inputs/day4-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

n_count <- nchar(df0[1, 1])
ws <- rep(1, times = n_count)
names(ws) <- 1:n_count

df1 <- df0 |> 
  tidyr::separate_wider_position(`V1`, widths = ws) |> 
  as.matrix()

# part 1
# idea: search all rows, all columns, all main diagonals and all reverse diagonals for words "XMAS" and "SAMX"

my_search <- function(x) {
  y <- paste0(x, collapse = "")
  
  stringr::str_count(y, pattern = "XMAS") +
    stringr::str_count(y, pattern = "SAMX")
  
}


n_vertical <- apply(df1, 1, my_search)

n_horizontal <- apply(df1, 2, my_search)

# search all diagonals
# main
d <- row(df1) - col(df1)

df2 <- split(df1, d)

n_main_diag <- sapply(df2, my_search)

# revers diagonals
d2 <- row(df1) + col(df1)

df3 <- split(df1, d2)

n_reverse_diag <- sapply(df3, my_search)

# answer 1
sum(n_vertical, n_horizontal, n_main_diag, n_reverse_diag)

# part 2
# idea: locate all "A"s and check they have according "M"s and "A"s around them

posA <- which(df1 == "A", arr.ind = TRUE)

check_xmas <- function(x) {
  # helper fun
  get_char <- function(x, m) {
    m[x[1], x[2]]
  }
  
  if (any(x == 1) | any(x == 140))
    return(FALSE)
  
  pos1 <- x - c(1, 1)
  pos2 <- x - c(1, -1)
  pos3 <- x - c(-1, 1)
  pos4 <- x - c(-1, -1)
  
  df1_ <- rbind(pos1, pos2, pos3, pos4)
  
  df2_ <- apply(df1_, 1, get_char, m = df1)
  
  if (sum(df2_ == "S") != 2 | sum(df2_ == "M") != 2)
    return(FALSE)
  
  if (df2_[1] == df2_[4] | df2_[2] == df2_[3])
    return(FALSE)
  
  TRUE
}


checkAs <- apply(posA, 1, check_xmas)

# answer 2
sum(checkAs)
