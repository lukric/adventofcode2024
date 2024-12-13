# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 12
library(tidyr)

# read data
df0 <- read.delim("inputs/day12-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

n_count <- nchar(df0[1, 1])
ws <- rep(1, times = n_count)
names(ws) <- 1:n_count

df1 <- df0 |> 
  tidyr::separate_wider_position(`V1`, widths = ws) |> 
  as.matrix()

# part 1

new_df1 <- df1
price <- 0

calc_price <- function(row, col, new_df1, price = 0) {
  print(paste0("row: ", row, " col: ", col, " ", df1[row, col]))
  
  calc_peri <- function(this_row, this_col, df) {
    
    perimeter <- 0
    # row, col + 1
    if (this_col == ncol(new_df1)) {
      perimeter <- perimeter + 1
    } else {
      if (nrow(df |> dplyr::filter(row == this_row, col == this_col + 1)) == 0)
        perimeter <- perimeter + 1
    }
    
    # row, col - 1
    if (this_col == 1) {
      perimeter <- perimeter + 1
    } else {
      if (nrow(df |> dplyr::filter(row == this_row, col == this_col - 1)) == 0)
        perimeter <- perimeter + 1
    }
    
    # row - 1, col
    if (this_row == 1) {
      perimeter <- perimeter + 1
    } else {
      if (nrow(df |> dplyr::filter(row == this_row - 1, col == this_col)) == 0)
        perimeter <- perimeter + 1
    }
    
    # row + 1, col
    if (this_row == nrow(new_df1)) {
      perimeter <- perimeter + 1
    } else {
      if (nrow(df |> dplyr::filter(row == this_row + 1, col == this_col)) == 0)
        perimeter <- perimeter + 1
    }
    perimeter
    
  }
  
  cut_plants <- function(this_row, this_col, m0 = data.frame(row = numeric(), col = numeric())) {
    # print(paste0("cut_plants: ", this_row, " ", this_col))
    val <- new_df1[this_row, this_col]
    
    m <- m0
    # row, col + 1
    if (this_col != ncol(new_df1) && val == new_df1[this_row, this_col + 1])
      m <- m |> dplyr::add_row(row = this_row, col = this_col + 1)
    
    # row, col - 1
    if (this_col != 1 && val == new_df1[this_row, this_col - 1])
      m <- m |> dplyr::add_row(row = this_row, col = this_col - 1)
    
    # row - 1, col
    if (this_row != 1 && val == new_df1[this_row - 1, this_col])
      m <- m |> dplyr::add_row(row = this_row - 1, col = this_col)
    
    # row + 1, col
    if (this_row != nrow(new_df1) && val == new_df1[this_row + 1, this_col])
      m <- m |> dplyr::add_row(row = this_row + 1, col = this_col)
    
    m <- m |> 
      dplyr::distinct(row, col, .keep_all = TRUE) |> 
      dplyr::arrange(row, col) |> 
      dplyr::mutate(already_checked = dplyr::if_else(row == this_row & col == this_col, 1, already_checked))
    
    m_check <- m |> 
      dplyr::filter(!already_checked %in% 1)
    
    if (nrow(m_check) == 0) {
      return(m)
    }
    
    m2 <- cut_plants(m_check[1, 1], m_check[1, 2], m) |> 
      dplyr::distinct(row, col, .keep_all = TRUE) |> 
      dplyr::arrange(row, col)
    
    m2
    
  }
  dfs <- cut_plants(row, col, m0 = data.frame(row = row, col = col, already_checked = 0))
  peris <- sapply(1:nrow(dfs), function(x) calc_peri(dfs[x, 1], dfs[x, 2], dfs))
  price <- price + nrow(dfs) * sum(peris)
  
  
  for(i in 1:nrow(dfs)) {
    new_df1[dfs[i, 1], dfs[i, 2]] <- "."
  }
  
  y <- which(new_df1 != ".", arr.ind = TRUE) |> 
    head(1)
  
  if (nrow(y) == 0)
    return(price)
  
  price <- calc_price(y[1], y[2], new_df1, price)
}

# answer 1
(my_price <- calc_price(1, 1, new_df1))




