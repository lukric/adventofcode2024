# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 9
library(stringr)
library(dplyr)
library(tidyr)

# read data
con <- file("inputs/day9-1.txt")
diskmap <- readLines(con)
close.connection(con)


# part 1
dm_digits <- stringr::str_extract_all(diskmap, "[0-9]{1}") |> 
  unlist() |> 
  as.numeric()

dm <- data.frame(digits = dm_digits) |> 
  dplyr::mutate(id = dplyr::row_number() / 2 - 0.5,
                id = dplyr::if_else(id == round(id), as.character(id), "."))

dm2 <- dm |> 
  tidyr::uncount(digits) |> 
  dplyr::rename("block" = "id") |> 
  dplyr::mutate(id = dplyr::row_number() - 1,
                cum_dots = cumsum(block == "."),
                cum_nums = rev(cumsum(rev(block != "."))))

# calculate how many to cut
n_cut <- max(dm2$cum_nums[which(dm2$cum_nums < dm2$cum_dots)]) + 1

dm2 <- dm2 |> 
  dplyr::select(-cum_dots, -cum_nums)

# non-empty blocks
dm_no_empty <- dm2 |> 
  dplyr::filter(block != ".")

# empty blocks which will be filled
dm_moved <- dm2 |> 
  dplyr::filter(block == ".") |> 
  dplyr::slice_head(n = n_cut) |> 
  dplyr::select(-block)

# blocks which will be moved to empty blocks
dm_move <- dm_no_empty |> 
  dplyr::slice_tail(n = n_cut)

dm_moved$block <- rev(dm_move$block)

# moved blocks
dm_moved <- dm_moved |> 
  dplyr::mutate(block = as.numeric(block))

# blocks which stay
dm_original <- dm_no_empty |> 
  dplyr::slice_head(n = -n_cut) |> 
  dplyr::mutate(block = as.numeric(block))

# answer 1
ans <- sum(dm_original$block * dm_original$id) + sum(dm_moved$block * dm_moved$id)
format(ans, scientific = FALSE)

# part 2

# initialise
x <- dm

id_vec <- rev(x$id)
id_vec <- id_vec[id_vec != "."]

for (my_id in id_vec) {
  y <- x |> 
    dplyr::filter(id == my_id)
  
  z <- x[1:which(x$id == my_id), ]
  
  my_spots <- which(z$digits >= y$digits & z$id == ".")
  
  # do stuff if we can insert y
  if (length(my_spots) > 0) {
    insert_y <- min(my_spots)
    # unchanged rows
    df_unchanged <- x[1:(insert_y - 1), ]
    # rows to insert
    df_insert <- y
    
    non_used_digits <- x[insert_y, "digits"] - y$digits
    if (non_used_digits > 0) {
      df_insert <- df_insert |> 
        dplyr::add_row("digits" = non_used_digits, id = ".")
    }
    
    # remaining rows
    # remove last row, if it was removed
    df_remain <- x[(insert_y + 1):nrow(x), ] |> 
      dplyr::mutate(id = dplyr::if_else(id == my_id, ".", id))
    
    x <- dplyr::bind_rows(list(df_unchanged, df_insert, df_remain))
  }
  
}

x2 <- x |> 
  tidyr::uncount(digits) |> 
  dplyr::rename("block" = "id") |> 
  dplyr::mutate(id = dplyr::row_number() - 1) |> 
  dplyr::filter(block != ".") |> 
  dplyr::mutate(block = as.numeric(block))

# answer 2
ans2 <- sum(x2$block * x2$id)
format(ans2, scientific = FALSE)








