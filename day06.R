# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 6
library(stringr)
library(tidyr)
library(dplyr)
library(parallel)

# read data
df0 <- read.delim("inputs/day6-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

n_count <- nchar(df0[1, 1])
ws <- rep(1, times = n_count)
names(ws) <- 1:n_count

df1 <- df0 |> 
  tidyr::separate_wider_position(`V1`, widths = ws) |> 
  as.matrix()

# part 1

# check if X is next in direction
# if yes: change_direction
# if no: current_location + direction

change_direction <- function(old) {
  if (identical(old, c(-1, 0)))
    return(c(0, 1))
  if (identical(old, c(0, 1)))
    return(c(1, 0))
  if (identical(old, c(1, 0)))
    return(c(0, -1))
  if (identical(old, c(0, -1)))
    return(c(-1, 0))
  warning("unexpected input (old direction)")
  NA
}

# returns TRUE if an obstruction is in the given direction
# returns FALSE otherwise
check_obstruction <- function(position, direction, obstructions) {
  check_position <- position + direction
  paste0(check_position[1], " ", check_position[2]) %in% obstructions$check
}

check_inside <- function(position) {
  if (any(position < 1) | any(position > n_count))
    return(FALSE)
  TRUE
}

guard <- which(df1 == "^", arr.ind = TRUE) |> 
  as.numeric()
direction <- c(-1, 0)
obstructions <- which(df1 == "#", arr.ind = TRUE) |> 
  as.data.frame() |> 
  dplyr::mutate(check = paste0(row, " ", col))

position <- guard

guard_info <- data.frame("row" = numeric(0), "col" = numeric(0), "dir1" = numeric(0), "dir2" = numeric(0))

while (check_inside(position)) {
  
  guard_info <- guard_info |> 
    dplyr::add_row(row = position[1],
                   col = position[2],
                   dir1 = direction[1],
                   dir2 = direction[2])
  
  co <- check_obstruction(position, direction, obstructions)
  
  if (co) {
    # new direction
    direction <- change_direction(direction)
  } else {
    # new location
    position <- position + direction
  }
  
}

# answer 1

guard_info |> 
  dplyr::distinct(row, col) |> 
  nrow()

# part 2
# be careful with the code below. it might be quite slow as we need to calculate
# about 5000 new paths, which is not very fast by itself... 

possible_obstacles <- guard_info |> 
  dplyr::distinct(row, col, .keep_all = TRUE)


is_loop <- function(guard_info) {
  
  # functions
  check_loop <- function(position, h) {
    h <- h |> 
      dplyr::filter(row == position[1])
    if (sum(duplicated(h)) > 0)
      return(TRUE)
    FALSE
  }
  
  check_obstruction <- function(position, direction, obstructions) {
    check_position <- position + direction
    paste0(check_position[1], " ", check_position[2]) %in% obstructions$check
  }
  
  check_inside <- function(position) {
    if (any(position < 1) | any(position > n_count))
      return(FALSE)
    TRUE
  }
  
  change_direction <- function(old) {
    if (identical(old, c(-1, 0)))
      return(c(0, 1))
    if (identical(old, c(0, 1)))
      return(c(1, 0))
    if (identical(old, c(1, 0)))
      return(c(0, -1))
    if (identical(old, c(0, -1)))
      return(c(-1, 0))
    warning("unexpected input (old direction)")
    NA
  }
  
  # initialise guard
  obstacle_add <- guard_info |> 
    dplyr::slice_tail(n = 1)
  
  obstructions1 <- obstructions |> 
    dplyr::add_row(obstacle_add |> 
                     dplyr::select(row, col) |> 
                     dplyr::mutate(check = paste0(row, " ", col)))
  
  guard_info2 <- guard_info |> 
    dplyr::slice_head(n = -1)
  
  position <- c(tail(guard_info2$row, 1), tail(guard_info2$col, 1))
  direction <- c(tail(guard_info2$dir1, 1), tail(guard_info2$dir2, 1))
  
  while (check_inside(position)) {
    
    if (check_loop(position, guard_info2)) {
      return (TRUE)
      break
    }
    
    co <- check_obstruction(position, direction, obstructions1)
    
    if (co) {
      # new direction
      direction <- change_direction(direction)
    } else {
      # new location
      position <- position + direction
    }
    
    guard_info2 <- guard_info2 |> 
      dplyr::add_row(row = position[1],
                     col = position[2],
                     dir1 = direction[1],
                     dir2 = direction[2])
  }
  FALSE
}

# parallelisation
n_obstacles <- 2:nrow(possible_obstacles)

cut_obstacles <- function(i, possible_obstacles) {
  possible_obstacles |> 
    dplyr::slice_head(n = i)
}

possible_obstacles0_list <- lapply(n_obstacles, cut_obstacles, possible_obstacles = possible_obstacles)

num_cores <- parallel::detectCores()
cl <- parallel::makeCluster(num_cores)

parallel::clusterExport(cl, varlist = c("guard", "obstructions", "n_count"))

a <- Sys.time()
checked_loops_par <- parSapply(cl, possible_obstacles0_list, is_loop)
b <- Sys.time()
b - a

# answer 2
sum(checked_loops_par)



