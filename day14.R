# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 14
library(stringr)
library(dplyr)
library(plot.matrix)

# read data
con <- file("inputs/day14-1.txt")
robots <- readLines(con)
close.connection(con)

xmax <- 101
ymax <- 103

# part 1
v1 <- stringr::str_replace(robots, " v=", ",") |>
  stringr::str_replace("p=", "") |> 
  stringr::str_split(",")

df1 <- lapply(v1,
             function(x) {
               x <- as.numeric(x)
               names(x) <- c("x", "y", "vx", "vy")
               x
             }) |> 
  dplyr::bind_rows()

n_sec <- 100
df2 <- df1 |> 
  dplyr::mutate(x_end = (x + n_sec * vx) %% xmax,
                y_end = (y + n_sec * vy) %% ymax) |> 
  dplyr::mutate(quadrant = dplyr::case_when(
    # q1
    x_end < (xmax - 1) / 2 & y_end < (ymax - 1) / 2 ~ 1,
    # q2
    x_end > (xmax - 1) / 2 & y_end < (ymax - 1) / 2 ~ 2,
    # q3
    x_end < (xmax - 1) / 2 & y_end > (ymax - 1) / 2 ~ 3,
    # q4
    x_end > (xmax - 1) / 2 & y_end > (ymax - 1) / 2 ~ 4
  )) |> 
  dplyr::filter(!is.na(quadrant)) |> 
  dplyr::group_by(quadrant) |> 
  dplyr::summarise(n = dplyr::n())

# answer 1
prod(df2$n)

# part 2

plot_robots <- function(n_sec = 1, df1, xmax, ymax) {
  
  df2 <- df1 |> 
    dplyr::mutate(x_end = (x + n_sec * vx) %% xmax,
                  y_end = (y + n_sec * vy) %% ymax)
  
  m <- matrix(0, nrow = ymax, ncol = xmax)
  for (i in 1:nrow(df2)) {
    xcoord <-  df2 |> 
      dplyr::filter(dplyr::row_number() == i) |> dplyr::pull(x_end) + 1
    ycoord <-  df2 |> 
      dplyr::filter(dplyr::row_number() == i) |> dplyr::pull(y_end) + 1
    
    m[ycoord, xcoord] <- 1
  }
  m
}

library(parallel)
num_cores <- parallel::detectCores()
cl <- parallel::makeCluster(20)
parallel::clusterExport(cl, varlist = c("df1", "xmax", "ymax"))

raw_m <- parallel::parLapply(cl, 1:10000, plot_robots, df1 = df1, xmax = xmax, ymax = ymax)


for (i in seq_along(raw_m)) {
  n_sec <- i
  png(paste0("aoc2024_outputs/robots_", n_sec, ".png"))
  plot(raw_m[[i]], key = NULL, col = c("black", "white"))
  dev.off()
}

# answer 2
# 7916
