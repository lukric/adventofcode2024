# Advent of Code 2024
# https://adventofcode.com/2024
# Lukas Richter

# Day 3
library(stringr)

# read data
df0 <- read.delim("inputs/day3-1.txt",
                  blank.lines.skip = FALSE,
                  header = FALSE)

# part 1
df1 <- stringr::str_extract_all(paste0(df0$V1, collapse = " "),
                                "mul\\([0-9]{1,3},[0-9]{1,3}\\)")

my_extraction <- function(x) {
  y <- stringr::str_extract_all(x, "[0-9]{1,3}") |> 
    unlist() |> 
    as.numeric()
  
  data.frame(num1 = y[1], num2 = y[2])
}

z <- lapply(df1[[1]], my_extraction) |> 
  dplyr::bind_rows()

# multiply and add
z <- z |> 
  dplyr::mutate(mult = num1 * num2)

# answer 1
sum(z$mult)

# part 2
df2 <- stringr::str_split(paste0(c(df0$V1, "do()"), collapse = " "),
                          "don't\\(\\).*?do\\(\\)") |> 
  unlist()

df3 <- stringr::str_extract_all(paste0(df2, collapse = " "),
                                "mul\\([0-9]{1,3},[0-9]{1,3}\\)")

v <- lapply(df3[[1]], my_extraction) |> 
  dplyr::bind_rows()

# multiply and add
v <- v |> 
  dplyr::mutate(mult = num1 * num2)

# answer 2
sum(v$mult)


