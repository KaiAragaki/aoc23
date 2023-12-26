library(tidyverse)

d <- read_lines("09.csv")

parse_line <- function(line) {
  strsplit(line, " ")[[1]] |> as.numeric()
}

new_n <- function(line) {
  n <- 0
  while(sum(line) != 0) {
    n <- line[length(line)] + n
    line <- diff(line)
  }
  n
}

sums <- sapply(d, \(x) x |> parse_line() |> new_n()) |> sum()

new_n2 <- function(line) {
  firsts <- c()
  while (sum(line != 0)) {
    firsts <- c(firsts, line[1])
    line <- diff(line)
  }
  Reduce(`-`, firsts, right = TRUE)
}

sums <- sapply(d, \(x) x |> parse_line() |> new_n2()) |> sum()
