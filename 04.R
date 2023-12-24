library(tidyverse)

data <- read_lines("04.csv")

n_match <- function(line) {
  parts <- strsplit(line, "\\|")[[1]]
  l <- parts[1]
  win_n <- strsplit(parts[2], " ")[[1]]
  parts2 <- strsplit(l, ":")[[1]]
  card <- parts2[1]
  have_n <- strsplit(parts2[2], " ")[[1]]
  length(intersect(win_n, have_n)) - 1 # rm "" match
}

n_matches <- sapply(data, \(x) x |> n_match())

names(n_matches) <- NULL

# Pt I
calc_pts <- function(n_match) {
  ifelse(n_match == 0, 0, 2^(n_match - 1))
}

calc_pts(n_matches) |> sum()

# Pt II
running_vec <- rep(1, times = length(n_matches))

for (i in seq_along(n_matches)) {
  pre_i <- rep(0, times = i)
  to_add <- rep(1 * running_vec[i], n_matches[i])
  pre <- c(pre_i, to_add)
  post <- rep(0, length(n_matches) - length(pre))
  running_vec <- running_vec + c(pre, post)
}
