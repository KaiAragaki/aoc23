library(tidyverse)

d <- read_table("06.csv", col_names = FALSE) |> t() |> as_tibble()
d <- d[-1,]
d <- type_convert(d)
colnames(d) <- c("time", "dist")

d <- d |>
  rowwise() |>
  mutate(n_wins = sum((seq(0, time) * seq(time, 0)) > dist))

max <- 35696887
goal <- 213116810861248

bin_search <- function(low, high, f) {
  mid <- low + ((high - low) / 2)
  mid <- floor(mid)
  if (mid == low) {
    return(mid)
  }

  res <- (max - mid) * mid

  if (f(res, goal)) {
    high <- mid
  } else {
    low <- mid
  }

  bin_search(low, high, f)
}

lbound <- bin_search(0, max, \(x, y)  x > y)

rbound <- bin_search(0, max, \(x, y) x < y)

rbound - lbound
