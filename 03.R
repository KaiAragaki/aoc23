library(tidyverse)

data <- read_lines("03.csv")

ncol <- nchar(data[1])

tib <- as_tibble(data) |>
  separate(everything(), into = paste0("col", seq_len(ncol + 1)), "") |>
  select(-col1)
colnames(tib) <- paste0("col", seq_len(ncol(tib)))

# Tacking on some dummy borders to avoid edge effects
tib <- cbind(l = ".", tib, r = ".")
tib <- rbind(".", tib, ".")

# Part I
sum <- 0
running_num <- "0"
symbol_near <- FALSE
for (i in 2:(nrow(tib) - 1)) {
  for (j in 2:(ncol(tib) - 1)) {
    if (!str_detect(tib[i, j], "[0-9]")) {
      sum <- as.numeric(running_num) * symbol_near + sum
      running_num <- "0"
      symbol_near <- FALSE
    } else {
      running_num <- paste0(running_num, tib[i, j])
      if (!symbol_near) {
        symbol_near <-
          get_surrounding(tib, i, j) |>
          unlist() |>
          str_detect("[^\\.[0-9]]") |>
          any()
      }
    }
  }
}
sum

# Part II

sum <- 0
for (i in 2:(nrow(tib) - 1)) {
  for (j in 2:(ncol(tib) - 1)) {
    if (tib[i, j] == "*" && n_n_at_pt(tib, i, j) == 2) {
      locs <- get_n_locs_around_pt(tib, i, j)
      sum <- sum + expand_n_at_pt(tib, locs[1, 1], locs[1, 2]) *
        expand_n_at_pt(tib, locs[2, 1], locs[2, 2])
    }
  }
}

get_surrounding <- function(tib, i, j) {
  tib[(i - 1):(i + 1), (j - 1):(j + 1)]
}

n_n_at_pt <- function(tib, i, j) {
  get_surrounding(tib, i, j) |>
    apply(1, \(x) paste0(x, collapse = "")) |>
    str_count("[0-9]+") |>
    sum()
}

get_n_locs_around_pt <- function(tib, i, j) {
  get_surrounding(tib, i, j) |>
    unite("data", everything(), sep = "") |>
    unlist() |>
    str_locate_all("[0-9]+") |>
    lapply(as.data.frame) |>
    bind_rows(.id = "row") |>
    mutate(
      row = as.numeric(row) + i - 2,
      start = start + j - 2
    )
}

expand_n_at_pt <- function(tib, i, j) {

  if (!str_detect(tib[i, j], "[0-9]")) {
    return(1)
  }

  row <- tib[i, ]
  n <- row[j]
  mark <- j - 1
  while(str_detect(row[mark], "[0-9]")) {
    n <- paste0(row[mark], n)
    mark <- mark - 1
  }
  mark <- j + 1
  while(str_detect(row[mark], "[0-9]")) {
    n <- paste0(n, row[mark])
    mark <- mark + 1
  }
  as.numeric(n)
}
