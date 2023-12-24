library(tidyverse)

d <- read_table("07.csv", col_names = FALSE)

rank_hand_type <- function(hand) {
  ns <- strsplit(hand, "")[[1]] |>
    table() |>
    sort(decreasing = TRUE)
  if (ns[1] == 5) {
    return(1)
  } else if (ns[1] == 4) {
    return(2)
  } else if (ns[1] == 3 && ns[2] == 2) {
    return(3)
  } else if (ns[1] == 3) {
    return(4)
  } else if (ns[1] == 2 && ns[2] == 2) {
    return(5)
  } else if (ns[1] == 2) {
    return(6)
  } else {
    7
  }
}

ranks <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")

rank_hand_pos <- function(hand, n) {
  which(ranks %in% strsplit(hand, "")[[1]][n])
}

out <- d |>
  rowwise() |>
  mutate(
    ht = rank_hand_type(X1),
    r1 = rank_hand_pos(X1, 1),
    r2 = rank_hand_pos(X1, 2),
    r3 = rank_hand_pos(X1, 3),
    r4 = rank_hand_pos(X1, 4),
    r5 = rank_hand_pos(X1, 5)
  ) |>
  arrange(ht, r1, r2, r3, r4, r5)

sum(out$X2 * (nrow(out):1))


ranks <- c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")

rank_hand_type2 <- function(hand) {
  ns <- strsplit(hand, "")[[1]] |>
    table() |>
    sort(decreasing = TRUE)
  if (ns[1] == 5) {
    return(1)
  } else if (ns[1] == 4) {
    if (any(names(ns) == "J")) return(1)
    return(2)
  } else if (ns[1] == 3 && ns[2] == 2) {
    if (any(names(ns) == "J")) return(1)
    return(3)
  } else if (ns[1] == 3) {
    if (any(names(ns) == "J")) return(2)
    return(4)
  } else if (ns[1] == 2 && ns[2] == 2) {
    if (names(ns[1]) == "J") return(2)
    if (names(ns[2]) == "J") return(2)
    if (names(ns[3]) == "J") return(3)
    return(5)
  } else if (ns[1] == 2) {
    if (any(names(ns) == "J")) return(4)
    return(6)
  } else {
    if (any(names(ns) == "J")) return(6)
    7
  }
}

out <- d |>
  rowwise() |>
  mutate(
    ht = rank_hand_type2(X1),
    r1 = rank_hand_pos(X1, 1),
    r2 = rank_hand_pos(X1, 2),
    r3 = rank_hand_pos(X1, 3),
    r4 = rank_hand_pos(X1, 4),
    r5 = rank_hand_pos(X1, 5)
  ) |>
  arrange(ht, r1, r2, r3, r4, r5)

sum(out$X2 * (nrow(out):1))
