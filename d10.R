library(tidyverse)

d <- read_lines("10.csv")

m <- d |>
  as_tibble() |>
  separate(value, into = as.character(seq_len(nchar(d[1]) + 1)), sep = "") |>
  as.matrix()
m <- m[, -1]
colnames(m) <- NULL

start <- which(m == "S") + 1 # Get it started by 1 so it's not on S

space <- list(
  v = c(0, 1), # Since we're on | rn
  pos = c((start %/% 140) + 1, (start %% 140)),
  shape = "|",
  count = 1
)

get_next_space <- function(space, board = m) {

  if (space$shape %in% c("7", "L"))
    space$v <- rev(space$v)

  if (space$shape %in% c("F", "J"))
    space$v <- -rev(space$v)

  space$pos <- space$pos + space$v
  space$shape <- get_shape_from_coord(space, board)
  space$count <- space$count + 1
  space
}

get_shape_from_coord <- function(space, board) {
  board[space$pos[2], space$pos[1]]
}

while(space$shape != "S") {
  space <- get_next_space(space)
}

space$count / 2

# Pt II

# Build up a collection of edge spaces
space <- list(
  v = c(0, 1), # Since we're on | rn
  pos = c((start %/% 140) + 1, (start %% 140)),
  shape = "|",
  count = 1
)
edges <- tibble(x = space$pos[1], y = space$pos[2])
edges <- rbind(edges, c(space$pos[1], space$pos[2] - 1)) # S position
while(space$shape != "S") {
  space <- get_next_space(space)
  edges <- rbind(edges, space$pos)
}

space_2 <- list(
  v = c(0, 1), # Since we're on | rn
  pos = c((start %/% 140) + 1, (start %% 140)),
  shape = "|",
  count = 1
)

# I'm moving anti-clockwise
# So when v = c(0, 1) (going down),
# in = R (1, 0)
# D (0, 1) -> R (1, 0)
# L (-1, 0) -> D (0, 1)
# U (0, -1) -> (-1, 0)
# R (1, 0) -> (0, -1)
get_in_dir <- function(space) {
  d <- rev(space$v)
  d[2] <- -d[2]
  d
}

edges <- arrange(edges, x, desc(y))

center <- tibble(x = 0, y = 0)
get_up_ray <- function(space, edges) {
  if (!space$shape %in% c("F", "7", "-") || sum(space$v > 0))
    return(tibble())

  if (space$shape == "F" && space$v[2] == -1)
    return(tibble())

  edge_bound <- edges |>
    filter(
      x == space$pos[1],
      y < space$pos[2] # remember that more up = lower y
    )
  if (nrow(edge_bound) == 0)
    return(tibble())

  if (space$pos[2] - 1 == edge_bound[[2]][1])
    return(tibble())

  tibble(
    x = space$pos[1],
    y = seq(space$pos[2] - 1, edge_bound[[2]][1] + 1)
  )
}

space <- list(
  v = c(0, 1), # Since we're on | rn
  pos = c((start %/% 140) + 1, (start %% 140)),
  shape = "|",
  count = 1
)
centers <- tibble(x = 0, y = 0)
i <- 0
while(i <= nrow(edges) && space$shape != "S") {
  space <- get_next_space(space)
  centers <- rbind(centers, get_up_ray(space, edges))
  i <- i + 1
}
