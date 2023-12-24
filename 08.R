library(tidyverse)
library(igraph)

d <- read_lines("08.csv")

dirs <-  d[1]

nodes <- d[-(1:2)]

parse_nodes <- function(nodes) {
  tibble(node = nodes) |>
    mutate(
      name = str_extract(node, "^[A-Z]{3}"),
      l = str_extract(node, "(?<=\\()[A-Z]{3}"),
      r = str_extract(node, "(?<=\\, )[A-Z]{3}"),
    )
}

pn <- parse_nodes(nodes)

g <- pivot_longer(pn, c(l, r), names_to = "dest")
g <- graph_from_data_frame(g[, c(2, 4)])
cl <- clusters(g)
zzz_cl <- cl$membership[which(names(cl$membership) == "ZZZ")]
nodes <- which(cl$membership == zzz_cl)

pn_filt <- pn |>
  filter(name %in% names(nodes))

pn_filt_long <- pn_filt |>
  pivot_longer(c(l, r), names_to  = "dest")

g_filt <- graph_from_data_frame(pn_filt_long[, c(2, 4)])

dirs_as_num <- dirs |>
  strsplit("") |>
  unlist()
dirs_as_num <- ifelse(dirs_as_num == "L", 0, 1)
dirs_as_col_idx <- dirs_as_num + 3

i <- 1
cur_node <- "AAA"
while (cur_node != "ZZZ") {
  dir_idx <- ifelse(i %% length(dirs_as_num) == 0, length(dirs_as_num), i %% length(dirs_as_num))
  dir_col <- dirs_as_col_idx[dir_idx]
  cur_node <- pn_filt[which(pn_filt$name == cur_node), dir_col][[1]]
  i <- i + 1
}
i - 1

# Pt II
#
# I'm pretty sure each node is on it's own cluster
#
# If that's the case, we can calculate the number of times it takes for each one
# to get to its dest and then the amount of time it needs to get back to the
# start (??A --> ??Z --> ??A) - both arrows = full loop. Then find least common
# denoninator between all of them

# Let's start with AAA as a toy example

# Looks like things don't go back to AAA but they DO go back to ZZZ

cl_t <- tibble(name = names(cl$membership), cluster = cl$membership)

pn_w_cl <- left_join(pn, cl_t, by = "name")

get_a_to_z_to_z <- function(node_set, n = 6) {
  i <- 1
  goal_node <- node_set[which(endsWith(node_set$name, "Z")),]$name
  cur_node <- node_set[which(endsWith(node_set$name, "A")),]$name
  hits <- c()
  while (length(hits) < (n + 1)) {
    if (goal_node == cur_node) hits <- c(hits, i - 1)
    dir_idx <- ifelse(i %% length(dirs_as_num) == 0, length(dirs_as_num), i %% length(dirs_as_num))
    dir_col <- dirs_as_col_idx[dir_idx]
    cur_node <- node_set[which(node_set$name == cur_node), dir_col][[1]]
    i <- i + 1
  }
  hits
}

zz1 <- get_a_to_z_to_z(filter(pn_w_cl, cluster == 1))
zz2 <- get_a_to_z_to_z(filter(pn_w_cl, cluster == 2))
zz3 <- get_a_to_z_to_z(filter(pn_w_cl, cluster == 3))
zz4 <- get_a_to_z_to_z(filter(pn_w_cl, cluster == 4))
zz5 <- get_a_to_z_to_z(filter(pn_w_cl, cluster == 5))
zz6 <- get_a_to_z_to_z(filter(pn_w_cl, cluster == 6))

# cl 5: GPA = SKD MRM
# cl 5: CVZ = MRM SDK
# cl 6: SKZ = VLF TVV
# cl 6: BBA = TVV VLF

# A -> Z distance is always same as Z -> Z distance

lcm <- function(x, y) {
  og_x <- x
  og_y <- y
  while(x != y) {
    if (x > y) {
      y <- y + og_y
    } else {
      x <- x + og_x
    }
  }
  x
}

sm <- lcm(zz1[1], zz2[1]) |>
  lcm(zz3[1]) |>
  lcm(zz4[1]) |>
  lcm(zz5[1]) |>
  lcm(zz6[1])
