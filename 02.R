library(tidyverse)

data <- read_lines("02.csv") |>
  as_tibble() |>
  separate(value, c("game_id", "contents"), sep = ":") |>
  mutate(game_id = str_extract(game_id, "[0-9]*$") |> as.numeric()) |>
  separate_longer_delim(contents, regex(";|,")) |>
  mutate(contents = trimws(contents)) |>
  separate(contents, c("number", "color"), sep = " ") |>
  mutate(number = as.numeric(number))

ans_pt_1 <- data |>
  mutate(invalid =
           (color == "red" & number > 12) |
           (color == "green" & number > 13) |
           (color == "blue" & number > 14)) |>
  filter(all(!invalid), .by = "game_id")

ans_pt_1$game_id |> unique() |> sum()

ans_pt_2 <- data |>
  filter(number == max(number), .by = c("color", "game_id")) |>
  distinct() |>
  summarize(power = prod(number), .by = game_id)

ans_pt_2$power |> sum()
