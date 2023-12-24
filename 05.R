library(tidyverse)

d <- read_lines("05.csv")

seeds <- strsplit(d[1], " ")[[1]][-1] |> as.numeric()

breaks <- which(d == "")


parse_map <- function(map) {
  map |>
    read_table(
      col_names = c("dest_st", "source_st", "range")
    ) |>
    arrange(source_st) |>
    mutate(
      source_end = source_st + range - 1,
      dest_end = dest_st + range - 1
    ) |>
    relocate(source_st, source_end, dest_st, dest_end) |>
    add_missing_ranges()
}

add_missing_ranges <- function(arr_map) {
  og_map <- arr_map
  arr_map$diff <- c(diff(arr_map$source_st), NA_real_)
  with_missing_range <- arr_map |>
    mutate(
      range_missing = diff - range,
      missing_st = ifelse(range_missing != 0, source_end + 1, NA),
      missing_end = ifelse(range_missing != 0, source_end + range_missing, NA)
    ) |>
    filter(range_missing != 0)
  missing_tibble <- tibble(
    source_st = with_missing_range$missing_st,
    source_end = with_missing_range$missing_end,
    dest_st = with_missing_range$missing_st,
    dest_end = with_missing_range$missing_end,
    range = with_missing_range$range_missing
  )
  rbind(og_map, missing_tibble) |>
    arrange(source_st)
}

se_so <- d[(breaks[1] + 2):(breaks[2] - 1)] |> parse_map()
so_fe <- d[(breaks[2] + 2):(breaks[3] - 1)] |> parse_map()
fe_wa <- d[(breaks[3] + 2):(breaks[4] - 1)] |> parse_map()
wa_li <- d[(breaks[4] + 2):(breaks[5] - 1)] |> parse_map()
li_te <- d[(breaks[5] + 2):(breaks[6] - 1)] |> parse_map()
te_hu <- d[(breaks[6] + 2):(breaks[7] - 1)] |> parse_map()
hu_lo <- d[(breaks[7] + 2):length(d)] |> parse_map()


convert <- function(seeds, map) {
  out <- c()
  for (seed in seeds) {
    for (i in seq_len(nrow(map))) {
      row <- map[i, ]
      if (seed >= row$source_st && seed <= row$source_end) {
        out <- c(out, seed + row$dest_st - row$source_st)
        break()
      }
      if (seed < row$source_end) {
        out <- c(out, seed)
        break()
      }
      if (seed > row$source_st && seed > row$source_end && i == nrow(map)) {
        out <- c(out, seed)
        break()
      }
    }
  }
  out
}

1055427336 |>
  convert(se_so) |>
  convert(so_fe) |>
  convert(fe_wa) |>
  convert(wa_li) |>
  convert(li_te) |>
  convert(te_hu) |>
  convert(hu_lo)


hu_lo <- hu_lo |> arrange(dest_st)

best <- hu_lo$source_st[1]

back_convert <- function(seed, map) {
  row <- filter(map, dest_st <= seed, dest_end >= seed)
  diff <- row$source_st - row$dest_st
  seed + diff
}

seed_ranges <- tibble(
  st = seeds[seq(1, length(seeds), by = 2)],
  end = st + seeds[seq(2, length(seeds), by = 2)]
) |>
  arrange(st)

in_seeds <- function(n, seed_ranges) {
  nrow(filter(seed_ranges, st <= n, end >= n)) > 0
}

n <- hu_lo$source_st[1] |>
  back_convert(te_hu) |>
  back_convert(li_te) |>
  back_convert(wa_li) |>
  back_convert(fe_wa) |>
  back_convert(so_fe) |>
  back_convert(se_so)

hu_to_se <- function(hu) {
  hu |>
    back_convert(te_hu) |>
    back_convert(li_te) |>
    back_convert(wa_li) |>
    back_convert(fe_wa) |>
    back_convert(so_fe) |>
    back_convert(se_so)
}

bin_search <- function(low, high) {
  mid <- low + ((high - low) / 2)
  mid <- floor(mid)
  if (mid == low) {
    return(F)
  }

  seed_n <- hu_to_se(mid)
  if (in_seeds(seed_n, seed_ranges)) {
    print(seed_n)
    high <- mid
  } else {
    low <- mid
  }

  bin_search(low, high)
}


# Need to do binary search
i <- 0
while(!in_seeds(n, seed_ranges)) {
  n <- (hu_lo$source_st[1] + i) |>
  back_convert(te_hu) |>
  back_convert(li_te) |>
  back_convert(wa_li) |>
  back_convert(fe_wa) |>
  back_convert(so_fe) |>
  back_convert(se_so)
  i <- i + 1
}

ranges <- c(seeds[1], seeds[1] * seeds[2])
