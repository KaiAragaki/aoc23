# Read in data (and coerce to vector)
d <- read.csv("01_1.csv", header = FALSE)[[1]]

# Part 1
first <- strcapture("^[a-zA-Z]*([0-9])", d, data.frame(x = numeric()))
last <- strcapture(".*([0-9])", d, data.frame(x = numeric()))
(sum(first) * 10) + sum(last)

# Part 2
first <- strcapture(
  "(one|two|three|four|five|six|seven|eight|nine|[0-9])",
  d,
  data.frame(x = character())
)[[1]]

last <- strcapture(
  ".*(one|two|three|four|five|six|seven|eight|nine|[0-9])",
  d,
  data.frame(x = character())
)[[1]]

nn <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
first <- sum(as.numeric(first), match(first, nn), na.rm = TRUE)
last <- sum(as.numeric(last), match(last, nn), na.rm = TRUE)
(sum(first * 10) + sum(last))
