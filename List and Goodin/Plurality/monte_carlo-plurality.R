
library("tidyverse")
library("purrr")

set.seed(123)

#### 2-options

n_voters <- 5950
p <- 0.52

simulate_jury <- function(n_voters, p) {
  votes <- rbinom(n_voters, 1, p)
  sum(votes) > ceiling((n_voters + 1)/2)
}

results <- map_dbl(1:10000, ~simulate_jury(n_voters, p))

mean(results)

### n-options
n_voters <- 100
p <- c(0.35, 0.325, 0.325)

simulate_jury <- function(n_voters, p) {
  votes <- rmultinom(n_voters, 1, p)
  sum(votes[1,]) > ceiling(n_voters/length(p)) + 1
}

results <- map_dbl(1:10000, ~simulate_jury(n_voters, p))

mean(results)