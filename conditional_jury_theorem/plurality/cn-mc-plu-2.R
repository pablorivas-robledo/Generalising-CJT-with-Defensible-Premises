
library("tidyverse")
library("purrr")

set.seed(123)

#### 2-options

n_voters <- 100
p <- 0.52
p_e = 0.51

simulate_jury <- function(n_voters, p) {
  votes <- rbinom(n_voters, 1, p)
  sum(votes) > ceiling((n_voters + 1)/2)
}

results <- map_dbl(1:10000, ~simulate_jury(n_voters, p))
prob = mean(results)

final_prob = (prob*p_e)+((1-prob)*(1-p_e))
final_prob