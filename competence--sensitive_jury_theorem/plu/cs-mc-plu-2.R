library("tidyverse")
library("purrr")

set.seed(123)

simulate_jury <- function(n_voters, p) {
  votes <- rbinom(n_voters, 1, p)
  sum(votes) > ceiling((n_voters + 1)/2)
}

competence_sensitivity = function(total.voters) {
  competence <- c(0, 0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875, 1)
  probability_of_competence <- c(0.01, 0.025, 0.05, 0.0875, 0.15, 0.375, 0.18, 0.1, 0.0225)
  
  df = data.frame(competence, probability_of_competence)
  
  tends_to_competence = which(df[,1] > 0.5)
  
  tends_to_competence = df[tends_to_competence,]
  
  converges_to = sum(tends_to_competence$probability_of_competence) + (0.5*df[df$competence == 0.5, 2])
  gamma = which(df[,1] < 0.5)
  gamma = df[gamma, ]
  gamma = sum(gamma$probability_of_competence) + (0.5*df[df$competence == 0.5, 2])
  
  summation = 0
  
  for (i in 1:nrow(tends_to_competence)) {
    first = tends_to_competence[i,1]
    options = c(first, 1-first)
    results <- map_dbl(1:10000, ~simulate_jury(total.voters, options))
    prob = mean(results)
    delta = df[df$competence == first, 2] - df[df$competence == (1-first), 2]
    summation = summation + (prob*delta)
  }
  
  prob = gamma + summation
  return(prob)
}

total.voters = 11

competence_sensitivity(total.voters)
