library(RcppAlgos)
library("tidyverse")
library("purrr")

set.seed(123)


n_voters = 11
options <- c(0.5, 0.3, 0.2)

perms = permuteGeneral(options) #permutations over the number of initial options

# Loop that generates all permutations and creates the new values
final_probabilities = c()
for (n in 1:nrow(perms)) {
  initial_sequence = as.vector(perms[n,]) #all permutations
  new_sequence = c(initial_sequence[1]) # Cumulative sum of all the probs over single votes
  
  # Second loop to complete the values for the cumulative sum of all the probs
  for (i in 2:(length(initial_sequence))) {
    new_sequence[i] = new_sequence[i-1] + initial_sequence[i]
  }
  
  new_sequence = c(0, new_sequence)
  values_for_final = c() # Probs for the orderings
  
  
  # Third loop for the probs for the orderings
  for (i in 1:length(options)) {
    single = initial_sequence[i] / (1 - new_sequence[i])
    values_for_final[i] = single 
  }
  #Probs for the orderings
  true_prob = prod(values_for_final) 
  
  # Final values of the orderings based on the probability of individual option
  final_probabilities[n] = true_prob
}

simulate_jury <- function(n_voters, final_probabilities) {
  votes <- rmultinom(n_voters, 1, final_probabilities)
  sum(votes[1,]) + sum(votes[2,]) + sum(votes[5,]) > sum(votes[3,]) + sum(votes[4,]) + sum(votes[6,]) & sum(votes[1,]) + sum(votes[2,]) + sum(votes[3,]) > sum(votes[4,]) + sum(votes[5,]) + sum(votes[6,])
}

results <- map_dbl(1:10000, ~simulate_jury(n_voters, final_probabilities))

mean(results)
