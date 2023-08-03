library(RcppAlgos)
library("tidyverse")
library("purrr")

set.seed(123)

rankings = function(options){
  perms = permuteGeneral(options) #permutations over the number of initial options
  final_probabilities = c()
  # Loop that generates all permutations and creates the new values
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
  return(final_probabilities)
}

simulate_jury = function(n_voters, probs){
  votes <- rmultinom(n_voters, 1, probs)
  a = sum(votes[1,]) + sum(votes[2,]) + sum(votes[3,]) + sum(votes[4,]) + sum(votes[5,]) + sum(votes[6,])
  ((a + sum(votes[13,]) + sum(votes[14,]) + sum(votes[17,]) + sum(votes[19,]) + sum(votes[20,]) + sum(votes[23,]) >
      sum(votes[7,]) + sum(votes[8,]) + sum(votes[9,]) + sum(votes[10,]) + sum(votes[11,]) + sum(votes[12,]) + 
      sum(votes[15,]) + sum(votes[16,]) + sum(votes[18,]) + sum(votes[21,]) + sum(votes[22,]) + sum(votes[24,])) & 
      (a + sum(votes[7,]) + sum(votes[8,]) + sum(votes[11,]) + sum(votes[19,]) + sum(votes[20,]) + sum(votes[21,]) > 
         sum(votes[13,]) + sum(votes[14,]) + sum(votes[15,]) + sum(votes[16,]) + sum(votes[17,]) + sum(votes[18,]) + 
         sum(votes[9,]) + sum(votes[10,]) + sum(votes[12,]) + sum(votes[22,]) + sum(votes[23,]) + sum(votes[24,]))) &
    (a + sum(votes[7,]) + sum(votes[8,]) + sum(votes[9,]) + sum(votes[13,]) + sum(votes[14,]) + sum(votes[15,]) >
       sum(votes[19,]) + sum(votes[20,]) + sum(votes[21,]) + sum(votes[22,]) + sum(votes[23,]) + sum(votes[24,]) + 
       sum(votes[10,]) + sum(votes[11,]) + sum(votes[12,]) + sum(votes[16,]) + sum(votes[17,]) + sum(votes[18,]))
}

simulate_condorcet <- function(n_voters, probs, p_e) {
  results <- map_dbl(1:10000, ~simulate_jury(n_voters, probs))
  return((mean(results)*p_e)+((1-mean(results))*(1-p_e)))
}

n_voters = 11
options <- c(0.4, 0.3, 0.2, 0.1)
probs = rankings(options)
p_e = 0.8

simulate_condorcet(n_voters, probs, p_e)

## asymptote
options =  c(0.4, 0.3, 0.3)
max = 0.51
calc = 0

start = 35

while (max - 0.001 > calc) {
  calc = simulate_condorcet(start , probs, max)
  print(start)
  print(calc)
  gc()
  start = start + 1
}
