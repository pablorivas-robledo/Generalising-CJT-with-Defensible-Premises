library(RcppAlgos)
library("tidyverse")
library("purrr")

set.seed(123)

options <- c(0.251, (1-0.251)/3, (1-0.251)/3, (1-0.251)/3)
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
  a = ((sum(votes[1,]) + sum(votes[2,]) + sum(votes[3,]) + sum(votes[4,]) + sum(votes[5,]) + sum(votes[6,])) * 3) + 
  ((sum(votes[7,]) + sum(votes[8,]) + sum(votes[13,]) + sum(votes[14,]) + sum(votes[19,]) + sum(votes[20,])) * 2) + 
  sum(votes[9,]) + sum(votes[11,]) + sum(votes[15,]) + sum(votes[17,])+ sum(votes[21,]) + sum(votes[23,])
  ((( a > ((sum(votes[7,]) + sum(votes[8,]) + sum(votes[9,]) + sum(votes[10,]) + sum(votes[11,]) + sum(votes[12,])) * 3) + 
       ((sum(votes[1,]) + sum(votes[2,]) + sum(votes[15,]) + sum(votes[16,]) + sum(votes[21,]) + sum(votes[22,])) * 2) + 
       sum(votes[3,]) + sum(votes[5,]) + sum(votes[13,]) + sum(votes[18,])+ sum(votes[19,]) + sum(votes[24,])) & 
      (a > ((sum(votes[13,]) + sum(votes[14,]) + sum(votes[15,]) + sum(votes[16,]) + sum(votes[17,]) + sum(votes[18,])) * 3) + 
         ((sum(votes[3,]) + sum(votes[4,]) + sum(votes[9,]) + sum(votes[10,]) + sum(votes[23,]) + sum(votes[24,])) * 2) + 
         sum(votes[1,]) + sum(votes[6,]) + sum(votes[7,]) + sum(votes[12,])+ sum(votes[20,]) + sum(votes[22,]))) & 
      (a > ((sum(votes[19,]) + sum(votes[20,]) + sum(votes[21,]) + sum(votes[22,]) + sum(votes[23,]) + sum(votes[24,])) * 3) + 
         ((sum(votes[5,]) + sum(votes[6,]) + sum(votes[11,]) + sum(votes[12,]) + sum(votes[17,]) + sum(votes[18,])) * 2) + 
         sum(votes[2,]) + sum(votes[4,]) + sum(votes[8,]) + sum(votes[10,])+ sum(votes[14,]) + sum(votes[16,])))
}



results <- map_dbl(1:10000, ~simulate_jury(25, final_probabilities))
mean(results)

stable_sim = function(n_voters, final_probabilities){
  results <- map_dbl(1:10000, ~simulate_jury(n_voters, final_probabilities))
  mean(results)
  }


trials = c()
  
for (i in 1:10){
    trial = stable_sim(25, final_probabilities)
    trials = c(trials, trial)
}

mean(trials)
max(trials)
min(trials)
