library(RcppAlgos)
library("tidyverse")
library("purrr")

set.seed(123)

rankings = function(options){
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
  return(final_probabilities)
}

simulate_jury <- function(n_voters, final_probabilities) {
  votes <- rmultinom(n_voters, 1, final_probabilities)
  ((sum(votes[1,]) + sum(votes[2,]))*2) + sum(votes[3,]) + sum(votes[5,]) > 
    ((sum(votes[3,]) + sum(votes[4,]))*2) + sum(votes[1,]) + sum(votes[6,]) & 
    ((sum(votes[1,]) + sum(votes[2,]))*2) + sum(votes[3,]) + sum(votes[5,]) > 
    ((sum(votes[5,]) + sum(votes[6,]))*2) + sum(votes[2,]) + sum(votes[2,])
}

competence_sensitivity = function(total.voters, options) {
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
    results <- map_dbl(1:10000, ~simulate_jury(total.voters, final_probabilities))
    prob = mean(results)
    delta = df[df$competence == first, 2] - df[df$competence == (1-first), 2]
    summation = summation + (prob*delta)
  }
  
  prob = gamma + summation
  return(prob)
}

total.voters = 11
options = c(0.625, 0.1875, 0.1875)
final_probabilities = rankings(options)

competence_sensitivity(total.voters)


# arriving at asymptote...
calc = 0
start = 3

while (0.752 > calc) {
  calc = competence_sensitivity(start, options)
  print(start)
  print(calc)
  gc()
  start = start + 1
}