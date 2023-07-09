library(RcppAlgos)
library(data.table)

# function plurality_rule
# input: a non negative integer number of voters and
#        a vector with probability mass function for options
# output: probability that first option wins under plurality rule

pairwise_condorcet = function(voters, options){
  # Calculating the probabilities for the orderings from probs over single votes
  
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
    true_prob = prod(values_for_final) #Probs for the orderings
    
    final_probabilities[n] = true_prob
  }
  
  candidates = permuteGeneral(letters[1:length(options)])
  
  df = compositionsGeneral(0:voters, length(1:nrow(candidates)), repetition = TRUE, weak = TRUE)
  
  
  most_preferred = intersect(which(df[,1] + df[,2] + df[,5] > df[,3] + df[,4] + df[,6]), 
                             which(df[,1] + df[,2] + df[,3] > df[,5] + df[,6] + df[,4]))
  
  df = df[most_preferred,]
  
  df=as.data.frame(df)
  rows = nrow(df)
  
  #convert the data.frame to a data.table
  table = setDT(df)
  
  #put the data in long format
  table = data.table::melt(df, measure.vars = names(df))
  
  table[, group := rep(1:rows, length(final_probabilities))]
  
  #apply function to each group
  table[, probability := dmultinom(value, prob = final_probabilities), by = "group"]
  
  whole.group = head(table, rows)
  prob = sum(whole.group$probability)
  return(prob)
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
    prob = pairwise_condorcet(total.voters, options)
    delta = df[df$competence == first, 2] - df[df$competence == (1-first), 2]
    summation = summation + (prob*delta)
  }
  
  prob = gamma + summation
  return(prob)
}

##voters
total.voters = 11
##options
options = c(0.625, 0.1875, 0.1875)

competence_sensitivity(total.voters, options)

# arriving at asymptote...
max = 0.7525 - 0.001
calc = 0
start = 11

while (max - calc > 0.00001) {
  calc = competence_sensitivity(start, options)
  print(start)
  print(calc)
  gc()
  start = start + 1
}

# computing the winning probabilities
values = c(7:21)
probs = c()
for (i in values){  
  probs <- c(probs, competence_sensitivity(i , options) )
}


# plotting the vector of winning probabilities against number of voters

plot( values, probs, 
      type = 'l', xlab="Number of voters", ylab = "Collective Competence",
      ylim = c(min(probs), max(probs)) 
)
title( "Probability that first option wins using plurality rule" )
abline( v=7)
