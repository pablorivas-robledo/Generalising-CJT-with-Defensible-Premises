library(RcppAlgos)
library(data.table)

# function plurality_rule
# input: a non negative integer number of voters and
#        a vector with probability mass function for options
# output: probability that first option wins under plurality rule

borda_count = function(voters, options, evidence){
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
  
  
  most_preferred = intersect(which(((df[,1] + df[,2]) * 2) + df[,3] + df[,5] > ((df[,3] + df[,4])*2) + df[,1] + df[,6]), 
                             which(((df[,1] + df[,2]) * 2) + df[,3] + df[,5] > ((df[,5] + df[,6])*2) + df[,2] + df[,4]))
  
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
  
  final_prob = (evidence*prob) + ((1-evidence)*(1-prob))
  return(final_prob)
}

#Example to run the function

##voters
total.voters = 15
##options
options = c(0.4, 0.3, 0.3)
## evidence
p_e = 0.8
## one call of the function 
borda_count(total.voters, options, p_e)


# arriving at asymptote...
max = p_e - 0.001
calc = 0
options =  c(0.6, 0.2, 0.2)

start = 10

while (max - calc > 0.00001) {
  calc = borda_count(start , options, p_e)
  print(start)
  print(calc)
  gc()
  start = start + 1
}


# computing the winning probabilities
values = c(7:21)
p = c()
for (i in values){  
  p<- c(p, borda_count(i , options, p_e) )
}


# plotting the vector of winning probabilities against number of voters

plot( values, p , 
      type = 'l', xlab="Number of voters", ylab = "Collective Competence",
      ylim = c(min(p), max(p)) 
)
title( "Probability that first option wins" )
abline( v=min_total.voters )