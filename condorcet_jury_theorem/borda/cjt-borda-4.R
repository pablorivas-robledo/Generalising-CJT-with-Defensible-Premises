library(RcppAlgos)
library(data.table)

# function plurality_rule
# input: a non negative integer number of voters and
#        a vector with probability mass function for options
# output: probability that first option wins under plurality rule

borda_count = function(voters, options){
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
  
  a = ((df[,1] + df[,2] + df[,3] + df[,4] + df[,5] + df[,6]) * 3) + ((df[,7] + df[,8] + df[,13] + df[,14] + df[,19] + df[,20]) * 2) + df[,9] + df[,11] + df[,15] + df[,17]+ df[,21] + df[,23]
  
  better_than_2 = which(a > ((df[,7] + df[,8] + df[,9] + df[,10] + df[,11] + df[,12]) * 3) + 
                          ((df[,1] + df[,2] + df[,15] + df[,16] + df[,21] + df[,22]) * 2) + 
                          df[,3] + df[,5] + df[,13] + df[,18]+ df[,19] + df[,24])
  better_than_3 = which(a > ((df[,13] + df[,14] + df[,15] + df[,16] + df[,17] + df[,18]) * 3) +
                          ((df[,3] + df[,4] + df[,9] + df[,10] + df[,23] + df[,24]) * 2) + 
                          df[,1] + df[,6] + df[,7] + df[,12]+ df[,20] + df[,22])
  better_than_4 = which(a > ((df[,19] + df[,20] + df[,21] + df[,22] + df[,23] + df[,24]) * 3) + 
                          ((df[,5] + df[,6] + df[,11] + df[,12] + df[,17] + df[,18]) * 2) 
                        + df[,2] + df[,4] + df[,8] + df[,10]+ df[,14] + df[,16])
  better_than_most = intersect(better_than_2, better_than_3)
  
  most_preferred = intersect(better_than_most, better_than_4)
  
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

#Example to run the function

##voters
total.voters = 7
##options
options = c(0.7, (1-0.7)/3, (1-0.7)/3, (1-0.7)/3)

## one call of the function 
borda_count(total.voters, options)



