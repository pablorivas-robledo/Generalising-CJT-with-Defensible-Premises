library(RcppAlgos)
library(data.table)

# function plurality_rule
# input: a non negative integer number of voters and
#        a vector with probability mass function for options
# output: probability that first option wins under plurality rule

plurality_rule <- function (total.voters, options) { 
  
  num.options = length(options)
  
  # Calculating how many votes are neede to win
  
  winning = ceiling((total.voters + 1)/num.options)
  
  difference = total.voters-winning
  
  df = compositionsGeneral(0:difference, length(options), repetition = TRUE, weak = TRUE)
  
  df[,1] = df[,1]+winning
  
  #Filtering cases in which the first option is the winning options with a general algorithm 
  
  preferred = which(df[,1] > df[,2])
  
  # Choosing only the cases where the first option wins
  df = df[preferred,]
  
  df=as.data.frame(df)
  rows = nrow(df)
  
  #convert the data.frame to a data.table
  table = setDT(df)
  
  #put the data in long format
  table = data.table::melt(df, measure.vars = names(df))
  
  table[, group := rep(1:rows, num.options)]
  
  #apply function to each group
  table[, probability := dmultinom(value, prob = options), by = "group"]
  
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
    #options = c(first, 1-first)
    prob = plurality_rule(total.voters, options)
    delta = df[df$competence == first, 2] - df[df$competence == (1-first), 2]
    summation = summation + (prob*delta)
  }
  
  prob = gamma + summation
  return(prob)
}

total.voters = 3
options = c(0.51, 0.49)
competence_sensitivity(total.voters, options)

# arriving at asymptote...
max = 0.7525 - 0.001
calc = 0
start = 11

while (max - calc > 0.00001) {
  calc = competence_sensitivity(start)
  print(start)
  print(calc)
  gc()
  start = start + 1
}

# computing the winning probabilities
values = c(3:21)
probs = c()
for (i in values){  
  probs <- c(probs, competence_sensitivity(i, options) )
}


# plotting the vector of winning probabilities against number of voters

plot( values, probs, 
      type = 'l', xlab="Number of voters", ylab = "Collective Competence",
      ylim = c(min(probs), max(probs)),
      col = "red",
      lwd = 4
)
title( "Probability that first option wins using plurality rule" )
abline( h=0.51, lwd = 4)

