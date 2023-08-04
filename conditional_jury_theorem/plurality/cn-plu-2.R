library(RcppAlgos)
library(data.table)

# function plurality_rule
# input: a non negative integer number of voters and
#        a vector with probability mass function for options
# output: probability that first option wins under plurality rule

plurality_rule <- function (total.voters, options, evidence) { 
  
  num.options = length(options)

  winning = ceiling((total.voters + 1)/num.options)
  
  difference = total.voters-winning
  
  df = compositionsGeneral(0:difference, length(options), repetition = TRUE, weak = TRUE)
  
  df[,1] = df[,1]+winning
  
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
  table[, probability := (evidence*dmultinom(value, prob = options))+((1-evidence)*dmultinom(value, prob = rev(options))), by = "group"]
  
  whole.group = head(table, rows)
  prob = sum(whole.group$probability)
  
  return(prob)
  
}

#Example to run the function

##voters
total.voters = 5
##options
options = c(0.6, 0.4)
## evidence
p_e = 0.8
## one call of the function 
plurality_rule(total.voters, options, p_e)


# arriving at asymptote...
max = p_e - 0.001
calc = 0
options =  c(0.6, 0.4)

start = 10

while (max - calc > 0.00001) {
  calc = plurality_rule(start , options, p_e)
  print(start)
  print(calc)
  gc()
  start = start + 1
}

values = 5:21
options = c(0.6, 0.4)
p = c()
for (i in values){  
  p<- c(p, plurality_rule(i , options, p_e) )
}


# plotting the vector of winning probabilities against number of voters

plot( values, p , 
      type = 'l', xlab="Number of voters", ylab = "Collective Competence",
      ylim = c(min(p), max(p)) 
)
title( "probability that first option wins" )
abline( v=min_total.voters )