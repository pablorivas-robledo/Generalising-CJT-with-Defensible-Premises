library(RcppAlgos)
library(data.table)

# function plurality_rule
# input: a non negative integer number of voters and
#        a vector with probability mass function for options
# output: probability that first option wins under plurality rule

plurality_rule <- function (total.voters, options, evidence) { 
  
  num.options = length(options)
  
  winning = ceiling(total.voters/num.options) + 1
  
  df = compositionsGeneral(0:total.voters, length(options), repetition = TRUE, weak = TRUE)
  
  #Filtering cases in which the first option is the winning options with a general algorithm 
  
  ## Standard. Comment and choose one below to improve speed
  #preferred = c()
  #for (option in 2:length(options)) {
  #   most_a = which(df[,1] > df[,option])
  #   if (option == 2) {
  #     preferred = c(preferred, most_a)
  #   } else {
  #     preferred = intersect(preferred, most_a)
  #   }
  # }
  
  ########################################################################
  #Filtering cases in which the first option is the winning options with an specific algorithm 
  
  #Un-comment for three options
  preferred = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
  
  #Un-comment for four options
  #preferred_b = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
  #preferred = intersect(preferred_b, which(df[,1] > df[,4]))
  
  #Un-comment for five options
  # preferred_b = intersect(which(df[,1] > df[,2]), which(df[,1] > df[,3]))
  # preffered_c = intersect(which(df[,1] > df[,4]), which(df[,1] > df[,5]))
  # preferred = intersect(preferred_b, preffered_c)
  
  ########################################################################

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
  
  final_prob = (evidence*prob) + ((1-evidence)*(1-prob))
  
  return(final_prob)
  
}

#Example to run the function

##voters
total.voters = 1000
##options
options = c(0.6, 0.2, 0.2)
## evidence
p_e = 0.8
## one call of the function 
plurality_rule(total.voters, options, p_e)


# arriving at asymptote...
max = p_e - 0.001
calc = 0
options =  c(0.6, 0.2, 0.2)

start = 10

while (max - calc > 0.00001) {
  calc = plurality_rule(start , options, p_e)
  print(start)
  print(calc)
  gc()
  start = start + 1
}
