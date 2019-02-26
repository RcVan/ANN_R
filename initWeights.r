#######################################
###         initWeights.r           ###
###           Ian, Fan              ###
###          2019.02.03             ###
#######################################

## Description
# This initWeights.r file generate weights with modifiable parameters.


## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/NMLh4")


## import zeros function
# install.packages("optimbase")
library(optimbase)


## code starts from here
########################################################

## initWeights(input_size, output_size, range)
## parameters: 
# vector: range
# numeric: input_size(without bias), output_size, 
## return:
# matrix: weights (output_size * input_size+1)
initWeights = function(input_size, output_size, range){
  
## get range 
  mi = range[1]
  ma = range[2]
  
## initialization
# remember that input is without bias 
  weights = zeros(output_size, input_size + 1)
  
## random weights
  for (i in c(1:output_size))
    weights[i,] = runif((input_size + 1), min = mi, max = ma)
  
## return 
  return(weights)
}
