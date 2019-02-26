#######################################
###          ANNlayer.r             ###
###           Ian, Fan              ###
###          2019.02.03             ###
#######################################

## Description
# This ANNlayer.r file defines a layer in ANN with modifiable parameters.


## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/NMLh4")


## import ones function
# install.packages("optimbase")
library(optimbase)


## function starts from here
#######################################################


## feedForward(input, weights)
## parameters: 
# matrix: weights(output_size * input_size+1)
# vector: input;
# numeric: input_size(without bias), output_size, batch_size(k patterns)
## return:
# vector: output
feedForward = function(input, weights){
  
## Initialization
# activate function
  fx = tanh
  
# extend input
# x: N+1 dim * P pattern
  batch_size = size(input)[2]
  x = rbind(rep(1, batch_size), input)
  
  
## feed forward calculation
# x: N+1 dim * P pattern
# weights: M * N+1
# y: M dim * P pattern
    Net = weights %*% x
    output = fx(Net)
    
    
## return result
    return (output)
  
}



## backProp(input, weights, delta, learning_rate, forgetting_factor, momentum, layer1_flag = FALSE)
## Note
# This function is designed for updating weights since 2nd layer, so the layer1_flag is set to FALSE as default;
# for layer 1, we do not calculate the delta, so the return type would be changed. Please set the flag as TRUE.
## parameters: 
# matrix: weights(output_size * input_size+1), momentum(output_size * input_size+1)
# vector: input, delta
# numeric: learning_rate, forgetting_factor
## return a list, contains:
# matrix: updated_weights, updated_momentum
# vector: previous_delta
backProp = function(input, weights, delta, learning_rate, forgetting_factor, momentum, layer1_flag = FALSE){

## extend input
  batch_size = size(input)[2]
  x = rbind(rep(1, batch_size), input)
  x_size = size(x)[1] 
  
  
if (!layer1_flag){
## get previous_delta
# delta: M dim * P pattern
# weights: M * N+1
# x: N+1 dim * P pattern
# previous_data: N dim * P pattern
# t(weights) %*% delta back prob the error to inputs
# [c(-1),] drops the delta vector of bias (which is zeros)
  previous_delta = t(colSums(t(weights) %*% delta) * t(ones(x_size, batch_size) - x^2))[c(-1),]
}
  
  
## update weights (add with batch)
# delta: M dim * P pattern 
# weights: M * N+1
# x: N+1 dim * P pattern
# momentum: M * N+1
  updated_momentum = learning_rate * (delta %*% t(x)) + alpha * momentum
  updated_weights = weights + learning_rate * updated_momentum
  
  
## return result  
if (!layer1_flag){  
## return list(updated_weights, updated_momentum, previous_delta)
  return(list(updated_weights, updated_momentum, previous_delta))
}
else{
## if in layer 1
  return(list(updated_weights, updated_momentum)) 
}
  
  
}





