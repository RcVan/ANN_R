#######################################
###          ANNlayer.r             ###
###           Ian, Fan              ###
###          2019.02.03             ###
#######################################

## Description
# This ANNlayer.r file defines a layer in ANN with modifiable parameters.


## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/")


## import ones function
# install.packages("optimbase")
library(optimbase)
source('Activate_Func.r')


## function starts from here
#######################################################


## feedForward(input, weights, bias_flag = TRUE, activate_func = tanh)
## parameters: 
# matrix: input(N dim * batch_size), weights(output_dim * input_dim[+1])
# boolean: bias_flag
# function: activate_func
## return:
# vector: output
feedForward = function(input, weights, bias_flag = TRUE, activate_func = tanh){
  
## Initialization
# activate function is tanh as default
  fx = activate_func
  
# extend input when bias_flag is TRUE
  if (bias_flag){
    batch_size = size(input)[2]
    # x: N+1 dim * batch_size
    x = rbind(rep(1, batch_size), input)
  }
  else
    x = input
  
  
## feed forward calculation
# x: N[+1] dim * P pattern
# weights: M * N[+1]
# y: M dim * P pattern
    Net = weights %*% x
    output = fx(Net)
    
    
## return result
    return (output)
  
}



## backProp(input, weights, delta, learning_rate, forgetting_factor, momentum, bias_flag = TRUE, layer1_flag = FALSE, derivative_activate_func = tanh_derivative)

## Note
# This function is designed for updating weights since 2nd layer, so the layer1_flag is set to FALSE as default;
# for layer 1, we do not calculate the delta, so the return type would be changed. Please set the flag as TRUE.

## parameters: 
# matrix: input(N dim * batch_size), weights(output_dim * input_dim[+1]), delta(M dim * batch_size), momentum(output_dim * input_dim[+1])
# numeric: learning_rate, forgetting_factor
# boolean: bias_flag, layer1_flag
# function: derivative_activate_func
## return a list, contains:
# matrix: updated_weights, updated_momentum
# vector: previous_delta
backProp = function(input, weights, delta, learning_rate, forgetting_factor, momentum, bias_flag = TRUE, layer1_flag = FALSE, derivative_activate_func = tanh_derivative){

## extend input when bias_flag is TRUE
  if (bias_flag){
    batch_size = size(input)[2]
    x = rbind(rep(1, batch_size), input)
    x_size = size(x)[1] 
  }
  else{
    x = input
    x_size = size(x)[1] 
  }
  
  
if (!layer1_flag){
## get previous_delta
# delta: M dim * P pattern
# weights: M * N[+1]
# x: N[+1] dim * P pattern
# previous_data: N dim * P pattern
# t(weights) %*% delta back prob the error to inputs
# [c(-1),] drops the delta vector of bias
  if (bias_flag)
    previous_delta = (t(weights) %*% delta * derivative_activate_func(x))[c(-1),]
  else
    previous_delta = (t(weights) %*% delta * derivative_activate_func(x))
  # previous_delta = t(colSums(t(weights) %*% delta) * t(derivative_activate_func(x)))          # bug
}
  
  
## update weights (add with batch)
# delta: M dim * P pattern 
# weights: M * N[+1]
# x: N[+1] dim * P pattern
# momentum: M * N[+1]
  updated_momentum = learning_rate * (delta %*% t(x)) + forgetting_factor * momentum
  updated_weights = weights + updated_momentum
  
  
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





