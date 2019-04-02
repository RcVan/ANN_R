#######################################
###         NML homework 4          ###
###          N-layerNN.r            ###
###           Ian, Fan              ###
###          2019.02.08             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/NMLh4")


## source .r file
source('initWeights.r')
source('ANNlayer.r')

########################################################


# initNLayerNN(layer_num, layer_size_vec, weights_range_matrix, bias_flag = TRUE)
## parameters: 
# numeric: layer_num
# vector: layer_size_vec
# matrix: weights_range_matrix
## return a list:
# list: weights_list, momentum_list
initNLayerNN = function(layer_num, layer_size_vec, weights_range_matrix, bias_flag = TRUE){
  
  
  # save all weights
  weights_list = list()
  # save all momentum
  momentum_list = list()
  
  for (i in seq(1, layer_num)){
    # initialize
    # initWeights(input_size, output_size, range, bias_flag = TRUE)
    current_Weights = initWeights(layer_size_vec[i], layer_size_vec[i+1], weights_range_matrix[,i], bias_flag)
    current_Momentum = initWeights(layer_size_vec[i], layer_size_vec[i+1], c(0,0), bias_flag)
    
    # save
    weights_list = append(weights_list, list(current_Weights))
    momentum_list = append(momentum_list, list(current_Momentum))
  }
  
  return(list(weights_list, momentum_list))
}



# NNPrediction(layer_num, input, weights_list, fx_list, bias_flag = TRUE)
## parameters: 
# numeric: layer_num;
# vector: input;
# list: weights_list, fx_list
## return:
# vector: output
NNPrediction = function(layer_num, input, weights_list, bias_flag = TRUE){
  
  # get input
  Net_i = input
  
  for (i in seq(1, layer_num)){
    ## Feed Forward
    # feedForward(input, weights, bias_flag = TRUE)
    # Layer i
    Net_i = feedForward(Net_i, weights_list[[i]], bias_flag)

  }
  
  # get ouput
  output = Net_i
  
  # return
  return(output)
  
}



# NNFeedForward(layer_num, input, weights_list, fx_list, bias_flag = TRUE)
## Note:
# NNFeedForward is based on NNPredition, but this function would save the intermediate computing result (Net as next layer input).
## parameters: 
# numeric: layer_num;
# vector: input;
# list: weights_list, fx_list
## return a list, include:
# list: input_list
# vector: output
NNFeedForward = function(layer_num, input, weights_list, bias_flag = TRUE){
  
  # get input
  Net_i = input
  
  # save intermeidate result
  input_list = list()
  
  for (i in seq(1, layer_num)){
    # join input
    input_list = append(input_list, list(Net_i))
    
    ## Feed Forward
    # feedForward(input, weights, bias_flag = TRUE, activate_func = tanh)
    # Layer i
    Net_i = feedForward(Net_i, weights_list[[i]], bias_flag)
    
  }
  
  # get ouput
  output = Net_i
  
  # return list(input_list, output)
  return(list(input_list, output))
  
}



## getFinalDelta(output, desired_output, final_layer_weights, final_layer_input, bias_flag = TRUE, derivative_activate_func = tanh_derivative)
## parameters: 
# matrix: output, desired_output, final_layer_weights, final_layer_input;
# function: derivative_activate_func
## return:
# matrix: delta_final
getFinalDelta = function(output, desired_output, final_layer_weights, final_layer_input, bias_flag = TRUE, derivative_activate_func = tanh_derivative)
{
  
  if (bias_flag){
    batch_size = size(final_layer_input)[2]
    # x: N+1 dim * batch_size
    x = rbind(rep(1, batch_size), final_layer_input)
  }
  else
    x = input
  
  
  ## feed forward calculation
  # x: N[+1] dim * P pattern
  # final_layer_weights: M * N[+1]
  Net = final_layer_weights %*% x
  
  
  # error in the last Layer
  delta_final = (desired_output - output) * tanh_derivative(Net)   
  
  
  return(delta_final)
}




# NNBackProp(layer_num, output, desired_output, input_list, weights_list, learning_rate_vec, forgetting_factor, momentum_list, fx_list, bias_flag = TRUE)
## parameters: 
# numeric: layer_num, forgetting_factor;
# matrix: output, desired_output, learning_rate_vec;
# list: input_list, weights_list, momentum_list, fx_list
## return:
# list: new_weights_list, updated_momentum_list
NNBackProp = function(layer_num, output, desired_output, input_list, weights_list, learning_rate_vec, forgetting_factor, momentum_list, bias_flag = TRUE){
  
  
  # error in the last Layer 
  delta_final = getFinalDelta(output, desired_output, weights_list[[layer_num]], input_list[[layer_num]], bias_flag)
  previous_delta = delta_final
  
  
  ## Back Propagation
  # backProp(input, weights, delta, learning_rate, forgetting_factor, momentum, bias_flag = TRUE, layer1_flag = FALSE, derivative_activate_func = tanh_derivative)  
  # back prop every layer
  for (i in seq(layer_num,2)){
    result = backProp(input_list[[i]], weights_list[[i]], previous_delta, learning_rate_vec[[i]], forgetting_factor, momentum_list[[i]], bias_flag)
    
    # update
    weights_list[[i]] = result[[1]]
    momentum_list[[i]] = result[[2]]
    previous_delta = result[[3]]
    
  }
  
  # update weights in Layer 1
  update_layer1 = backProp(input_list[[1]], weights_list[[1]], previous_delta, learning_rate_vec[[1]], forgetting_factor, momentum_list[[1]], bias_flag, layer1_flag = TRUE)
  weights_list[[1]] = update_layer1[[1]]
  momentum_list[[1]] = update_layer1[[2]]
  
  # return updated weight_list, momentum_list
  return(list(weights_list, momentum_list))
}











