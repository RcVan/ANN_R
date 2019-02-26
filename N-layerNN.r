#######################################
###          N-layerNN.r            ###
###           Ian, Fan              ###
###          2019.02.08             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/")


## source .r file
source('initWeights.r')
source('ANNlayer.r')

########################################################


# initNLayerNN(layer_num, layer_size_vec, weights_range_matrix)
## parameters: 
# numeric: layer_num
# vector: layer_size_vec
# matrix: weights_range_matrix
## return a list:
# list: weights_list, momentum_list
initNLayerNN = function(layer_num, layer_size_vec, weights_range_matrix){
  
  
  # save all weights
  weights_list = list()
  # save all momentum
  momentum_list = list()
  
  for (i in seq(1, layer_num)){
    # initialize
    current_Weights = initWeights(layer_size_vec[i], layer_size_vec[i+1], weights_range_matrix[,i])
    current_Momentum = initWeights(layer_size_vec[i], layer_size_vec[i+1], c(0,0))
    
    # save
    weights_list = append(weights_list, list(current_Weights))
    momentum_list = append(momentum_list, list(current_Momentum))
  }
  
  return(list(weights_list, momentum_list))
}



# NNPrediction(layer_num, input, weights_list, fx_list)
## parameters: 
# numeric: layer_num;
# vector: input;
# list: weights_list, fx_list
## return:
# vector: output
NNPrediction = function(layer_num, input, weights_list){
  
  # get input
  Net_i = input
  
  for (i in seq(1, layer_num)){
    ## Feed Forward
    # feedForward(input, weights)
    # Layer i
    Net_i = feedForward(Net_i, weights_list[[i]])

  }
  
  # get ouput
  output = Net_i
  
  # return
  return(output)
  
}



# NNFeedForward(layer_num, input, weights_list, fx_list)
## Note:
# NNFeedForward is based on NNPredition, but this function would save the intermediate computing result (Net as next layer input).
## parameters: 
# numeric: layer_num;
# vector: input;
# list: weights_list, fx_list
## return a list, include:
# list: input_list
# vector: output
NNFeedForward = function(layer_num, input, weights_list){
  
  # get input
  Net_i = input
  
  # save intermeidate result
  input_list = list()
  
  for (i in seq(1, layer_num)){
    # join input
    input_list = append(input_list, list(Net_i))
    
    ## Feed Forward
    # feedForward(input, weights)
    # Layer i
    Net_i = feedForward(Net_i, weights_list[[i]])
    
  }
  
  # get ouput
  output = Net_i
  
  # return list(input_list, output)
  return(list(input_list, output))
  
}



# NNBackProp(layer_num, output, desired_output, input_list, weights_list, learning_rate_vec, forgetting_factor, momentum_list, fx_list)
## parameters: 
# numeric: layer_num, forgetting_factor;
# vector: output, desired_output, learning_rate_vec;
# list: input_list, weights_list, momentum_list, fx_list
## return:
# list: new_weights_list, updated_momentum_list
NNBackProp = function(layer_num, output, desired_output, input_list, weights_list, learning_rate_vec, forgetting_factor, momentum_list){
  
  
  # error in the last Layer 
  delta_final = (desired_output - output) * (1 - output^2)
  previous_delta = delta_final
  
  
  ## Back Propagation
  # backProp(input, weights, delta, learning_rate, layer1_flag = FALSE)
  # back prop every layer
  for (i in seq(layer_num,2)){
    result = backProp(input_list[[i]], weights_list[[i]], previous_delta, learning_rate_vec[[i]], forgetting_factor, momentum_list[[i]])
    
    # update
    weights_list[[i]] = result[[1]]
    momentum_list[[i]] = result[[2]]
    previous_delta = result[[3]]
    
  }
  
  # update weights in Layer 1
  update_layer1 = backProp(input_list[[1]], weights_list[[1]], previous_delta, learning_rate_vec[[1]], forgetting_factor, momentum_list[[1]], layer1_flag = TRUE)
  weights_list[[1]] = update_layer1[[1]]
  momentum_list[[1]] = update_layer1[[2]]
  
  # return updated weight_list, momentum_list
  return(list(weights_list, momentum_list))
}











