#######################################
###         NML homework 4          ###
###     batchTrainN-LayerNN.r       ###
###           Ian, Fan              ###
###          2019.02.17             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/NMLh4")


## source .r file
source('N-layerNN.r')
source('ANNlayer.r')


## code starts from here
########################################################


## batchTrainNLayerNN(trainSet, trainSet_size, input_col, output_col, batch_size, layer_num, NNParameters, learning_rate_vec, forgetting_factor)
## parameters:
# matrix: trainSet(trainSet_size * (N+M)dim)
# vector: input_col(N-dim), output_col(M-dim), batch_size
# list: NNParameters(NNWeights, NNMomentum)
## return:
# list: updated_NNParameters(NNWeights, NNMomentum)
batchTrainNLayerNN = function(trainSet, trainSet_size, input_col, output_col, batch_size, layer_num, NNParameters, learning_rate_vec, forgetting_factor){
  
  ## train 1 epoch
  for (iter in c(0:(trainSet_size/batch_size - 1))) {
    
    # get NN parameters
    NNWeights = NNParameters[[1]]
    NNMomentum = NNParameters[[2]]
    
    # get training pattern x
    # sample batch 
    batch = trainSet[c((iter*batch_size +1):(iter*batch_size +batch_size)),]
    # transform x as N*batch_size
    x = t(batch[,input_col])
    # D = desired_y, transform as M*batch_size
    D = t(batch[,output_col])
    
    
    ## Feed Forward
    # NNFeedForward(layer_num, input, weights_list, fx_list)
    # return list(input_list, output)
    input_list_output = NNFeedForward(layer_num, x, NNWeights)
    input_list = input_list_output[[1]]
    output = input_list_output[[2]]
    
    ## Back Propagation
    # NNBackProp(layer_num, output, desired_output, input_list, weights_list, learning_rate_vec, forgetting_factor, momentum_list, fx_list)
    # return list(updated weight_list, momentum_list)
    backProp_result = NNBackProp(layer_num, output, D, input_list, NNWeights, learning_rate_vec, forgetting_factor, NNMomentum)
    
    
    ## update Network
    NNParameters = backProp_result
  }
  
  
  # return
  return (NNParameters)
  
}



## getPredictError(layer_num, weights, input, desired_output, errorMeasureFunc)
## parameters:
# numeric: layer_num
# matrix: input(data_size*N), desired_output(data_size*M)
# list: weights
# function: errorMeasureFunc
## return:
# numeric: error
getPredictError = function(layer_num, weights, input, desired_output, errorMeasureFunc){
  
  # transform x as N*data_size
  x = t(input)
  output_size = size(input)[1]
  
  
  ## get prediction
  # NNPrediction(layer_num, input, weights_list, fx_list)
  output = NNPrediction(layer_num, x, weights)
  pred_output = t(output)
  
  
  ## calculate error rate
  # errorMeasureFunc(desired_output, network_ouput, size)
  error = errorMeasureFunc(desired_output, pred_output, output_size)
  
  
  # return
  return (error)
}



## getPredictOutputError(layer_num, weights, input, desired_output, errorMeasureFunc)
## Note:
# This function also return the predicted output
## parameters:
# numeric: layer_num
# matrix: input(data_size*N), desired_output(data_size*M)
# list: weights
# function: errorMeasureFunc
## return:
# matrix: pred_output(data_size*M)
# numeric: error
getPredictOutputError = function(layer_num, weights, input, desired_output, errorMeasureFunc){
  
  # transform x as N*data_size
  x = t(input)
  output_size = size(input)[1]
  
  
  ## get prediction
  # NNPrediction(layer_num, input, weights_list, fx_list)
  output = NNPrediction(layer_num, x, weights)
  pred_output = t(output)
  
  
  ## calculate error rate
  # errorMeasureFunc(desired_output, network_ouput, size)
  error = errorMeasureFunc(desired_output, pred_output, output_size)
  
  
  # return
  return (list(pred_output, error))
}