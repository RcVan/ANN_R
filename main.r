#######################################
###         NML homework 4          ###
###         classifyIris.r          ###
###           Ian, Fan              ###
###          2019.02.12             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/NMLh4")


## source .r file
source('N-layerNN.r')
source('batchTrainN-LayerNN.r')
source('kfoldCrossValidation.r')
source('loadData.r')
source('classifyError.r')


# seed
seed = 1024
set.seed(seed)
## code starts from here
########################################################

## NN parameters

# layer number
layer_num = 2

# input size
input_size = 4
# hidden layer size
hidden_size1 = 5
# output size
output_size = 3
# layer size
layer_size = c(input_size, hidden_size1, output_size)

# weight init range
range = c(-0.1, 0.1)
weights_range = matrix(rep(range, layer_num), ncol = layer_num)

# activate function
fx = tanh

# learning rate Eta
eta = 0.2
learning_rate_vec = c(eta, 0.1) #rep(eta, layer_num)
learning_rate_shrink_factor = 0.9

# forgetting factor for momentum
alpha = 0.9

# (4,5,3)(0.4, 0.2)*0.9, 0.5, 10, 3000
# (4,5,3)(0.2,0.1)*0.9, 0.9, 5, 3000
## Data parameters
# batch size
batch_size = 5

# trainset size
trainSet_size = 100
# testset size
testSet_size = 50


## Training parameters
# training step N
n_step = 3000

# epsilon 
ep = 0.05
saved = FALSE

# N-fold
k = 3



## Load Data
# source('loadData.r')
trainSet = as.matrix(trainData)
testSet = as.matrix(testData)
alldata = as.matrix(rbind(trainData, testData))
# data in set
input_col = c(1:4)
output_col = c(5:7)



## Initializaion ANN
# initNLayerNN(layer_num, layer_size_vec, weights_range_matrix)
NNParameters = initNLayerNN(layer_num, layer_size, weights_range)
# print(NNWeights)



## k-fold cross validation
# kFoldData(k, dataset_size, seed)
# Warning: setting seed in this function would cause some change with other place using seed
kfold_seq = kFoldData(k, (trainSet_size+testSet_size), seed)
# get trainSet like this
# trainSet = alldata[-c(kfold_seq[1,]),]
fold_num = 2
trainSet = alldata[-c(kfold_seq[fold_num,]),]
testSet = alldata[c(kfold_seq[fold_num,]),]



## Train the network
# training iterates for n_step times
# initial error rate as 1
error = data.frame(Train = c(1), Test = c(1))
i = 2
while(i <= n_step){
  
  ## train 1 epoch
  # batchTrainNLayerNN(trainSet, trainSet_size, input_col, output_col, batch_size, layer_num, NNParameters, learning_rate_vec, forgetting_factor)
  NNParameters = batchTrainNLayerNN(trainSet, trainSet_size, input_col, output_col, batch_size, layer_num, NNParameters, learning_rate_vec, alpha)
  NNWeights = NNParameters[[1]]
  
  
  ## for every 100 iterations, record error of trainSet and testSet
  if (i %% 100 == 0){
    
    ## Recall and Test
    # calculate error rate
    # getPredictError(layer_num, weights, input, desired_output, errorMeasureFunc)
    error_rate_train = getPredictError(layer_num, NNWeights, trainSet[,input_col], trainSet[,output_col], calculateError)
    error_rate_test = getPredictError(layer_num, NNWeights, testSet[,input_col], testSet[,output_col], calculateError)
    
    # print(c(RMSe_train, RMSe_test))
    error = rbind(error,c(error_rate_train, error_rate_test))
    
    
    ## early stopping detection
    if (error_rate_train <= ep && error_rate_test <= ep){
      
      learning_rate_vec = learning_rate_shrink_factor * learning_rate_vec
      ep = min(error_rate_train, error_rate_test)
      
      # record present info.
      stop_iter = i
      target_weights = NNWeights
      saved = TRUE
      print(paste0("Stop at iteration ", i))
    }
  }
  
  
  ## iteration para
  i = i+1
  
}



## Training Results
print("When finishing the training: ")
print(paste0("Iteration ", i))
print("Error: ")
print(error)



## if the network has an early stopping 
if (saved){
  
  # calculate error rate
  # getPredictOutputError(layer_num, weights, input, desired_output, errorMeasureFunc)
  trainResult = getPredictOutputError(layer_num, target_weights, trainSet[,input_col], trainSet[,output_col], calculateError)
  testResult = getPredictOutputError(layer_num, target_weights, testSet[,input_col], testSet[,output_col], calculateError)
  
  print(trainResult[[2]])
  print(testResult[[2]])
}



# plot Error throughout training
# plotClassifyError(error, error_size)
plt = plotClassifyError(as.data.frame(error[-1,]), (n_step / 100))
plt



## confusion matrix
## getConfusionMatrix(desired_output, network_ouput)
getConfusionMatrix(trainSet[,output_col], trainResult[[1]])
getConfusionMatrix(testSet[,output_col], testResult[[1]])


