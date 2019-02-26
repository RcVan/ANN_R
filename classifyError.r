#######################################
###         NML homework 4          ###
###       classifyResult.r          ###
###           Ian, Fan              ###
###          2019.02.15             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/NMLh4")


## import ggplot2
library(ggplot2)
# install.packages("optimbase")
library(optimbase)
# install.packages("e1071")
library(caret)

## code starts from here
########################################################


## getConfusionMatrix(desired_output, network_ouput)
## parameters:
# vector: desired_output, network_ouput
# numeric: size
## return:
# numeric: confusionMatrix
getConfusionMatrix = function(desired_output, network_ouput){
  
  # function: get class
  getClass = function(x){
    return (x[1]+2*x[2]+3*x[3])
  }
  
  # process network output as classification result
  network_ouput = t(apply(network_ouput, 1, function(x){ return(ifelse(x==max(x), 1, 0)) } ))
  
  # get class for output
  pred_class = as.factor(apply(network_ouput, 1, getClass))
  true_class = as.factor(apply(desired_output, 1, getClass))
  
  
  # generate confusion matrix from class
  confusionMatrix = confusionMatrix(pred_class, true_class)
  
  return(confusionMatrix)
}



## calculateError(desired_output, network_ouput, size)
## parameters:
# vector: desired_output, network_ouput
# numeric: size
## return:
# numeric: error_rate
calculateError = function(desired_output, network_ouput, size){
  
  # get confusion matrix
  confusion_matrix = as.matrix(as.table(getConfusionMatrix(desired_output, network_ouput)))
  
  # process error data
  confusion_matrix[1,1]
  error_num = sum(confusion_matrix) - sum(confusion_matrix[1,1] + confusion_matrix[2,2] + confusion_matrix[3,3])
  
  # calculate error rate
  error_rate = error_num / size
  
  return(error_rate)
}



## plotClassifyError(error, error_size)
## parameters: 
# matrix: error(error_train, error_test)
# numeric: error_size
## return:
# plot
plotClassifyError = function(error, error_size){
  
  plt = ggplot(error, aes(x = factor(c(1:error_size)))) + 
    geom_line(aes(y = error[,1], colour = "Train"), group = 1) + 
    geom_line(aes(y = error[,2], colour="Test"), group = 1) +
    scale_colour_manual("", breaks = c("Train", "Test"), values = c("red", "green")) + 
    labs(title="Training History",
         x = "Training Step * 750", y = "Classify Error")
  
  return(plt)
}





