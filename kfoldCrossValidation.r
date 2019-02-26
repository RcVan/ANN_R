#######################################
###         NML homework 4          ###
###     kfoldCrossValidation.r      ###
###           Ian, Fan              ###
###          2019.02.17             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/NMLh4")


## import size function
# install.packages("optimbase")
library(optimbase)


## code starts from here
########################################################

## kFoldData(k, dataset_size, seed)
## parameters:
# numeric: k, dataset_size, seed
## return:
# matrix: kfold_sequence( k * dataset_size/k)
kFoldData = function(k = 3, dataset_size, seed = 1024){
  
  # random the sequence
  set.seed(seed)
  
  # get data set size
  data_size = dataset_size
  sequence_size = as.integer((data_size-1)/k) + 1
  
  # sample 
  sample_sequence = sample(data_size, data_size)
  
  # process output
  kfold_sequence = c()
  for (iter in c(0:(k-1))){
    kfold_sequence = rbind(kfold_sequence, sample_sequence[seq(iter*sequence_size,(iter+1)*sequence_size)])
  }
  
  return(kfold_sequence)
  
}