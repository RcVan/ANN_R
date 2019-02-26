#######################################
###         errorAndPlot.r          ###
###           Ian, Fan              ###
###          2019.02.05             ###
#######################################

## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/")


## import ggplot2
library(ggplot2)


## code starts from here
########################################################

## calculateError(desired_output, network_ouput)
## parameters:
# vector: desired_output, network_ouput
## return:
# numeric: RMSe
calculateError = function(desired_output, network_ouput){
  
  # get RMS error
  RMSe = sqrt(sum((desired_output - network_ouput)^2))
  
  return(RMSe)
}


## plotRMSerror(RMSe, RMSe_size)
## parameters: 
# matrix: RMSe(RMSe_train, RMSe_test)
# numeric: RMSe_size, trainSet_size
## return:
# plot
plotRMSerror = function(RMSe, RMSe_size){
  
  plt = ggplot(RMSe, aes(x = factor(c(1:RMSe_size)))) + 
    geom_line(aes(y = RMSe[,1], colour = "Train"), group = 1) + 
    geom_line(aes(y = RMSe[,2], colour="Test"), group = 1) +
    scale_colour_manual("", breaks = c("Train", "Test"), values = c("red", "green")) + 
    labs(title="Training History",
                   x = "Training Step * 4*10^4", y = "RMS Error of an epoch") 
  
  return(plt)
}


## plotTrainAndTest(Data)
## parameters: 
# dataFrame: Data( trainData(x, y), train_network_output, testData(x, y), test_network_output)
## return:
# plot
plotTrainAndTest = function(Data){
  rever = function(x){return (1/x)}
  
  # plot
  plt = ggplot() + 
    geom_line(aes(x = Data[,1], y = Data[,2], colour = "Target Func")) + 
    
    # geom_point(aes(x = Data[,1], y = Data[,2], colour = "Train", shape = "Train")) +
    geom_point(aes(x = Data[,1], y = Data[,3], colour = "predTrain", shape = "predTrain")) +
    # geom_point(aes(x = Data[,4], y = Data[,5], colour = "Test", shape = "Test")) +
    geom_point(aes(x = Data[,4], y = Data[,6], colour = "predTest", shape = "predTest")) +
    
    scale_colour_manual("", breaks = c("Target Func", "predTrain", "predTest"), values = c("red","cyan", "black")) + 
    scale_shape_manual("", breaks = c("predTrain", "predTest"), values = c(15, 20)) + 
    labs(title="Simulated F(x) = 1/x ",
         x = "x", y = "F(x) = 1/x")
  
  return(plt)
}



## plotBackgroud()
## parameters: 
# function: func
# vector: x_range
## return:
# plot
plotBackgroud = function(func, x_range = seq(0.1, 1, by = 0.01)){
  
  x = x_range #seq(0.1, 1, by = 0.01)
  y = func(x)
  Data = data.frame(x = x, y = y)
  
  target_name = c("Target Func")
  # plot
  plt = ggplot() + 
    geom_line(aes(x = Data[,1], y = Data[,2], colour = "Target Func")) + 
    scale_colour_manual("", breaks = target_name, values = c(1)) +
    labs(title="Simulated F(x) = 1/x ",
         x = "x", y = "F(x) = 1/x")
  
  return(list(plt,target_name))
}


## projectData(plt, Data)
## parameters: 
# plot: plt
# dataFrame: Data(trainData(x, y), train_network_output)
# numeric: global_data_num: for different shape and color
#
## return:
# plot
projectData = function(plt, Data, global_data_num = 1, previous_data_name = c(), target_name){
  
  legend = colnames(Data)
  new_data_name = c(previous_data_name, legend[3])
  # plot
  plt = plt + 
    
    # geom_point(aes(x = Data[,1], y = Data[,2], colour = legend[2], shape = legend[2])) +
    geom_point(aes(x = Data[,1], y = Data[,3], colour = legend[3], shape = legend[3])) +
    
    scale_colour_manual("", breaks = c(target_name, new_data_name), values = seq(1, global_data_num+1)) +
    scale_shape_manual("", breaks = new_data_name, values = seq(1, global_data_num))+ 
    xlim(0.1,0.25)
    
  
  return(list(plt, new_data_name))
}

# pl = projectData(p, data.frame( trainSet[c(1:200),], t(output_train)[c(1:200),]), 2)
# p2 = projectData(p3[[1]], data.frame(testSet[c(1:200),], Network2=t(output_test)[c(1:200),]), 3, target_name = p[[2]], previous_data_name = p3[[2]])