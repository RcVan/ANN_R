#######################################
###         Activate_Func.r         ###
###           Ian, Fan              ###
###          2019.03.23             ###
#######################################

## Description
# This Activate_Func.r file contains the activate functions and their derivatives.


## clear variables and set work directory
# rm(list = ls())
# setwd("/Users/rcusf/Desktop/")


## import ones function
# install.packages("optimbase")
library(optimbase)


## function starts from here
#######################################################

# tanh
tanh_derivative = function(x){
  
  # 1- tanh(x)^2
  return(ones(size(x)[1], size(x)[2]) - tanh(x)^2) 
}