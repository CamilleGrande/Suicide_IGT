# Driver_SPAD.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script runs the SPAD analysis

install.packages("hBayesDM")
library(hBayesDM)
install.packages("rstan")
library(rstan)
install.packages("bayestestR")
library(bayestestR)
install.packages("ggplot2")
library(ggplot2)
install.packages("reshape2")
library(reshape2)
install.packages("ggpubr")
library(ggpubr)
install.packages("cowplot")
library(cowplot)


source("Utilities_Model_IGT.R")

# args need -> original .csv file, 
args <- commandArgs(trailingOnly = TRUE)



source("Utilities_Model_IGT.R")
Loading.Data("./data/IGT_data_modelling.csv")
Model.Fitting("./data/all_groups.txt", 5000, 2500)
Final.Model(10000, 5000)




## For plotting:
source("Plotting_Group_Differences.R")
ORL.Group.Differences(args[1], args[2], args[3])

