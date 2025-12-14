# Model_IGT.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script runs the modelling part of the SPAD analysis

library(hBayesDM) # install if not installed. Mine is version 1.2.1
# install.packages("hBayesDM")
# library(hBayesDM) # put in if statement

source("Utilities_Model_IGT.R")

args <- commandArgs(trailingOnly = TRUE)

Load.Data(args[1])


## model all models one by one here, then put call to all of them in one function + fit infos to not use global variables here
#vpp.fit <- VPP.Modelling("igt_data.txt")
#vpp.fit$fit

ORL.Fit <- ORL.Fit("igt_data.txt")
ORL.Fit$fit


