# Driver_Classification.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script runs the Classification part of the SPAD study

## Wipe and recreate the Outputs folder so old files don't interfere with this run
unlink("./Classification/Outputs", recursive = TRUE)
dir.create("./Classification/Outputs", recursive = TRUE)

## Libraries
    library(tidyverse)
    library(caret)
    library(nnet)
    library(broom)
    library(ggplot2)
    library(pROC)

## Paths
    jena = "./Classification/Data/Multinomial_Logistic_Regression_data_jena.csv"
    spad = "./Imputation/Data/SPAD_Imputation.rds"

source("./Classification/Utilities_Classification.R")

Run.Classification(jena, spad)