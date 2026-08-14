# Driver_Descriptives.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the code necessary to run the 
#       descriptive stats part of the SPAD study


## Libraries needed to run the script:
library(tidyverse)
library(parameters)
library(rstatix)
## if want to plot:
#library(ggplot2)
#library(paletteer)
#library(e1071)
#library(corrplot)
#library(RColorBrewer)


## Contains the functions used in this script; will not run without
source("./Descriptives/Plotting_Utilities.R")
source("./Descriptives/Utilities_Descriptives.R")


## ---- Paths to data ----

## Everything is set to be run from the main directory
## Change those paths if needed, not the functions :)
setwd(".")
    IGT.SPAD = "./SPAD_2026/1-data/all_groups.txt"
    Descriptives.SPAD = "./SPAD_2026/All_SPAD.csv"
    IGT.Jena =  "./Jena_2026/1-data/all_groups.txt"
    Descriptives.Jena = "./Jena_raw_data/All_Jena.csv"
    ## To compute descriptives for regression, use ./Imputation/Data csv


## ------- SPAD -------
SPAD.Descriptives(IGT.SPAD, Descriptives.SPAD)


## ------- SUICIDE-DECIDE -------
SUICIDEDECIDE.Descriptives(IGT.Jena, Descriptives.Jena)