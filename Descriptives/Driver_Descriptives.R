# Driver_Descriptives.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the code necessary to run the 
#       descriptive stats part of the SPAD study


## TO DO:
##          Descriptives ptcp each group Jena / SPAD 
##          Demographics
##          Clinical 
##          Cognition?
##          IGT Behavioral scores
##          Change path SPAD and SUICIDE DECIDE modelling so can run everything from root SPAD


## Libraries needed to run the script:
library(tidyverse)
library(ggplot2)
library(paletteer)
library(e1071)
library(corrplot)
library(RColorBrewer)


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


All.Numeric(IGT.SPAD, Descriptives.SPAD, "./Descriptives/Outputs/", c("Age", "MMSE_Total", "NART_Corr", "SIS_Total", "BDI13_Total"), "_SPAD.pdf", "_SPAD.csv", group.var = "Group")
All.Frequencies(IGT.SPAD, Descriptives.SPAD, "./Descriptives/Outputs/", c("Sex", "Group"), "_SPAD.pdf", "_SPAD.csv", group.var = "Group" )

All.Numeric(IGT.Jena, Descriptives.Jena, "./Descriptives/Outputs/", c("Age", "SIS_Total", "BDI2_sum"), "_Jena.pdf", "_Jena.csv", group.var = "Group")
All.Frequencies(IGT.Jena, Descriptives.Jena, "./Descriptives/Outputs/", c("Sex", "Group", "GroupSA"), "_Jena.pdf", "_Jena.csv", group.var = "Group" )