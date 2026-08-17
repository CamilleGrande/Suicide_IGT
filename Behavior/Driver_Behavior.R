# Driver_Behavior.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the code necessary to run the 
#       behavioral scores stats part of the SPAD study


## Libraries needed to run the script:
library(tidyverse)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(paletteer)


## Contains the functions used in this script; will not run without
source("./Behavior/Utilities_Behavior.R")
source("./Behavior/Plotting_Behavior.R")

## ---- Paths to data ----

## Everything is set to be run from the main directory
## Change those paths if needed, not the functions :)
setwd(".")
    IGT.SPAD = "./SPAD_2026/1-data/IGT_data_modelling.csv"
    IGT.Jena = "./Jena_2026/1-data/Jena_data.csv"


## ---- SPAD ----
Descriptives.IGT(IGT.SPAD, "_SPAD")
Group.Diff.IGT(IGT.SPAD, "_SPAD")
Plot.IGT(IGT.SPAD, "_SPAD", my.width = 15)


## ---- SUICIDE-DECIDE ----
Descriptives.IGT(IGT.Jena, "_SUICIDEDECIDE")
Group.Diff.IGT(IGT.Jena, "_SUICIDEDECIDE")
Plot.IGT(IGT.Jena, "_SUICIDEDECIDE", my.width = 10)