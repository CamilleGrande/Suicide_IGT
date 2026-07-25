# Driver_Behavior.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the code necessary to run the 
#       behavioral scores stats part of the SPAD study


## Libraries needed to run the script:
library(tidyverse)
library(ggplot2)
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
    SPAD.Total.Scores = "./Behavior/Outputs/Subject_level_total_scores_SPAD.csv"
    Jena.Total.Scores = "./Behavior/Outputs/Subject_level_total_scores_Jena.csv"
    SPAD.Block.Scores = "./Behavior/Outputs/Subject_level_block_scores_SPAD.csv"
    Jena.Block.Scores =  "./Behavior/Outputs/Subject_level_block_scores_Jena.csv"


## ---- SPAD ----
Behavior.IGT(IGT.SPAD, my.group = Group, "_SPAD")
Plot.Choice.Prop.Block(IGT.SPAD, my.group = Group, group.labels = c("0" = "Healthy controls", "1" = "Patient controls", "2" = "Suicide attempters"), 
    "_SPAD", my.width = 15)

IGT.All.Differences(SPAD.Total.Scores, vars = c("net_total", "A", "B", "C", "D"), my.group = "Group", "_SPAD")
IGT.Differences.Per.Block(SPAD.Block.Scores, vars = "net_block", my.group = "Group", cohort = "_SPAD")


## ---- SUICIDE-DECIDE ----
Behavior.IGT(IGT.Jena, my.group = Group_by_suicide, "_Jena")
Plot.Choice.Prop.Block(IGT.Jena, my.group = Group_by_suicide, group.labels = c("0" = "Healthy controls", "1" = "Patient controls", "NVSA" = "Non violent attempters", "VSA" = "Violent attempters"),
    "_Jena", my.width = 10)

IGT.All.Differences(Jena.Total.Scores, vars = c("net_total", "A", "B", "C", "D"), my.group = "Group_by_suicide", "_Jena")
IGT.Differences.Per.Block(Jena.Block.Scores, vars = "net_block", my.group = "Group_by_suicide", cohort = "_Jena")