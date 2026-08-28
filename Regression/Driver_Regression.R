# Regression.R
# Author: Lucas De Zorzi & Camille Grandé
# Study: SPAD
# Description:
#       This script runs the correlation/regression part of the SPAD study


## Libraries needed to run this script:
  library(tidyverse)
  library(car)
  library(lmtest)
  library(sandwich)
  library(outliers)
  library(mice)


## Utility functions:
  source("./Regression/Utilities_Regression.R")


## Wipe and recreate the Outputs folder so old .rds files don't interfere with this run
unlink("./Regression/Outputs", recursive = TRUE)
dir.create("./Regression/Outputs", recursive = TRUE)


## Set the number of imputations you have; the vectors below for the paths adapt automatically
n.imp <- 5


## Paths
## If ran imputation script before, should not modify the name of the files as they are created through this other script; otherwise adapt the paths/names
## and script won't run!!
  all.data = "./Imputation/Data/All_Imputation.rds"
  imp = file.path("./Imputation/Outputs", sprintf("imputed_dataset_%d.rds", 1:n.imp))
  no.outliers.path = file.path("./Regression/Outputs", sprintf("imp_df_without_infl_obs_%d.rds", 1:n.imp))
 
## Adapt arguments as needed
##      imputed.datasets = all imputed datasets (can have as many as needed; minimum is 1). This is changed through paths, not the argument in itself
##      imputed.datasets.no.outliers = DO NOT CHANGE, holds path to dataset with no outliers. Even if you have no outliers, keep it like this
##      DV = the dependent variable (predicted variable)
##      method = if "none", applies normal regression modelling. If specify "backward" or "forward", applies step() regression
##      IV = Independent variables (predictors) 

## ------- All (SPAD + SUICIDE-DECIDE) -------
Regression(imputed.datasets = imp, imputed.datasets.no.outliers = no.outliers.path, 
    DV = "K", method = "backward", 
    IV = c("age", "sex", "group", "bdi_zscore", "zscore_gonogo_total_omissions", "zscore_gonogo_total_commissions", "zscore_gonogo_total_correct", "gonogo_mean_rt", "flu_verb_p", "flu_verb_ani", "site", "cohort"))

Regression.Exclude.NA(all.data, DV = "K", method = "backward", 
    IV = c("age", "sex", "group", "bdi_zscore", "zscore_gonogo_total_omissions", "zscore_gonogo_total_commissions", "zscore_gonogo_total_correct", "gonogo_mean_rt", "flu_verb_p", "flu_verb_ani", "site", "cohort"))