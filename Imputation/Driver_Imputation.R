## Driver_Imputation.R
## Author: Camille Grandé
## Description:
##      This script runs the functions to impute the missing data (where applicable)
##      in the SPAD and Jena datasets


## Paths to .rds (change as needed)
SUICIDEDECIDE = "./Imputation/Data/Jena_Imputation.rds"
SPAD = "./Imputation/Data/SPAD_Imputation.rds"

## Libraries needed to run the script
library(tidyverse)
library(mice)

## Wipe and recreate the Outputs folder so old files don't interfere with this run
unlink("./Imputation/Outputs", recursive = TRUE)
dir.create("./Imputation/Outputs", recursive = TRUE)

## Utility file with functions
source("./Imputation/Utilities_Imputation.R")


## ---------- SPAD ----------

Run.Imputation(SPAD, SUICIDEDECIDE, c(
        ## vars to impute
        "flu_verb_p", 
        "flu_verb_ani", 
        "zscore_gonogo_total_correct", 
        "zscore_gonogo_total_omissions", 
        "zscore_gonogo_total_commissions", 
        "gonogo_mean_rt"))