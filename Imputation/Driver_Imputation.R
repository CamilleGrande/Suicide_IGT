## Driver_Imputation.R
## Author: Camille Grandé
## Description:
##      This script runs the functions to impute the missing data (where applicable)
##      in the SPAD and Jena datasets


## Paths to .rds (change as needed)
Jena = "./Imputation/Jena_Imputation.rds"
SPAD = "./Imputation/SPAD_Imputation.rds"

## Libraries needed to run the script
library(tidyverse)
library(mice)

## Utility file with functions
source("./Imputation/Utilities_Imputation.R")


## ---------- SPAD ----------



## ---------- Jena ----------
My.Imputation(Jena, "sis_total", "sis_total_imp", by.group = T, my.group = "SA", "group", "age", "sex")