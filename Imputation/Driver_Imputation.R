## Driver_Imputation.R
## Author: Camille Grandé
## Description:
##      This script runs the functions to impute the missing data (where applicable)
##      in the SPAD and Jena datasets


## Paths to .rds (change as needed)
Jena = "./Imputation/Data/Jena_Imputation.rds"
SPAD = "./Imputation/Data/SPAD_Imputation.rds"

## Libraries needed to run the script
library(tidyverse)
library(mice)

## Utility file with functions
source("./Imputation/Utilities_Imputation.R")


## ---------- SPAD ----------

## Verbal fluency – phonemic
My.Imputation(SPAD, "SPAD_", "flu_verb_p", "flu_verb_p_imp", 
    "group", "age", "sex", "mmse_total", "nart_corr", "ssi_total")

## Verbal fluency – categorical
My.Imputation(SPAD, "SPAD_", "flu_verb_ani", "flu_verb_ani_imp", 
    "group", "age", "sex", "mmse_total", "nart_corr", "ssi_total")
    
## Go / No go – total correct
My.Imputation(SPAD, "SPAD_", "zscore_gonogo_total_correct", "zscore_gonogo_total_correct_imp", 
    "group", "age", "sex", "mmse_total", "nart_corr", "ssi_total")

## Go / No go – total omissions
My.Imputation(SPAD, "SPAD_", "zscore_gonogo_total_omissions", "zscore_gonogo_total_omissions_imp", 
    "group", "age", "sex", "mmse_total", "nart_corr", "ssi_total")

## Go / No go – total commissions
My.Imputation(SPAD, "SPAD_", "zscore_gonogo_total_commissions", "zscore_gonogo_total_commissions_imp", 
    "group", "age", "sex", "mmse_total", "nart_corr", "ssi_total")


## ---------- Jena ----------

## SIS Total score
My.Imputation(Jena, "Jena_", "sis_total", "sis_total_imp", by.group = T, my.group = "SA", 
    "group", "group_lethality", "age", "sex", "sis_total", "bdi2_sum", "flu_verb_p", "flu_verb_ani",
    "jena_gonogo_total_omissions", "jena_gonogo_total_commissions", "gonogo_mean_rt", 
    "jena_gonogo_total_correct", "university", "school_years_num")




## ---------- Make new datasets for regression ----------
Make.Imputed.Datasets(SPAD, "SPAD")
Make.Imputed.Datasets(Jena, "Jena")

Save.All.Datasets("SPAD", "JENA")