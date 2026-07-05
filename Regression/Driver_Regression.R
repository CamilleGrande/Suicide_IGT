# Regression.R
# Author: Lucas De Zorzi & Camille Grandé
# Study: SPAD
# Description:
#       This script runs the correlation/regression part of the SPAD study


## TO DO:
##      commenter arguments
##      Change all 5 and other hardcoded to nb imp / 2 etc
##      France/Allemagne as site
##      SPAD / Suicide_decide as study

source("./Regression/Utilities_Regression.R")

if (!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  # Données
  tidyverse, janitor, naniar, psych,
  # Tableaux
  knitr, kableExtra, gt, gtsummary, broom,
  # Régression & diagnostics
  car, lmtest, lme4, lmerTest, sandwich, MASS, caret,
  # Outliers
  outliers,
  # Visualisation
  ggplot2, patchwork, ggpubr, corrplot, GGally,
  # Taille d'effet & performance
  effectsize, performance, see, 
  # Coefficients standardisés
  lm.beta,
  # Imputation
  mice
)


## Wipe and recreate the Outputs folder so old .rds files don't interfere with this run
unlink("./Regression/Outputs", recursive = TRUE)
dir.create("./Regression/Outputs", recursive = TRUE)


# Thème ggplot global
theme_set(theme_bw(base_size = 12))

# Palette couleurs groupes
col_groupes <- c(
  "HC"   = "#4CAF50",
  "PC"   = "#2196F3",
  "SA"   = "#F44336",
  "NVSA" = "#FF9800",
  "VSA"  = "#C62828"
)


## Set the number of imputations you have; the vectors below for the paths adapt automatically
n.imp <- 5
 
## Paths
## If ran imputation script before, should not modify the name of the files as they are created through this other script; otherwise adapt the paths/names
## If number of imputations is incorrect, will create path to files that don't exist (e.g. 5 imputations but creates path "imp6")
## and script won't run!!
imp             <- file.path("./Imputation/Outputs", sprintf("all_imp%d.rds", 1:n.imp))
Jena_imp        <- file.path("./Imputation/Outputs", sprintf("Jena_imp%d.rds", 1:n.imp))
no.outliers.imp <- file.path("./Regression/Outputs", sprintf("imp_df_without_infl_obs_%d.rds", 1:n.imp))


## Adapt arguments as needed
##      imputed.datasets: all imputed datasets (can have as many as needed; minimum is 1)
##      dataset = in our case, "all", "SPAD" or "SUICIDE-DECIDE". If "all", will include "site" & "cohort" as IVs. If something else, will drop those variables
##      group =
##      DV =
##      

## ------- All (SPAD + SUICIDE-DECIDE) -------
Regression(imputed.datasets = imp, imputed.datasets.no.outliers = no.outliers.imp, cohort = "all", group = NULL, DV = "K", method = "backward", 
    IV = c("age", "sex", "group", "bdi_zscore", "zscore_gonogo_total_omissions", "zscore_gonogo_total_commissions", "flu_verb_p", "flu_verb_ani", "site", "cohort"))



## ------- SUICIDE-DECIDE ONLY -------
Regression(imputed.datasets = Jena_imp, imputed.datasets.no.outliers = no.outliers.imp, cohort = "SUICIDE-DECIDE", group = "SA", DV = "K", method = "backward", 
    IV = c("age", "sex", "group_lethality", "sis_total", "bdi_zscore", "zscore_gonogo_total_omissions", "zscore_gonogo_total_commissions", "flu_verb_p", "flu_verb_ani", "site", "cohort"))
