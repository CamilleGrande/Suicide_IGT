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
imp      <- file.path("./Imputation/Outputs", sprintf("all_imp%d.rds", 1:n.imp))
Jena_imp <- file.path("./Imputation/Outputs", sprintf("Jena_imp%d.rds", 1:n.imp))
 
## Regression() prefixes its outputs (e.g. "All_Cook_Graph.pdf") with whatever prefix we specified,
## so each run's files don't overwrite the other's. The "no outliers" paths below must match that
## same prefix, since Regression() writes to them first, then reads them back in.
no.outliers.path <- function(prefix) {
    file.path("./Regression/Outputs", sprintf("%s_imp_df_without_infl_obs_%d.rds", prefix, 1:n.imp))
}
 
#no.outliers.imp.all             <- no.outliers.path("All")
#no.outliers.imp.suicide.decide  <- no.outliers.path("SUICIDE-DECIDE")

## Adapt arguments as needed
##      imputed.datasets = all imputed datasets (can have as many as needed; minimum is 1). This is changed through paths, not the argument in itself
##      imputed.datasets.no.outliers = DO NOT CHANGE, holds path to dataset with no outliers. Even if you have no outliers, keep it like this
##      cohort = in our case, "all", "SPAD" or "SUICIDE-DECIDE". If "all", will include "site" & "cohort" as IVs. If something else, will drop those variables
##      prefix = prepended to every output file for this run (e.g. "All", "SUICIDE-DECIDE"), so runs don't overwrite each other's outputs
##      group = NULL if whole dataset; otherwise specify the group of interest (in our case, suicide attempters; "SA")
##      DV = the dependent variable (predicted variable)
##      method = if "none", applies normal regression modelling. If specify "backward" or "forward", applies step() regression
##      IV = Independent variables (predictors) 

## ------- All (SPAD + SUICIDE-DECIDE) -------
Regression(imputed.datasets = imp, imputed.datasets.no.outliers = no.outliers.path("All"), cohort = "all", prefix = "All", group = NULL,
    DV = "K", method = "backward", 
    IV = c("age", "sex", "group", "bdi_zscore", "zscore_gonogo_total_omissions", "zscore_gonogo_total_commissions", "flu_verb_p", "flu_verb_ani", "site", "cohort"))



## ------- SUICIDE-DECIDE ONLY -------
Regression(imputed.datasets = Jena_imp, imputed.datasets.no.outliers = no.outliers.path("SUICIDE-DECIDE"), cohort = "SUICIDE-DECIDE", prefix = "SUICIDE-DECIDE", group = "SA", 
    DV = "K", method = "backward", 
    IV = c("age", "sex", "group_lethality", "sis_total", "bdi2_sum", "jena_gonogo_total_omissions", "jena_gonogo_total_commissions", "flu_verb_p", "flu_verb_ani", "site", "cohort"))
