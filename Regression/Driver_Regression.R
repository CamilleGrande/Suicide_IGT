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


# Paths
# Adapt them depending on number of imputation files you have
imp1 = "./Imputation/Outputs/all_imp1.rds"
imp2 = "./Imputation/Outputs/all_imp2.rds"
imp3 = "./Imputation/Outputs/all_imp3.rds"
imp4 = "./Imputation/Outputs/all_imp4.rds"
imp5 = "./Imputation/Outputs/all_imp5.rds"

Jena_imp1 = "./Imputation/Outputs/Jena_imp1.rds"
Jena_imp2 = "./Imputation/Outputs/Jena_imp2.rds"
Jena_imp3 = "./Imputation/Outputs/Jena_imp3.rds"
Jena_imp4 = "./Imputation/Outputs/Jena_imp4.rds"
Jena_imp5 = "./Imputation/Outputs/Jena_imp5.rds"

no.outliers.imp1 = "./Regression/Outputs/imp_df_without_infl_obs_1.rds"
no.outliers.imp2 = "./Regression/Outputs/imp_df_without_infl_obs_2.rds"
no.outliers.imp3 = "./Regression/Outputs/imp_df_without_infl_obs_3.rds"
no.outliers.imp4 = "./Regression/Outputs/imp_df_without_infl_obs_4.rds"
no.outliers.imp5 = "./Regression/Outputs/imp_df_without_infl_obs_5.rds"


## Adapt arguments as needed
##      imputed.datasets: all imputed datasets (can have as many as needed; minimum is 1)
##      dataset = in our case, "all", "SPAD" or "SUICIDE-DECIDE". If "all", will include "site" & "cohort" as IVs. If something else, will drop those variables
##      group =
##      DV =
##      
Regression(imputed.datasets = c(imp1, imp2, imp3, imp4, imp5), imputed.datasets.no.outliers = c(no.outliers.imp1, no.outliers.imp2, no.outliers.imp3, no.outliers.imp4, no.outliers.imp5),
    cohort = "all", group = NULL, DV = "K", method = "backward", 
    IV = c("age", "sex", "group", "bdi_zscore", "zscore_gonogo_total_omissions", "zscore_gonogo_total_commissions", "flu_verb_p", "flu_verb_ani", "site", "cohort"))

Regression(imputed.datasets = c(Jena_imp1, Jena_imp2, Jena_imp3, Jena_imp4, Jena_imp5), imputed.datasets.no.outliers = c(no.outliers.imp1, no.outliers.imp2, no.outliers.imp3, no.outliers.imp4, no.outliers.imp5),
    cohort = "SUICIDE-DECIDE", group = "SA", DV = "K", method = "backward", 
    IV = c("age", "sex", "group_lethality", "sis_total", "bdi_zscore", "zscore_gonogo_total_omissions", "zscore_gonogo_total_commissions", "flu_verb_p", "flu_verb_ani", "site", "cohort"))
