# Driver_SPAD.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script runs the SPAD analysis, using functions found in "Utilities_Model_IGT.R". 
#       Install the packages you don't have yet by de-commenting them (removing the hashtag)
#       When running your own analysis, do not run the Final.Model line. Stop after the model comparison,
#       see which model fits your data best, then choose your Final Model (here ORL)
#
# Usage example: Rscript Driver_SPAD.R "./1-data/IGT_data_modelling.csv" 5000 2500 10000 5000
#       1st arg = .csv file
#       2nd arg = model comparison iterations
#       3rd arg = model comparison warmups
#       4th arg = final model iterations
#       5th arg = final model warmups


## To do:
##      Tidy code
##      Save as rds not csv?


## Our command line arguments!
args <- commandArgs(trailingOnly = TRUE)
csv_file <- args[1]
iter_warmp <- as.numeric(args[2:5])

## Defensive code
## Makes sure we have the right number and type of arguments, and if not, gives a usage example
if (length(args) < 5) {
    cat(paste0("Please specify a file name and the number of iterations and warmups for the model comparison and final fit 
        (Usage example: Rscript Driver_SPAD.R", " ./data/IGT_data_modelling.csv ", "5000 2500 10000 5000)\n"))
    q()
}
if (length(args) > 5) {
    cat(paste0("Please use no more than five arguments: one for a file name, one for model comparison iterations, one for model comparison warmups,
        one for final model iterations, and one for final model warmups
        (Usage example: Rscript Driver_SPAD.R", " ./data/IGT_data_modelling.csv ", "5000 2500 10000 5000)\n"))
    q()
}
if (!endsWith(csv_file, ".csv")) {
    cat("The file (first argument) should be a .csv file\n")
    q()
}
if (is.numeric(iter_warmp) != T) {
    cat(paste0("The second, third, fourth and fifth arguments should be numeric (number of iterations and warmups)\n",
        "(Usage example: Rscript Driver_SPAD.R", " ./data/IGT_data_modelling.csv ", "5000 2500 10000 5000)\n"))
    q()
} else {
    cat("Processing data from file:", csv_file, "\nwith desired iterations and warmups:", "\nModel comparison iterations:", iter_warmp[1], 
        "\nModel comparison warmups:", iter_warmp[2], "\nFinal Model iterations:", iter_warmp[3], "\nFinal Model warmups:", iter_warmp[4])
}

###### Need to test those I can remove; + should I remove rstan and loo?
## Remove the hashtag for the packages you need to install :)
#install.packages("dplyr", repos='https://cloud.r-project.org/')
library(dplyr)
#install.packages("hBayesDM", repos='https://cloud.r-project.org/')
library(hBayesDM)
#install.packages("bayestestR", repos='https://cloud.r-project.org/')
library(bayestestR)
#install.packages("reshape2", repos='https://cloud.r-project.org/')
library(reshape2) #function melt
#install.packages("ggpubr", repos='https://cloud.r-project.org/')
library(ggpubr) #ggarrange function
#install.packages("cowplot", repos='https://cloud.r-project.org/')
library(cowplot) 
#install.packages("gridExtra", repos='https://cloud.r-project.org/')
library(gridExtra) #didnt
#install.packages("ggh4x", repos='https://cloud.r-project.org/')
library(ggh4x)
#install.packages("loo", repos='https://cloud.r-project.org/')
library(loo)
library(posterior)


## Modelling part
source("Utilities_Model_IGT.R")
source("Plotting_Utilities.R")

Loading.Data(csv_file)
Model.Comparison("./1-data/all_groups.txt", iter_warmp[1], iter_warmp[2]) ## 5000 and 2500
Final.Model(iter_warmp[3], iter_warmp[4]) ## 10000 and 5000

Plot.Groups("./4-final_model/ORL.HC_parVals.csv", "./4-final_model/ORL.PC_parVals.csv", "./4-final_model/ORL.SA_parVals.csv")
Plot.HDI("./4-final_model/ORL.HC_parVals.csv", "./4-final_model/ORL.PC_parVals.csv", "./4-final_model/ORL.SA_parVals.csv")


#install.packages("rstan")
#library(rstan)
#install.packages("loo")
#library(loo)