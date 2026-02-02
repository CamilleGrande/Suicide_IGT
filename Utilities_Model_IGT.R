# Utilities_Model_IGT.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to run the Driver Script for the modelling of the IGT task


fit.ORL <- function() {


    fit.ORL.group0 <- igt_orl(        
        data    = "igt_data_group0.txt",        
        niter   = 10000,        
        nwarmup = 2000,        
        nchain  = 4,        
        ncore   = 4        
        )
    
    fit.ORL.group1 <- igt_orl(        
        data    = "igt_data_group1.txt",        
        niter   = 10000,        
        nwarmup = 2000,        
        nchain  = 4,        
        ncore   = 4        
        )

    fit.ORL.group2 <- igt_orl(        
        data    = "igt_data_group2.txt",        
        niter   = 10000,        
        nwarmup = 2000,        
        nchain  = 4,        
        ncore   = 4        
        )

    write.csv(fit.ORL.group0$allIndPars, "./Group0_allIndPars.csv")


    df <- as.data.frame(fit.ORL.group0$fit)
    str(df)    
    write.csv(df, "./Group0_fit.csv")


    write.csv(fit.ORL.group1$allIndPars, "./Group1_allIndPars.csv")
    write.csv(fit.ORL.group1$fit, "./Group1_fit.csv")
    write.csv(fit.ORL.group2$allIndPars, "./Group2_allIndPars.csv")
    write.csv(fit.ORL.group2$fit, "./Group2_fit.csv")

    return(c(fit.ORL.group0$allIndPars, fit.ORL.group0$fit, fit.ORL.group1$allIndPars, fit.ORL.group1$fit, fit.ORL.group2$allIndPars, fit.ORL.group2$fit,))
}


## This function creates the tables for each group in our main file
## It is incorporated in the data loading function
Table.Per.Group <- function(my.data) {

    ## Create a .txt file for each group's data 
    ## These files will be used in our final model
    for (i in unique(my.data$Group)) {

        ## Extract data of the group, and keep only relevant columns
        data_group <- my.data[my.data$Group == i,]
        data_group <- data_group[, c("subjID", "choice", "gain", "loss")]

        ## Write .txt file, to be found in ./data directory
        write.table(        
            data_group,        
            file = paste0("./data/group_", i, ".txt"),        
            sep = "\t",        
            row.names = FALSE,        
            quote = FALSE        
            )

    }
}

## This function loads our data, and creates the main .txt file
## and .txt files for each group (for the final modeling part)
##
## Usage example: Loading.Data("./data/IGT_data_modelling.csv")
Loading.Data <- function(file) {

    ## Checks files do not already exist and .csv original file present
    if (file.exists("./data/all_groups.txt") & file.exists("./data/group_0.txt")) {
        return(cat("\nData is already loaded, moving on to the next step\n"))
    }
    if (!file.exists(file)) {
        return(cat("\nCannot find .csv file. Please make sure the data .csv file in in the data folder\n"))
    }

    ## Reads file and keep only relevant columns
    ## Make sure columns are correctly named in .csv file, or it will not work
    ## These names are a prerequisite for the modelling part too
    igt_data <- read.csv(file, header = T, sep = ";")
    all_groups <- igt_data[, c("subjID", "choice", "gain", "loss")]
    
    ## Check your data :)
    cat("_________________________________\n\n")
    cat("Is the data well formatted?\n")
    cat(c("Output should be similar to:\n", "'data.frame': 5700 obs. of 4 variables:\n", "$ subjID: chr ...\n",
        "$ choice: int ...\n", "$ gain: int ...\n", "$ loss: int ...\n\n"))
    cat(str(all_groups), "\n")
    cat("_________________________________\n")

    ## Write .txt file, to be found in ./data directory
    write.table(
        all_groups,
        file = "./data/all_groups.txt",
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
        )

    ## Writes .txt files per group
    Table.Per.Group(igt_data)

}


  