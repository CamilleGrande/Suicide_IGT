# Utilities_Model_IGT.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to run the Driver Script for the modelling of the IGT task



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


## This function fits a given model
## The model has to be precised in the first argument
##
## Usage example: Models(igt_pvl_delta, "./data/all_groups.txt")
Model.Fitting <- function(igt.model, my.txt.file) {

    my.model <- igt.model(
        data = my.txt.file,
        niter = 1000,
        nwarmup = 500,
        nchain = 4,
        ncore = 4
    )

    return(my.model)
}

my.rhat <- function(my.model) {
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    return(rhat(my.model))
}


My.Models <- function(my.txt.file) {

    cat("________________________\n")
    cat("Starting PVL delta fit\n\n")

    fit.PVLdelta <- Model.Fitting(igt_pvl_delta, "./data/all_groups.txt")
    
    my.rhat(fit.PVLdelta)

    cat("________________________\n")
    cat("Starting PVL decay fit\n\n")

    fit.PVLdecay <- igt_pvl_decay(
        data = my.txt.file,
        niter = 1000,
        nwarmup = 500,
        nchain = 4,
        ncore = 4
    )

    cat("________________________\n")
    cat("Starting ORL fit\n\n")

    fit.ORL <- igt_orl(
        data    = my.txt.file,
        niter   = 1000,
        nwarmup = 500,
        nchain  = 4,
        ncore   = 4
    )



}





## PVLdelta.Fit("./data/all_groups.txt")
PVLdelta.Fit <- function(my.txt.file) {

    

    trace.plot <- plot(fit.PVLdelta, type = "trace", inc_warmup=T, fontSize=11)
    parameter.plot <- plot(fit.PVLdelta)
    
    all.plots <- ggarrange(trace.plot, parameter.plot, ncol=1, nrow=6)
    ggsave(filename="./model_comparison/PVLdelta_Fit_Plots.pdf", plot = all.plots)

    ## All Rhat values should be less or equal than 1.1
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    Rhat(fit.PVLdelta)
    cat("\n________________________\n")

    cat("All indices can be found in a .csv file\n")
    write.csv(fit.PVLdelta$allIndPars, "./model_comparison/PVLdelta_allIndPars.csv")
    cat("\n________________________\n")

    return(fit.PVLdelta)
}

## PVLdecay.Fit("./data/all_groups.txt")
PVLdecay.Fit <- function(my.txt.file) {
    
    pdf("./model_comparison/PVLdecay_Fit_Plots.pdf")
        trace.plot <- plot(fit.PVLdecay, type = "trace", inc_warmup=T, fontSize=11)
        parameter.plot <- plot(fit.PVLdecay)
        multiplot(trace.plot, parameter.plot)
    dev.off()


     ## All Rhat values should be less or equal than 1.1
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    rhat(fit.PVLdecay)
    cat("\n________________________\n")

    cat("All indices can be found in a .csv file\n")
    write.csv(fit.PVLdecay$allIndPars, "./model_comparison/PVLdecay_allIndPars.csv")
    cat("\n________________________\n")
}









