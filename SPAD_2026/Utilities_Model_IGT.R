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

        ## Write .txt file, to be found in ./1-data directory
        write.table(        
            data_group,        
            file = paste0("./1-data/group_", i, ".txt"),        
            sep = "\t",        
            row.names = FALSE,        
            quote = FALSE)
    }
}

## This function loads our data, and creates the main .txt file
## and .txt files for each group (for the final modeling part)
## Usage example: Loading.Data("./1-data/IGT_data_modelling.csv")
Loading.Data <- function(file) {

    ## Checks files do not already exist and .csv original file present
    if (file.exists("./1-data/all_groups.txt") & file.exists("./1-data/group_0.txt")) {
        return(cat("\nData is already loaded, moving on to the next step\n"))
    }
    if (!file.exists(file)) {
        return(cat("\nCannot find .csv file. Please make sure the data .csv file in in the data folder\n"))
    }

    ## Reads file and keep only relevant columns
    ## Make sure columns are correctly named in .csv file, or it will not work
    ## These names are a prerequisite for the modelling part too in hBayesDM
    igt_data <- read.csv(file, header = T)
    all_groups <- igt_data[, c("subjID", "choice", "gain", "loss")]
    
    ## Check your data :)
    ## You should have 100 trials for each participant!!!
    cat("_________________________________\n\n")
    cat("Is the data well formatted?\n")
    cat(c("Output should be similar to:\n", "'data.frame': 5700 obs. of 4 variables:\n", "$ subjID: chr ...\n",
        "$ choice: int ...\n", "$ gain: int ...\n", "$ loss: int ...\n\n"))
    cat(str(all_groups), "\n")
    cat("_________________________________\n\n")

    ## Writes .txt file, to be found in ./1-data directory
    write.table(
        all_groups,
        file = "./1-data/all_groups.txt",
        sep = "\t",
        row.names = FALSE,
        quote = FALSE)

    ## Writes .txt files per group
    Table.Per.Group(igt_data)
}


## This function fits a given model
## The model has to be precised in the first argument
## It is incorporated in Model fitting and Final fitting functions
Model.Fitting <- function(igt.model, my.txt.file, my.iterations, my.warmups) {

    ## Adapt number of iterations and warmup as needed in command line arguments
    my.model <- igt.model(
        data = my.txt.file,
        niter = my.iterations,
        nwarmup = my.warmups,
        nchain = 4,
        ncore = 4)

    return(my.model)
}


## This function gives Rhat values for a given model
## It is incorporated in Model fitting and Final fitting functions
my.rhat <- function(my.model, directory, model.name) {
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    write.csv(rhat(my.model), paste0(directory, model.name, ".csv"))
    print(rhat(my.model))
}


## This function creates a trace plot to check our model's chain convergence
## It is incorporated in Model fitting and Final fitting functions
Fitting.Plots <- function(my.model, directory, model.name) {
    trace.plot <- plot(my.model, type = "trace", fontSize=11)
    ggsave(filename= paste0(directory, model.name, "_Trace.pdf"), plot = trace.plot)
}


## This function saves our model's group level indices in a .csv file
## It is incorporated in Model fitting and Final fitting functions
## allIndPars: Summary of individual subjects’ parameters (default: mean). 
## Users can also choose to use median or mode (e.g., output1 = gng_m1("example", indPars="mode") ).
Save.Indices <- function(my.model, directory, model.name) {
    cat("Indices per participant can be found in a .csv file in the 2-model_fitting directory\n")
    write.csv(my.model$allIndPars, paste0(directory, model.name, "_allIndPars.csv"))
    cat("\n\n________________________\n")
}


## This function saves our model's individual fit indices in a .csv file
## It is incorporated in the Final fitting function
## parVals: Posterior samples of all parameters. Extracted by rstan::extract(rstan_object, permuted=T). 
## Note that hyper (group) mean parameters are indicated by mu_PARAMETER (e.g., mu_xi, mu_ep, mu_rho).
Save.parVals <- function(my.model, directory, model.name) {
    cat("All parameter values per participant can be found in a .csv file in the 4-final_model directory\n")
    write.csv(my.model$parVals, paste0(directory, model.name, "_parVals.csv"))
    cat("\n\n________________________\n")
}


Save.RDS <- function(my.model, directory, model.name) {
    cat("Saving .RDS file for model values in 3-model_comparison directory\n")
    saveRDS(my.model, file = paste0(directory, model.name, ".rds"))
    cat("\n\n________________________\n")
}


## This function fits all our different models
## Usage example: Model.Comparison("./1-data/all_groups.txt", 4000, 2000)
Model.Comparison <- function(my.txt.file, my.iterations, my.warmups) {

    cat("\n\n________________________\n")
    cat("Starting PVL delta fit\n\n")

    fit.PVLdelta <- Model.Fitting(igt_pvl_delta, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.PVLdelta, "./2-model_fitting/rhat_", "PVLdelta") 
    Fitting.Plots(fit.PVLdelta, "./2-model_fitting/Plot_", "PVLdelta")
    Save.Indices(fit.PVLdelta, "./2-model_fitting/", "PVLdelta")
    Save.RDS(fit.PVLdelta, "./3-model_comparison/", "PVLdelta")


    cat("\n\n________________________\n")
    cat("Starting PVL decay fit\n\n")

    fit.PVLdecay <- Model.Fitting(igt_pvl_decay, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.PVLdecay, "./2-model_fitting/rhat_", "PVLdecay")
    Fitting.Plots(fit.PVLdecay, "./2-model_fitting/Plot_", "PVLdecay")
    Save.Indices(fit.PVLdecay, "./2-model_fitting/", "PVLdecay")
    Save.RDS(fit.PVLdecay, "./3-model_comparison/", "PVLdecay")


    cat("\n\n________________________\n")
    cat("Starting ORL fit\n\n")

    fit.ORL <- Model.Fitting(igt_orl, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.ORL, "./2-model_fitting/rhat_", "ORL")
    Fitting.Plots(fit.ORL, "./2-model_fitting/Plot_", "ORL")
    Save.Indices(fit.ORL, "./2-model_fitting/", "ORL")
    Save.RDS(fit.ORL, "./3-model_comparison/", "ORL")

    cat("\n\nModel comparison values (LOOIC and values)\nLower values indicate better model performance\n\n")
    write.csv(printFit(fit.PVLdelta, fit.PVLdecay, fit.ORL, ic="both"), "3-model_comparison/printFit_output.csv")
    print(printFit(fit.PVLdelta, fit.PVLdecay, fit.ORL, ic="both"))
    return(printFit(fit.PVLdelta, fit.PVLdecay, fit.ORL, ic="both"))
}

Comparison <- function() {
    log_lik_1 <- extract_log_lik(fit.PVLdelta$fit)
    loo_1 <- loo(log_lik_1)
    log_lik_2 <- extract_log_lik(fit.PVLdecay$fit)
    loo_2 <- loo(log_lik_2)
    log_lik_3 <- extract_log_lik(fit.ORL$fit)
    loo_3 <- loo(log_lik_3)

    saveRDS(fit, file = "fit.rds")
    fit <- readRDS("fit.rds")
}




## This function fits our final model
## Here, the winning model is the ORL model. Adapt code accordingly to your winning model
Final.Model <- function(my.iterations, my.warmups) {

    cat("\n\n________________________\n")
    cat("Starting ORL final fit\n\n")

    ORL.HC <- Model.Fitting(igt_orl, "./1-data/group_0.txt", my.iterations, my.warmups)
    my.rhat(ORL.HC, "./4-final_model/rhat_", "ORL.HC")
    Fitting.Plots(ORL.HC, "./4-final_model/Plot_", "ORL.HC")
    Save.Indices(ORL.HC, "./4-final_model/", "ORL.HC")
    Save.parVals(ORL.HC, "./4-final_model/", "ORL.HC")

    ORL.PC <- Model.Fitting(igt_orl, "./1-data/group_1.txt", my.iterations, my.warmups)
    my.rhat(ORL.PC, "./4-final_model/rhat_", "ORL.PC")
    Fitting.Plots(ORL.PC, "./4-final_model/Plot_", "ORL.PC")
    Save.Indices(ORL.PC, "./4-final_model/", "ORL.PC")
    Save.parVals(ORL.PC, "./4-final_model/", "ORL.PC")

    ORL.SA <- Model.Fitting(igt_orl, "./1-data/group_2.txt", my.iterations, my.warmups)
    my.rhat(ORL.SA, "./4-final_model/rhat_", "ORL.SA")
    Fitting.Plots(ORL.SA, "./4-final_model/Plot_", "ORL.SA")
    Save.Indices(ORL.SA, "./4-final_model/", "ORL.SA")
    Save.parVals(ORL.SA, "./4-final_model/", "ORL.SA")
}



