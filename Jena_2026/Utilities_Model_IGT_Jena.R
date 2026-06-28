# Utilities_Model_IGT_Jena.R
# Author: Camille Grandé
# Study: SPAD (Jena)
# Description: 
#       This script holds the utility functions necessary to run the Driver Script for the modelling of the IGT task in the Jena data

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

    for (i in unique(my.data$Group_by_suicide)) {

        ## Extract data of the group, and keep only relevant columns
        data_group <- my.data[my.data$Group_by_suicide == i,]
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
    igt_data <- read.csv(file, header = T, sep = ";")
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
    write.csv(rhat(my.model), paste0(directory, model.name, ".csv"), row.names = F)
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
Save.Indices <- function(my.model, directory, model.name) {
    cat("Indices per participant can be found in a .csv file in the 2-model_fitting directory\n")
    write.csv(my.model$all_ind_pars, paste0(directory, model.name, "_all_ind_pars.csv"), row.names = F)
    cat("\n\n________________________\n")
}


## This function saves our model's individual fit indices in a .csv file
## It is incorporated in the Final fitting function
Save.parVals <- function(my.model, directory, model.name) {
    cat("All parameter values per participant can be found in a .csv file in the 4-final_model directory\n")
    write.csv(my.model$par_vals, paste0(directory, model.name, "_par_vals.csv"), row.names = F)
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


    cat("\n\n________________________\n")
    cat("Starting PVL decay fit\n\n")

    fit.PVLdecay <- Model.Fitting(igt_pvl_decay, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.PVLdecay, "./2-model_fitting/rhat_", "PVLdecay")
    Fitting.Plots(fit.PVLdecay, "./2-model_fitting/Plot_", "PVLdecay")
    Save.Indices(fit.PVLdecay, "./2-model_fitting/", "PVLdecay")


    cat("\n\n________________________\n")
    cat("Starting ORL fit\n\n")

    fit.ORL <- Model.Fitting(igt_orl, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.ORL, "./2-model_fitting/rhat_", "ORL")
    Fitting.Plots(fit.ORL, "./2-model_fitting/Plot_", "ORL")
    Save.Indices(fit.ORL, "./2-model_fitting/", "ORL")

    cat("\n\nModel comparison values (LOOIC and WAIC)\nLower values indicate better model performance\n\n")
    write.csv(print_fit(fit.PVLdelta, fit.PVLdecay, fit.ORL, ic="both"), "3-model_comparison/printFit_output.csv", row.names = F)
    print(print_fit(fit.PVLdelta, fit.PVLdecay, fit.ORL, ic="both"))
    return(print_fit(fit.PVLdelta, fit.PVLdecay, fit.ORL, ic="both"))
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

    ORL.NVSA <- Model.Fitting(igt_orl, "./1-data/group_NVSA.txt", my.iterations, my.warmups)
    my.rhat(ORL.NVSA, "./4-final_model/rhat_", "ORL.NVSA")
    Fitting.Plots(ORL.NVSA, "./4-final_model/Plot_", "ORL.NVSA")
    Save.Indices(ORL.NVSA, "./4-final_model/", "ORL.NVSA")
    Save.parVals(ORL.NVSA, "./4-final_model/", "ORL.NVSA")

    ORL.VSA <- Model.Fitting(igt_orl, "./1-data/group_VSA.txt", my.iterations, my.warmups)
    my.rhat(ORL.VSA, "./4-final_model/rhat_", "ORL.VSA")
    Fitting.Plots(ORL.VSA, "./4-final_model/Plot_", "ORL.VSA")
    Save.Indices(ORL.VSA, "./4-final_model/", "ORL.VSA")
    Save.parVals(ORL.VSA, "./4-final_model/", "ORL.VSA")
}