# Utilities_Model_IGT.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to run the Driver Script for the modelling of the IGT task


## Loads our .csv data and checks that it looks okay
Load.Data <- function(my.file) {

    ## Reads the csv file, 1st row are headings, and separator is ;
    igt_data <- read.csv(my.file, header = T, sep = ";")

    ## Keep only the 4 columns needed to do the modelling
    igt_data <- igt_data[, c("subjID", "choice", "gain", "loss")]

    ## Shows an example and parts of the file to check your data is well formatted
    cat("_________________________________\n\n")
    cat("Is the data well formatted?\n")
    cat(c("Output should be similar to:\n", "'data.frame': 5700 obs. of 4 variables:\n", "$ subjID: chr ...\n",
        "$ choice: int ...\n", "$ gain: int ...\n", "$ loss: int ...\n\n"))
    cat(str(igt_data), "\n")
    cat("_________________________________\n")

    ## hBayesDM requires .txt files, so we convert our .csv to a tabulated .txt file
    write.table(
        igt_data,
        file = "igt_data.txt",
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
        )
}


## This function fits 
VPP.Fit <- function(my.txt.file) {

    cat("________________________\n")
    cat("Starting VPP fit\n\n")

    fit.vpp <- igt_vpp(
        data    = my.txt.file,
        niter   = 2000,
        nwarmup = 1000,
        nchain  = 4,
        ncore   = 4
        )

    pdf("VPP_Fit_Plots.pdf")
    
        plot(fit.vpp, type = "trace")
        plot(fit.vpp)

    dev.off()

    ## All Rhat values should be less or equal than 1.1
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    rhat(fit.vpp)
    cat("\n________________________\n")

    cat("All indices:\n")
    cat(fit.vpp$allIndPars)
    cat("\n________________________\n")

    return(fit.vpp)

}



PVLdelta.Fit <- function(my.txt.file) {

    cat("________________________\n")
    cat("Starting PVL delta fit\n\n")

    fit.PVLdelta <- igt_pvl_delta(
        data = my.txt.file,
        niter = 2000,
        nwarmup = 1000,
        nchain = 4,
        ncore = 4
    )

    pdf("PVLdelta_Fit_Plot.pdf")
    
        plot(fit.PVLdelta, type = "trace")
        plot(fit.PVLdelta)

    dev.off()

    ## All Rhat values should be less or equal than 1.1
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    rhat(PVLdelta)
    cat("\n________________________\n")

    cat("All indices:\n")
    cat(PVLdelta$allIndPars)
    cat("\n________________________\n")
    return(fit.PVLdelta)
}



ORL.Fit <- function(my.txt.file) {

    cat("________________________\n")
    cat("Starting ORL fit\n\n")

    fit.ORL <- igt_orl(
        data    = my.txt.file,
        niter   = 2000,
        nwarmup = 1000,
        nchain  = 4,
        ncore   = 4
)

    pdf("ORL_Fit_Plots.pdf")
    
        plot(fit.ORL, type = "trace")
        plot(fit.ORL)

    dev.off()

    ## All Rhat values should be less or equal than 1.1
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    rhat(fit.ORL)
    cat("\n________________________\n")

    cat("All indices:\n")
    cat(fit.ORL$allIndPars)
    cat("\n________________________\n")

    return(fit.ORL)
}


Fit.Models <- function(my.txt.file) {
    vpp.fit <- VPP.Fit(my.txt.file)

    pvl.fit <- PVLdelta.Fit(my.txt.file)

    orl.fit <- ORL.Fit(my.txt.file)

    printFit(c(vpp.fit, pvl.fit, orl.fit))
}