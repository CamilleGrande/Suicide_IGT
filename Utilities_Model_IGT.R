# Utilities_Model_IGT.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to run the Driver Script for the modelling of the IGT task


fit.ORL <- function() {
    fit.ORL.group0 <- igt_orl(        
        data    = "igt_data_group0.txt",        
        niter   = 6000,        
        nwarmup = 2000,        
        nchain  = 4,        
        ncore   = 4        
        )
    
    fit.ORL.group1 <- igt_orl(        
        data    = "igt_data_group1.txt",        
        niter   = 6000,        
        nwarmup = 2000,        
        nchain  = 4,        
        ncore   = 4        
        )

    fit.ORL.group2 <- igt_orl(        
        data    = "igt_data_group2.txt",        
        niter   = 6000,        
        nwarmup = 2000,        
        nchain  = 4,        
        ncore   = 4        
        )

    return(c("\nModel parameters for Healthy Controls:\n\n",
            "Indices per participants:\n\n", 
            fit.ORL.group0$allIndPars,
            "\n\nGroup level indices\n\n",
            fit.ORL.group0$fit,
            "\n\nModel parameters for Patient Controls:\n\n",
            "Indices per participants:\n\n", 
            fit.ORL.group1$allIndPars,
            "\n\nGroup level indices\n\n",
            fit.ORL.group1$fit,
            "\n\nModel parameters for Suicide Attempters:\n\n",
            "Indices per participants:\n\n", 
            fit.ORL.group2$allIndPars,
            "\n\nGroup level indices\n\n",
            fit.ORL.group2$fit,
            ))
}


Indices.Per.Participants <- function() {

}