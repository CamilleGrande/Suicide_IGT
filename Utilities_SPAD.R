# Utilities_SPAD.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to run the Driver Script

Load.Data <- function(my.file) {
    return(read.csv(my.file))
}