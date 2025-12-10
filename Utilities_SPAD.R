# Utilities_SPAD.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to run the Driver Script

Load.Data <- function(my.file) {
    return(read.csv(my.file))
}

Descriptive.Stats <- function(my.file) {
    
    ## This function loads the data file and puts it into
    ## the variable my.data
    my.data <- Load.Data(my.file)
    
    ## This function loads a table with the descriptive
    ## statistics for each column in our dataframe, per Group
    ## 0 = Healthy Controls
    ## 1 = Depressed Patient Controls
    ## 2 = Suicide Attempters
    describeBy(my.data, group='Group')

    
}