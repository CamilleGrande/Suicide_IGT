# Utilities_Descriptives.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       descriptive stats part of the SPAD study

## This function loads our data in a csv file, and puts all values of missing data as NA to facilitate description of the dataset
Load.Data <- function(data) {
    dat <- as.data.frame(read.csv(data, na = c("NA", "PNA", "UNK", "Prefer not to answer", "MI", "NASK", "ASKU", " ", "NI", "OTH")))
    return(dat)
}


## This function summarises our numeric data in a nice little table
My.Numeric <- function(data, var) {

   data |>
      summarise(
        Variable = var,
        n = sum(!is.na(.data[[var]])),
        Mean = mean(.data[[var]], na.rm = T),
        SD = sd(.data[[var]], na.rm = T),
        Median = median(.data[[var]], na.rm = T),
        IQR = IQR(.data[[var]], na.rm = T),
        Min = min(.data[[var]], na.rm = T),
        Max = max(.data[[var]], na.rm = T),
        Skewness = as.numeric(skewness(.data[[var]], na.rm = T)),
        Kurtosis = as.numeric(kurtosis(.data[[var]], na.rm = T)),
        Standard_Error = sd(.data[[var]], na.rm = T) / sqrt(sum(!is.na(.data[[var]]))))
    
}


## This function summarises our non numeric data in a nice little frequency table
My.Frequencies <- function(data, var) {

  col <- as.character(data[[var]])

  n_total <- length(col)
  n_valid <- sum(!is.na(col))

  counts <- table(col, useNA = "no")

    data.frame(
      Values = names(counts),
      Frequency = as.integer(counts),
      Pctg_Valid = round(as.integer(counts) / n_valid * 100, 2),
      Pctg_Valid_Cum = round(cumsum(as.integer(counts) / n_valid * 100), 2),
      Pctg_Total = round(as.integer(counts) / n_total * 100, 2),
      Pctg_Total_Cum = round(cumsum(as.integer(counts) / n_total * 100), 2))

}


## This function summarises our numeric data
Summarise.Numeric <- function(my.df, path, directory, ext.csv, ext.pdf) {

    lapply(names(my.df), function(v) {
        My.Numeric(my.df, v) |>
            write.csv(paste0({{ path }}, {{ directory }}, v, {{ ext.csv }}), row.names = FALSE)
        
        my.histogram(my.df, v, directory, ext.pdf)
      })

}


## This function summarises our categorical / ordinal data
Summarise.Frequency <- function(my.df, path.csv, directory, ext.csv, ext.pdf) {

    lapply(names(my.df), function(v) {
        My.Frequencies(my.df, v) |>
            write.csv(paste0({{ path.csv }}, {{ directory }}, v, {{ ext.csv }}), row.names = FALSE)
       
        my.barplot(my.df, v, directory, ext.pdf)
      })
}


## This function computes the descriptive statistics in the whole sample and SNF sample only
## for a given numeric variable
Descriptives.Numeric <- function(data, directory, path, my.names, my.values) {

  ## Whole sample
  df <- Load.Data(data) |>
          select(-"PIN")

  Summarise.Numeric(df, "./1-Descriptives/Outputs/tables/", directory, ".csv", ".pdf")

  my.violinplot(df, path, my.names, my.values, ".pdf")
  
}