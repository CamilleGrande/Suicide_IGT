# Utilities_Descriptives.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       descriptive stats part of the SPAD study

## This function loads our data in a csv file, and puts all values of missing data as NA to facilitate description of the dataset
Load.Data <- function(data) {
    dat <- as.data.frame(read.csv(data, na = c("NA", " ")))
    return(dat)
}


## Splits a dataframe into a named list of subsets: one entry per level of
## group.var, or a single "Overall" entry containing the whole sample if
## group.var is NULL. This is the switch that lets every summary function
## below run either on the whole sample or separately per group, using
## the exact same code path either way.
Split.By.Group <- function(data, group.var = NULL) {
 
  if (is.null(group.var)) {
    return(list(Overall = data))
  }
 
  data <- data |> filter(!is.na(.data[[group.var]]))
  split(data, data[[group.var]])
}


## This function summarises our numeric data in a nice little table
My.Numeric <- function(data, var, group.label = "Overall") {
# name
    data <- data |> filter(!is.na(.data[[var]]))

    my.summmary <- data |>
                    summarise(
                        # here
                        Group = group.label,
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
My.Frequencies <- function(data, var, group.label = "Overall") {

  col <- as.character(data[[var]])

  n_total <- length(col)
  n_valid <- sum(!is.na(col))

  counts <- table(col, useNA = "no")

    data.frame(
      Group = group.label,
      Values = names(counts),
      Frequency = as.integer(counts),
      Pctg_Valid = round(as.integer(counts) / n_valid * 100, 2),
      Pctg_Valid_Cum = round(cumsum(as.integer(counts) / n_valid * 100), 2),
      Pctg_Total = round(as.integer(counts) / n_total * 100, 2),
      Pctg_Total_Cum = round(cumsum(as.integer(counts) / n_total * 100), 2))

}


All.Numeric <- function(ids.IGT, data.Descriptives, path, vars, ext.pdf, ext.csv, group.var = NULL) {

    df <- read_delim(ids.IGT)|> 
                distinct(subjID) |> 
                    rename(SID = subjID)

    data <- read_csv(data.Descriptives) |>
                rename(SID = 1) |>
                    inner_join(df, by = "SID")

    groups <- Split.By.Group(data, group.var)
 
    ## filenames only change when we're actually splitting by group, so
    ## ungrouped calls keep producing the exact same files as before
    file.tag <- if (is.null(group.var)) "" else paste0("_by_", group.var)
    
    ## Creates numeric summary for all variables provided as argument
    lapply(vars, function(v) {
        
        my.table <- bind_rows(lapply(names(groups), function(g) {
            My.Numeric(groups[[g]], v, group.label = g)
        }))
 
        write.csv(my.table, paste0(path, v, file.tag, ext.csv), row.names = FALSE)
        my.histogram(data, v, path, paste0(file.tag, ext.pdf), group.var = group.var)
    })
}


All.Frequencies <- function(ids.IGT, data.Descriptives, path, vars, ext.pdf, ext.csv, group.var = NULL) {

    df <- read_delim(ids.IGT)|> 
                distinct(subjID) |> 
                    rename(SID = subjID)

    data <- read_csv(data.Descriptives) |>
                rename(SID = 1) |>
                    inner_join(df, by = "SID")

    groups <- Split.By.Group(data, group.var)
 
    ## filenames only change when we're actually splitting by group, so
    ## ungrouped calls keep producing the exact same files as before
    file.tag <- if (is.null(group.var)) "" else paste0("_by_", group.var)
    
    ## Creates numeric summary for all variables provided as argument
    lapply(vars, function(v) {
        
        my.table <- bind_rows(lapply(names(groups), function(g) {
            My.Frequencies(groups[[g]], v, group.label = g)
        }))
 
        write.csv(my.table, paste0(path, v, file.tag, ext.csv), row.names = FALSE)
        my.barplot(data, v, path, paste0(file.tag, ext.pdf), group.var = group.var)
    })
}