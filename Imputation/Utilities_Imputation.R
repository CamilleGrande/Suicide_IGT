## Utilities_Imputation.R
## Author: Camille Grandé
## Description:
##      This script holds the functions necessary to impute the missing data for 
##      our SPAD and Jena data for the linear regression modelling step


## This function imputes the missing data
My.Imputation <- function(data.rds, dataset, var, new.name, by.group = NULL, my.group = NULL, ...) {

    ## Read our .rds prepared data
    data <- read_rds(data.rds)

    ## Keep only vars we will use to impute values var
    var_and_predictors <- select(data, all_of(c("subj_id", var, ...)))

    ## We will impute values inside a group as we are working with clinical data
    ## this will keep only our group data if we specifiy by.group = T
    if (isTRUE(by.group)) {
        var_and_predictors <- subset(var_and_predictors, group == my.group)
    } 

    ## Save as .pdf the pattern of missing data among our group on our var of interest
    pdf(paste0("./Imputation/Outputs/", dataset, var, "_Missing_pattern.pdf"))
        md.pattern(var_and_predictors)
    dev.off()

    ## Check is values are NA -> if not, there is a problem and should check data before running the rest of the script
    ids <- subset(var_and_predictors, is.na(var_and_predictors[[var]]))

    if (all(is.na(ids[[var]]))) {
        cat("All values are correctly NA \n")
    } else {
        cat("Not all values are NA, double check before running imputation")
    }

    ## Separate subj id and predictor var for mice; will re-attach after
    subj_ids <- var_and_predictors$subj_id
    model_data <- select(var_and_predictors, -all_of("subj_id")) |> as.data.frame()

    ## Imputes our missing data
    tempData <- mice(model_data, m = 5, maxit = 50, meth = "pmm", seed = 42, print = F)

    ## Summary of our data before imputation
    before <- summary(var_and_predictors)


    ## Building the summary after imputation
    ## summary(complete(tempData)) gives us the summary for the first iteration only
    ## As we will work with all iteration values then average, we want the average summary

    ## First, get list of all data across all iteration (not summaries, actual data for each ptcp)
    imp_lists <- lapply(1:tempData$m, function(i) {
                        df <- complete(tempData, i)
                        as.numeric(df[[var]])
                    })

    ## create a summary for each of the lists
    imp_summaries <- lapply(imp_lists, function(x) {
                            c(
                                Min    = min(x, na.rm = TRUE),
                                Q1     = quantile(x, 0.25, na.rm = TRUE),
                                Median = median(x, na.rm = TRUE),
                                Mean   = mean(x, na.rm = TRUE),
                                Q3     = quantile(x, 0.75, na.rm = TRUE),
                                Max    = max(x, na.rm = TRUE)
                            )
                        })

    ## row bind the summaries, then average values across columns
    after <- do.call(rbind, imp_summaries) |>
                colMeans()
    after <- as.data.frame(t(after))

    ## Save summaries of before / after imputation for supplementary materials
    write.csv(before, paste0("./Imputation/Outputs/", dataset, var, "_Summary_before.csv"), row.names = F)
    write.csv(after, paste0("./Imputation/Outputs/", dataset, var, "_Summary_after.csv"), row.names = F)

    ## Save data distribution after imputation across all 5 iterations for supplementary materials
    pdf(paste0("./Imputation/Outputs/", dataset, var, "_Imputation_values.pdf"))
        print(stripplot(tempData, as.formula(paste0(var, " ~ .imp")), pch = 19, xlab = "Imputation number"))
    dev.off()

    ## Put back subject ids before exporting rds object so we can use it in regression
    list_with_ids <- lapply(1:tempData$m, function(i) {
        df <- complete(tempData, i)
        df$subj_id <- subj_ids
        df
    })

    ## save our final imputed data as rds to be used in regression models after
    write_rds(list_with_ids, paste0("./Imputation/Outputs/", dataset, var, "_tempData.rds"))
}


## This function cleans our imputed data lists
Return.Cleaned.Lists <- function(data, dataset) {

    ## Reads our original data
    df <- read_rds(data)

    ## List all .rds files of our datasdet (SPAD or Jena)
    files <- list.files("./Imputation/Outputs/", pattern = paste0("^", dataset, ".*\\.rds$"), full.names = TRUE)

    ## Reads all the .rds files we listed
    imp_lists <- lapply(files, readRDS)

    ## Get original filenames to clean future variable names
    var_names <- basename(files)
    ## 1. Remove SPAD_
    var_names <- gsub("^SPAD_", "", var_names)
    ## 2. Remove _tempData.rds
    var_names <- gsub("_tempData\\.rds$", "", var_names)

    ## Assigns our new cleaned names to our imputed values lists
    names(imp_lists) <- var_names

    ## Extract the imputed column (first column) and subj id for each iteration
    clean_lists <- lapply(imp_lists, function(var_list) {

        ## Extract the names of the imputed column (first column)
        imputed_col <- names(var_list[[1]])[1]

        ## Extract that column for each iteration + subj id
        lapply(var_list, function(df) {
            data.frame(
                subj_id = df$subj_id,
                value   = df[[imputed_col]]
            )
        })    
    })

    return(clean_lists)
}


## This function builds our newly imputed datasets
Build.Imputed.Datasets <- function(df, clean_lists) {

    completed <- vector("list", 5)

    for (i in 1:5) {

        temp <- df

        for (v in names(clean_lists)) {

            imp_df <- clean_lists[[v]][[i]]   # iteration i: df(subj_id, value)

            # merge imputed values into temp by subj_id
            temp <- merge(temp, imp_df, by = "subj_id", all.x = TRUE)

            # replace missing values in the original variable
            temp[[v]][is.na(temp[[v]])] <- temp$value[is.na(temp[[v]])]

            # remove helper column
            temp$value <- NULL
        }

        completed[[i]] <- temp
    }

    names(completed) <- paste0("imp", 1:5)
    return(completed)
}


## This function saves a .rds file for each new dataset
Save.Imputed.Datasets <- function(completed, dataset) {

    for (name in names(completed)) {
        saveRDS(
            completed[[name]],
            file = paste0("./Imputation/Outputs/", dataset, "_", name, ".rds")
        )
    }
}


## This functions wraps our previous functions for the pipeline to make the imputed datasets
Make.Imputed.Datasets <- function(data, dataset) {

    clean_lists <- Return.Cleaned.Lists(data, dataset)

    df <- read_rds(data)

    completed <- Build.Imputed.Datasets(df, clean_lists)

    Save.Imputed.Datasets(completed, dataset)
}


Make.All.Datasets <- function(dataset_spad, dataset_jena) {

    all_sets <- vector("list", 5)

    for (i in 1:5) {

        spad <- readRDS(paste0("./Imputation/Outputs/SPAD_imp", i, ".rds"))
        jena <- readRDS(paste0("./Imputation/Outputs/JENA_imp", i, ".rds"))

        all_sets[[i]] <- bind_rows(spad, jena)
    }

    names(all_sets) <- paste0("all_imp", 1:5)
    return(all_sets)
}


Save.All.Datasets <- function(dataset_spad, dataset_jena) {

    all_sets <- Make.All.Datasets(dataset_spad, dataset_jena)

    for (name in names(all_sets)) {
        saveRDS(
            all_sets[[name]],
            file = paste0("./Imputation/Outputs/", name, ".rds")
        )
    }
}