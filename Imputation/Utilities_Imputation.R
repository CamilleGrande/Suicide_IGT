## Utilities_Imputation.R
## Author: Camille Grandé
## Description:
##      This script holds the functions necessary to impute the missing data for 
##      our SPAD and Jena data for the linear regression modelling step


# This function imputes the missing data
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