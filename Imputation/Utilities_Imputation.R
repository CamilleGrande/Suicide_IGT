## Utilities_Imputation.R
## Author: Camille Grandé
## Description:
##      This script holds the functions necessary to impute the missing data for 
##      our SPAD and Jena data for the linear regression modelling step


# This function imputes the missing data
My.Imputation <- function(data.rds, dataset, var, new.name, by.group = NULL, my.group = NULL, ...) {

    data <- read_rds(data.rds)

    var_and_predictors <- select(data, all_of(c("subj_id", var, ...)))

    if (isTRUE(by.group)) {
        var_and_predictors <- subset(var_and_predictors, group == my.group)
    } 

    pdf(paste0("./Imputation/Outputs/", dataset, var, "_Missing_pattern.pdf"))
        md.pattern(var_and_predictors)
    dev.off()

    ids <- subset(var_and_predictors, is.na(var_and_predictors[[var]]))

    if (all(is.na(ids[[var]]))) {
        cat("All values are correctly NA \n")
    } else {
        cat("Not all values are NA, double check before running imputation")
    }

    ## Separate subj id and predictor var for mice; will re-attach after
    subj_ids <- var_and_predictors$subj_id
    model_data <- select(var_and_predictors, -all_of("subj_id")) |> as.data.frame()

    tempData <- mice(model_data, m = 5, maxit = 50, meth = "pmm", seed = 42, print = F)

    before <- summary(var_and_predictors)
    after <- summary(complete(tempData))

    write.csv(before, paste0("./Imputation/Outputs/", dataset, var, "_Summary_before.csv"), row.names = F)
    write.csv(after, paste0("./Imputation/Outputs/", dataset, var, "_Summary_after.csv"), row.names = F)

    # stripplot(tempData, .[[var]], pch = 19, xlab = "Imputation number")
    pdf(paste0("./Imputation/", var, "_Imputation_values.pdf"))
        print(stripplot(tempData, as.formula(paste0(var, " ~ .imp")), pch = 19, xlab = "Imputation number"))
    dev.off()

    write_rds(tempData, paste0("./Imputation/Outputs/", dataset, var, "_tempData.rds"))
}