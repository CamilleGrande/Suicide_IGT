# Utilities_Regression.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       correlation/regression part of the SPAD study


## This function fits a given number of models (depending on imputed datasets given) with the specified parameters
## and returns a list with all our models
Fit.Models <- function(imputed.datasets, dataset, group = NULL, DV, method, cook.threshold, IV) {

    complete_formula <- as.formula(paste(DV, "~", paste(IV, collapse = " + ")))

    cat(paste(complete_formula))

    models <- lapply(imputed.datasets, function(imp) {

        df <- read_rds(imp)

        complete_model <- lm(complete_formula, data = df)

        model <- if (method == "none") {
                        complete_model
                    } else {
                        step(complete_model, direction = method, trace = 0)
                    }
        
        cat("\nDataset:", dataset, "| N:", nrow(df), "| DV:", DV,
        "\nFinal formula:", deparse(formula(model)), "\n")

        return(model)
    })

    return(models)
}



