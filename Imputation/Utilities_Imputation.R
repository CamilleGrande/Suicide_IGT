## Utilities_Imputation.R
## Author: Camille Grandé
## Description:
##      This script holds the functions necessary to impute the missing data for 
##      our SPAD and Jena data for the linear regression modelling step


## This function imputes the data for a variable
Imputation <- function(data, var) {

    ## Keep only vars we will use to impute values var
    var_and_predictors <- select(data, all_of(c("subj_id", var, 
                                ## predictors:
                                "group", "age", "sex", "mmse_total", "nart_corr", "ssi_total")))

    model_data <- select(var_and_predictors, -all_of("subj_id")) |> 
                    as.data.frame()

    ## Imputes our missing data
    tempData <- mice(model_data, m = 5, maxit = 50, meth = "pmm", seed = 42, print = F)

    return(tempData)
}


## This function averages the 5 imputed dataset for the imputed variable
## and returns a summary of the old an new descriptives post imputation
Summary.Before.After <- function(data, to_impute, all_imputed) {

    summary_before <- data |>
                        select(all_of(names(to_impute))) |>
                            summarise(across(everything(), list(
                                n = ~sum(!is.na(.)),
                                n_missing = ~sum(is.na(.)),
                                mean = ~round(mean(., na.rm = TRUE), 3),
                                sd = ~round(sd(., na.rm = TRUE), 3),
                                median = ~round(median(., na.rm = TRUE), 3),
                                iqr = ~round(IQR(., na.rm = TRUE), 3),
                                min = ~round(min(., na.rm = TRUE), 3),
                                max = ~round(max(., na.rm = TRUE), 3)),
                                .names = "{.col}__{.fn}")) |>
                                    pivot_longer(everything(), names_to = c("variable", ".value"), names_sep = "__")

    write.csv(summary_before, "./Imputation/Outputs/Summary_before_imputation.csv", row.names = FALSE)


    summary_after <- map_dfr(names(all_imputed), function(varname) {

            ## extracts imputation data for variable x
            imp <- all_imputed[[varname]]

            ## summarise descriptives for each imputation
            per_imp <- map_dfr(1:5, function(m) {
                                
                        completed <- complete(imp, action = m)
                                tibble(
                                    variable   = varname,
                                    imputation = as.character(m),
                                    n    = sum(!is.na(completed[[varname]])),
                                    mean = round(mean(completed[[varname]], na.rm = TRUE), 3),
                                    sd   = round(sd(completed[[varname]], na.rm = TRUE), 3),
                                    median = round(median(completed[[varname]], na.rm = TRUE), 3),
                                    iqr = round(IQR(completed[[varname]], na.rm = TRUE), 3),
                                    min  = round(min(completed[[varname]], na.rm = TRUE), 3),
                                    max  = round(max(completed[[varname]], na.rm = TRUE), 3)
                                )
                        })

            ## summarise descriptives across all imputations 
            mean_row <- per_imp |>
                            summarise(
                                variable = varname, 
                                imputation = "mean_across_5",
                                n = mean(n), 
                                mean = round(mean(mean), 3),
                                sd = round(mean(sd), 3),
                                median = round(mean(median), 3),
                                iqr = round(mean(iqr), 3),
                                min = round(mean(min), 3),
                                max = round(mean(max), 3))

            bind_rows(per_imp, mean_row)
    })

    write.csv(summary_after, "./Imputation/Outputs/summary_after_imputation.csv", row.names = FALSE)
}


## Run all the steps
Run.Imputation <- function(data.SPAD, data.Jena, ...) {

    ## Read our .rds prepared data
    spad <- read_rds(data.SPAD)

    ## Select the vars to be imputed
    to_impute <- spad |>
                    select(all_of(c(...))) 

    ## Impute data and plot imputed values                         
    all_imputed <- lapply(names(to_impute), function(i) {
        
        ## imputation
        imp <- Imputation(spad, i)

        ## plot stripplots of imputed data
        pdf(paste0("./Imputation/Outputs/Stripplot_", i, ".pdf"))
            print(stripplot(imp, as.formula(paste0(i, "~ .imp")), pch = 19, xlab = "Imputation number"))
        dev.off()

        return(imp)
    }) 
                     

    ## Plot convergence 
    ## for go no go, will not have sd panels because only 1 missing data point
    ## so cannot compute SD (SD of a single nb is undefined)                       
    pdf("./Imputation/Outputs/Convergence_Chains.pdf")

        ## in order:
        ## flu verb p, ani, gonogo correct, omissions, commissions, mean RT
        invisible(lapply(all_imputed, function(imp) {
            print(plot(imp))
        }))

    dev.off()                         

    ## sets the names in our list
    names(all_imputed) <- names(to_impute)

    ## Data summaries before and after imputation
    Summary.Before.After(spad, to_impute, all_imputed)                  

    jena <- read_rds(data.Jena)

    ## Builds imputed datasets                         
    for (imp in 1:5) {

        spad_imputed <- spad 

        for (var in names(all_imputed)) {
            spad_imputed[[var]] <- complete(all_imputed[[var]], action = imp) |>
                                        pull(all_of(var))
        }

        ## z score go no go values for each imputation
        spad_imputed$zscore_gonogo_total_correct <- as.numeric(scale(spad_imputed$spad_gonogo_total_correct))
        spad_imputed$zscore_gonogo_total_commissions <- as.numeric(scale(spad_imputed$spad_gonogo_total_commissions))
        spad_imputed$zscore_gonogo_total_omissions <- as.numeric(scale(spad_imputed$spad_gonogo_total_omissions))

        ## bind spad and suicide decide
        full_dataset <- bind_rows(spad_imputed, jena)

        write_rds(full_dataset, paste0("./Imputation/Outputs/imputed_dataset_", imp, ".rds"))
    }
}