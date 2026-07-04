# Utilities_Regression.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       correlation/regression part of the SPAD study


## This function fits a given number of models (depending on imputed datasets given) with the specified parameters
## and returns a list with all our models
Fit.Models <- function(imputed.datasets, dataset, group = NULL, DV, method, cook.threshold, IV) {

    cat("\n=== MODEL FITTING ===\n")

    ## Create our formula with our dependent variable and all our inependent variable
    complete_formula <- as.formula(paste(DV, "~", paste(IV, collapse = " + ")))

    cat(paste(complete_formula))

    ## Fits one model for each imputed dataset and returns a list with all models
    models <- lapply(imputed.datasets, function(imp) {

        ## Reads imputed dataset
        my.data <- readr::read_rds(imp)
  
        if (!is.null(group)) {
            my.data <- dplyr::filter(my.data, group == !!group)
        }

        ## Fits model for this dataset
        complete_model <- lm(complete_formula, data = my.data)
        complete_model$call$data <- my.data

        ## Adjusts model with our method if we specified one
        model <- if (method == "none") {
                        complete_model
                    } else {
                        step(complete_model, direction = method, trace = 0)
                    }

        cat("\nDataset:", dataset, "| N:", nrow(my.data), "| DV:", DV,
        "\nFinal formula:", deparse(formula(model)), "\n")

        return(model)
    })

    return(models)
}


## Check the outliers using Cook's distance
## and returns updated imputed dfs without those observations
Check.Outliers.Cook <- function(models, imputed.datasets) {

    cat("\n=== COOK'S DISTANCE ===\n")

    ## Creates a list with all our cooks distance for each participant in each imputed dataset
    cook_list <- lapply(seq_along(models), function(i) {
  
        m   <- models[[i]]
        cd  <- cooks.distance(m)
    
        ## cook's threshold
        threshold <- 4 / length(cd)
    
        data.frame(
            subj_id  = subj_ids$subj_id,
            imp      = paste0("imp", i),
            cook     = as.numeric(cd),
            seuil    = threshold,
            influent = cd > threshold
        )
    })

    cook_df <- bind_rows(cook_list)

    ## Synthesizes which cook's distances are influent across all imputations
    ## We will get rid of observations that are influent across 3 or more dataset (majority of datasets)
    cook_synthesis <- cook_df |>
                        group_by(subj_id) |>
                            summarise(
                                n_influent    = sum(influent),
                                mean_cook      = round(mean(cook), 4),
                                max_cook      = round(max(cook), 4)
                            ) |>
                                arrange(desc(n_influent), desc(max_cook)) |>
                                    mutate(
                                        Influent_in = case_when(
                                        n_influent == 5 ~ "All imputations",
                                        n_influent >= 3 ~ "Nearly all (≥3/5)",
                                        n_influent >= 1 ~ "Some (1-2/5)",
                                        TRUE            ~ "None" 
                                    ))
    
    ## Keep ids of influent observations for log purposes
    cook_synthesis |>
        filter(n_influent >= 3) |>
            write.csv("./Regression/Outputs/Check_Influent_Observations.csv", row.names = F)


    mean_threshold <- mean(4 / sapply(models, function(m) length(residuals(m))))

    ## Creates graph for supplementary materials
    pdf("./Regression/Outputs/Cook_Graph.pdf")
        cook_synthesis |>
            mutate(label = ifelse(n_influent >= 3, as.character(subj_id), "")) %>%
                ggplot(aes(x = reorder(subj_id, mean_cook), 
                            y = mean_cook, 
                            fill = Influent_in)) +
                geom_col() +
                geom_hline(yintercept = mean_threshold, linetype = "dashed", color = "red") +
                scale_fill_manual(values = c(
                    "All imputations"     = "#E63946",
                    "Nearly all (≥3/5)"   = "#F4A261",
                    "Some (1-2/5)"        = "#FFD166",
                    "None"                = "#457B9D"
                )) +
                labs(title = "Mean Cook's distance (5 imputations)",
                    x = "Participant", y = "Mean Cook", fill = "Influent in") +
                theme_bw() +
                theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    dev.off()

    influent_ids <- subset(cook_synthesis, Influent_in == "All imputations" | Influent_in == "Nearly all (≥3/5)") |>
                        dplyr::select(subj_id)

    imp_df_without_infl_obs <- lapply(imputed.datasets, function(imp) {

                                ## Reads imputed dataset
                                df <- read_rds(imp)

                                df_filtered <- df |>
                                    dplyr::filter(!subj_id %in% influent_ids$subj_id)
    })

    ## Save each filtered imputed dataset as an RDS file
    lapply(seq_along(imp_df_without_infl_obs), function(i) {
        write_rds(
            imp_df_without_infl_obs[[i]],
            paste0("./Regression/Outputs/imp_df_without_infl_obs_", i, ".rds")
        )
    })

    return(imp_df_without_infl_obs)
}


## This function computes Grubb's test for outliers
## It quits our script if outliers are detected!
Check.Outliers.Grubb <- function(my_models_without_influent) {

    cat("\n=== GRUBB ===\n")

    # might need library(outliers)
    grubbs_results <- map_dfr(seq_along(my_models_without_influent), function(i) {
        
        g <- grubbs.test(residuals(my_models_without_influent[[i]]))

        data.frame(
            imputation  = paste0("imp", i),
            statistic = round(unname(g$statistic["G"]), 3),
            p_value     = round(g$p.value, 4),
            conclusion  = ifelse(g$p.value < .05,
                                "Outlier values detected",
                                "No outlier values detected"),
            row.names   = NULL
        )
    })

    ## Keep log of outlier status
    outliers <- grubbs_results |>
                    dplyr::filter(conclusion == "⚠ Outlier values detected")
    
    write.csv(outliers, "./Regression/Outputs/Check_Grubbs_Outliers.csv", row.names = F)

    if (nrow(outliers) > 0) {
        cat("Grubb's test detected outliers; Inspect your data and re-run the script\n")
        q()
    } else {
        cat("Grubb's test did not detect outliers; moving on to next test\n")
    }
}


Check.Variance.Inflation.Factor <- function(my_models_without_influent) {

    cat("\n=== VIF ===\n")

    vif_list <- lapply(seq_along(my_models_without_influent), function(i) {
  
        model   <- my_models_without_influent[[i]]
        vif  <- vif(model)

    })

    vif_df <- as.data.frame(vif_list)

    names(vif_df)[1] <- "VIF"
    vif_df$Statut <- case_when(
                            vif_df$VIF < 4  ~ "No or Little Multicollinearity",
                            vif_df$VIF < 10 ~ "Moderate Multicollinearity",
                            TRUE            ~ "High Multicollinearity"
                        )

    ## We need the row names here to have the variable names
    write.csv(vif_df, "./Regression/Outputs/Check_VIF_Multicollinearity.csv", row.names = T)

    if (any(vif_df$Statut %in% c("Moderate Multicollinearity", "High Multicollinearity"))) {
        cat("Variance Inflation Factor detected moderate / high multicollinearity; Inspect your data and re-run the script\n")
        q()
    } else {
        cat("Variance Inflation Factor did not detect multicollinearity; moving on to next test\n")
    }
}


Check.Residuals.Normality <- function(my_models_without_influent) {

    cat("\n=== NORMALITY OF RESIDUALS (SHAPIRO WILK & QQ-PLOTS) ===\n")

    shapiro_results <- map_dfr(seq_along(my_models_without_influent), function(i) {
        
        s <- shapiro.test(residuals(my_models_without_influent[[i]]))
        data.frame(
            imputation = paste0("imp", i),
            W         = round(s$statistic, 4),
            p_value   = round(s$p.value, 4),
            conclusion = ifelse(s$p.value > .05, "Is normal", "Is NOT normal")
        )
    })

    write.csv(shapiro_results, "./Regression/Outputs/Check_Shapiro_Residuals_Normality.csv", row.names = F)

    pdf("./Regression/Outputs/QQ_Plots_Residuals_Normality.pdf")
        par(mfrow = c(2, 3))
        for (i in 1:5) {
            qqnorm(residuals(my_models_without_influent[[i]]), 
                    main = paste("Q-Q plot - imp", i))
            qqline(residuals(my_models_without_influent[[i]]), col = "red")
        }
        par(mfrow = c(1, 1))
    dev.off()

    cat("\nQQ-Plots can be found in Outputs folder; inspect them for further residual normality check\n\n")
}


Check.Homoscedasticity.Breusch.Pagan <- function(my_models_without_influent) {

    cat("\n=== HOMOSCEDASTICITY (BREUSCH PAGAN) ===\n")

    bp_results <- map_dfr(seq_along(my_models_without_influent), function(i) {
  
        bp <- bptest(my_models_without_influent[[i]])

        data.frame(
            imputation = paste0("imp", i),
            BP         = round(bp$statistic, 3),
            df         = bp$parameter,
            p_value    = round(bp$p.value, 4),
            conclusion = ifelse(bp$p.value > .05, "Homoscedasticity OK", "Heteroscedasticity")
        )
    })

    write.csv(bp_results, "./Regression/Outputs/Check_Homoscedasticity_Breusch_Pagan.csv")

    ## Count nb of imputations with heteroscedasticity
    n_hetero <- sum(bp_results$p_value < .05)

    ## Our cut-off to say heteroscedasticity is present is 3 or more imputations (more than half) with heteroscedasticity
    if (n_hetero >= 3) {
        
        cat("\nHeteroscedasticity in ", n_hetero, " of 5 imputations → moving on with robust error HC3\n\n")

        ## Compute robust regression
        robust_models <- lapply(my_models_without_influent, function(model) {
            model$vcovHC3 <- vcovHC(model, type = "HC3")
            model
        })

        return(robust_models)

    } else {

        cat("\nHomoscedasticity in ", 5 - n_hetero, "/5 imputations.\n")

        return(my_models_without_influent)
    }
}


Pooling.Models <- function(models) {

    cat("\n=== POOLING MODELS ... ===\n")

    ## as.mira to be able to pool them after
    models_mira <- as.mira(models)

    ## Pooling all 5 models
    models_pooled <- pool(models)

    cat("\n── pooling R²...  ──\n")
        ## R2 pooled of our models (normal or robust depending on homoscedasticity status)
        r2_pooled <- pool.r.squared(models_pooled)
    print(r2_pooled) 

    pooled_summary <- summary(models_pooled)
    write.csv(pooled_summary, "./Regression/Outputs/Pooled_Regression_Summary.csv")

    return(models_pooled)
}








p <- ggplot(pooled_summary, aes(x = estimate, y = term)) +
        geom_point(size = 3) +
        geom_errorbarh(aes(xmin = estimate - 1.96 * std.error,
                        xmax = estimate + 1.96 * std.error),
                    height = 0.2) +
        geom_vline(xintercept = 0, linetype = "dashed") +
        labs(
            title = "Pooled Regression Coefficients (MI + Robust SE)",
            x = "Estimate (with 95% CI)",
            y = "Predictor"
        ) +
        theme_minimal()
    ggsave("./Regression/Outputs/Forest_Plot.pdf", plot = p)




Regression <- function(imputed.datasets, imputed.datasets.no.outliers, dataset, group = NULL, DV, method, cook.threshold, IV) {

    ## Keep our subject ids here for when need them
    subj_ids <- read_rds(imputed.datasets[1]) |> dplyr::select(subj_id)

    ## First, we fit the initial models for all imputations
    my_models <- Fit.Models(imputed.datasets, dataset, group = NULL, DV, method, cook.threshold, IV)

    ## Then we check for outliers with Cook's distance
    ## This function also removes the outliers to give us updated dfs without thos subjects, for each imputation
    ## Subjects are excluded if they are outliers on at least 3 imputations
    imp_dfs_without_influent <- Check.Outliers.Cook(my_models, imputed.datasets)

    ## Here, we re-fit the models without the outliers 
    my_models_without_influent <- Fit.Models(imputed.datasets.no.outliers, dataset, group = NULL, DV, method, cook.threshold, IV)

    ## Next, we check for outliers with Grubb's test
    ## If it detects outliers, it will quit the environment and you should check the data
    Check.Outliers.Grubb(my_models_without_influent)

    ## We assess multicollinearity
    ## If it detects outliers, it will quit the environment and you should check the data
    Check.Variance.Inflation.Factor(my_models_without_influent)

    ## Check that the residuals are normally distributed
    ## using Shapiro Wilk's test and QQ-plot inspection
    Check.Residuals.Normality(my_models_without_influent)

    ## Check Homoscedasticity with Breusch Pagan test
    ## If heteroscedasticity in 3 or more imputations, compute robust regression models
    ## Will overwrite my_models either with normal models or robust models to pool them
    my_models <- Check.Homoscedasticity.Breusch.Pagan(my_models_without_influent)

    ## Pools our models in one final model
    models_pooled <- Pooling.models(my_models_without_influent)
}