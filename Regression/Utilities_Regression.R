# Utilities_Regression.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       correlation/regression part of the SPAD study


## Builds an output file path prefixed with the run's name (e.g. "All", "SUICIDE-DECIDE")
## so that outputs from different Regression() calls don't overwrite each other
Output.Path <- function(prefix, filename) {
    file.path("./Regression/Outputs", paste0(prefix, "_", filename))
}

## This function fits a given number of models (depending on imputed datasets given) with the specified parameters
## and returns a list with all our models
Fit.Models <- function(imputed.datasets, cohort, group = NULL, DV, method, IV) {

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

        cat("\nCohort:", cohort, "| N:", nrow(my.data), "| DV:", DV,
        "\nFinal formula:", deparse(formula(model)), "\n")

        return(model)
    })

    return(models)
}

## Check the outliers using Cook's distance
## and returns updated imputed dfs without those observations
Check.Outliers.Cook <- function(models, imputed.datasets, sub, prefix) {

    cat("\n=== COOK'S DISTANCE ===\n")

    ## Creates a list with all our cooks distance for each participant in each imputed dataset
    cook_list <- lapply(seq_along(models), function(i) {
  
        m   <- models[[i]]
        cd  <- cooks.distance(m)
    
        ## cook's threshold
        threshold <- 4 / length(cd)
    
        data.frame(
            subj_id  = sub$subj_id,
            imp      = paste0("imp", i),
            cook     = as.numeric(cd),
            seuil    = threshold,
            influent = cd > threshold
        )
    })

    cook_df <- bind_rows(cook_list)

    ## Number of imputations, and majority threshold (adapts to however many were provided)
    n.imp     <- length(models)
    majority  <- floor(n.imp / 2) + 1
 
    label_all      <- "All imputations"
    label_majority <- sprintf("Nearly all (\u2265%d/%d)", majority, n.imp)
    label_some     <- sprintf("Some (1-%d/%d)", majority - 1, n.imp)
    label_none     <- "None"

    ## Synthesizes which cook's distances are influent across all imputations
    ## We will get rid of observations that are influent across a majority of datasets
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
                                        n_influent == 5 ~ label_all,
                                        n_influent >= 3 ~ label_majority,
                                        n_influent >= 1 ~ label_some,
                                        TRUE            ~ label_none 
                                    ))
    
    ## Keep ids of influent observations for log purposes
    cook_synthesis |>
        filter(n_influent >= majority) |>
            write.csv(Output.Path(prefix, "Check_Influent_Observations.csv"), row.names = F)


    mean_threshold <- mean(4 / sapply(models, function(m) length(residuals(m))))

    ## Creates graph for supplementary materials
    pdf(Output.Path(prefix, "Cook_Graph.pdf"))
        cook_plot <- cook_synthesis |>
                        mutate(label = ifelse(n_influent >= 3, as.character(subj_id), "")) %>%
                            ggplot(aes(x = reorder(subj_id, mean_cook), 
                                        y = mean_cook, 
                                        fill = Influent_in)) +
                            geom_col() +
                            geom_hline(yintercept = mean_threshold, linetype = "dashed", color = "red") +
                            scale_fill_manual(values = setNames(
                                c("#E63946", "#F4A261", "#FFD166", "#457B9D"),
                                c(label_all, label_majority, label_some, label_none)
                            )) +
                            labs(title = sprintf("Mean Cook's distance (%d imputations)", n.imp),
                                    x = "Participant", y = "Mean Cook", fill = "Influent in") +
                            theme_bw() +
                            theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
        print(cook_plot)
    dev.off()

    influent_ids <- subset(cook_synthesis, Influent_in == label_all | Influent_in == label_majority) |>
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
            Output.Path(prefix, paste0("imp_df_without_infl_obs_", i, ".rds")))
    })

    return(imp_df_without_infl_obs)
}


## This function computes Grubb's test for outliers
## It quits our script if outliers are detected!
Check.Outliers.Grubb <- function(my_models_without_influent, prefix) {

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
                    dplyr::filter(conclusion == "Outlier values detected")
    
    write.csv(outliers, Output.Path(prefix, "Check_Grubbs_Outliers.csv"), row.names = F)

    if (nrow(outliers) > 0) {
        cat("Grubb's test detected outliers; Inspect your data and re-run the script\n")
        q()
    } else {
        cat("Grubb's test did not detect outliers; moving on to next test\n")
    }
}


Check.Variance.Inflation.Factor <- function(my_models_without_influent, prefix) {

    cat("\n=== VIF ===\n")

    vif_list <- lapply(seq_along(my_models_without_influent), function(i) {
 
        model    <- my_models_without_influent[[i]]
        vif_raw  <- vif(model)
 
        ## car::vif() returns a plain named vector only when every term has 1 df
        ## As soon as one predictor is a factor with >2 levels (e.g. group, site),
        ## it returns a GVIF matrix instead (Variable / GVIF / Df / GVIF^(1/(2*Df))).
        ## We normalise both cases to a single named vector here.
        if (is.matrix(vif_raw)) {
            data.frame(
                Variable = rownames(vif_raw),
                ## squared, generalized VIF is the value comparable to a classic VIF
                VIF      = vif_raw[, "GVIF^(1/(2*Df))"]^2,
                row.names = NULL
            )
        } else {
            data.frame(
                Variable = names(vif_raw),
                VIF      = as.numeric(vif_raw),
                row.names = NULL
            )
        }
    })

    vif_df <- do.call(rbind, lapply(seq_along(vif_list), function(i) {
 
        data.frame(
            Model    = paste0("Model_", i),
            Variable = vif_list[[i]]$Variable,
            VIF      = vif_list[[i]]$VIF
        )
    }))

    vif_df$Statut <- case_when(
                            vif_df$VIF < 4  ~ "No or Little Multicollinearity",
                            vif_df$VIF < 10 ~ "Moderate Multicollinearity",
                            TRUE            ~ "High Multicollinearity"
                        )

    write.csv(vif_df, Output.Path(prefix, "Check_VIF_Multicollinearity.csv"), row.names = F)

    if (any(vif_df$Statut %in% c("Moderate Multicollinearity", "High Multicollinearity"))) {
        cat("Variance Inflation Factor detected moderate / high multicollinearity; Inspect your data and re-run the script\n")
        q()
    } else {
        cat("Variance Inflation Factor did not detect multicollinearity; moving on to next test\n")
    }
}


Check.Residuals.Normality <- function(my_models_without_influent, prefix) {

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

    write.csv(shapiro_results, Output.Path(prefix, "Check_Shapiro_Residuals_Normality.csv"), row.names = F)
 
    pdf(Output.Path(prefix, "QQ_Plots_Residuals_Normality.pdf"))
        par(mfrow = c(2, 3))
        for (i in seq_along(my_models_without_influent)) {
            qqnorm(residuals(my_models_without_influent[[i]]), 
                    main = paste("Q-Q plot - imp", i))
            qqline(residuals(my_models_without_influent[[i]]), col = "red")
        }
        par(mfrow = c(1, 1))
    dev.off()

    cat("\nQQ-Plots can be found in Outputs folder; inspect them for further residual normality check\n\n")
}


Check.Homoscedasticity <- function(my_models_without_influent, prefix) {

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

    write.csv(bp_results, Output.Path(prefix, "Check_Homoscedasticity_Breusch_Pagan.csv"))

    ## Number of imputations, and majority threshold (adapts to however many were provided)
    n.imp    <- length(my_models_without_influent)
    majority <- floor(n.imp / 2) + 1

    ## Count nb of imputations with heteroscedasticity
    n_hetero <- sum(bp_results$p_value < .05)

     ## Our cut-off to say heteroscedasticity is present is a majority of imputations with heteroscedasticity
    if (n_hetero >= majority) {
        
        cat("\nHeteroscedasticity in ", n_hetero, " of", n.imp, "imputations → moving on with robust error HC3\n\n")

        ## Compute robust regression
        robust_models <- lapply(my_models_without_influent, function(model) {
            model$vcovHC3 <- vcovHC(model, type = "HC3")
            model
        })
 
        return(list(models = robust_models, robust = TRUE))
 
    } else {
 
        cat("\nHomoscedasticity in ", n.imp - n_hetero, "/", n.imp, "imputations.\n")

        return(list(models = my_models_without_influent, robust = FALSE))
    }
}


Pooling.Models <- function(models, prefix) {

    cat("\n=== POOLING MODELS ... ===\n")

    ## as.mira to be able to pool them after
    models_mira <- as.mira(models)

    ## Pooling all models
    #     models_pooled <- pool(models)

    models_pooled <- pool(models_mira)

    cat("\n── pooling R²...  ──\n")
        ## R2 pooled of our models
        r2_pooled <- pool.r.squared(models_pooled)
    print(r2_pooled) 

    pooled_summary <- summary(models_pooled)
    write.csv(pooled_summary, Output.Path(prefix, "Pooled_Regression_Summary.csv"))

    return(pooled_summary)
}


## Pools models using Rubin's rules but substitutes each model's HC3 robust 
## variance in place of the default OLS variance. mice::pool() has no option
## for a custom vcov, so we do the pooling by hand, term by term, using
## mice::pool.scalar() (the same thing pool() uses internally per coefficient).
Pooling.Models.Robust <- function(models, prefix) {

    cat("\n=== POOLING MODELS (ROBUST HC3 SE) ===\n")
 
    terms <- names(coef(models[[1]]))
    m     <- length(models)
    n     <- nobs(models[[1]])
 
    pooled_rows <- lapply(terms, function(term) {
 
        Q <- vapply(models, function(mod) unname(coef(mod)[term]), numeric(1))
 
        U <- vapply(models, function(mod) {
            vc <- if (!is.null(mod$vcovHC3)) mod$vcovHC3 else vcov(mod)
            vc[term, term]
        }, numeric(1))
 
        p <- mice::pool.scalar(Q, U, n = n, k = 1)
 
        data.frame(
            term      = term,
            estimate  = p$qbar,
            std.error = sqrt(p$t),
            statistic = p$qbar / sqrt(p$t),
            df        = p$df,
            p.value   = 2 * pt(-abs(p$qbar / sqrt(p$t)), df = p$df)
        )
    })
 
    pooled_summary <- bind_rows(pooled_rows)
 
    cat("\n── pooled estimates (robust HC3) ──\n")
    print(pooled_summary)
 
    write.csv(pooled_summary, Output.Path(prefix, "Pooled_Regression_Summary_Robust.csv"), row.names = FALSE)

    return(pooled_summary)
}


Check.Linearity <- function(my_models_without_influent, prefix) {

    cat("\n=== LINEARITY (RAMSEY, MODEL SPECIFICATION) ===\n")

    reset_results <- map_dfr(seq_along(my_models_without_influent), function(i) {

        model     <- my_models_without_influent[[i]]
        reset_raw <- resettest(model)

        data.frame(
            imputation = paste0("imp", i),
            RESET      = round(unname(reset_raw$statistic), 3),
            p_value    = round(reset_raw$p.value, 4),
            conclusion = ifelse(reset_raw$p.value > .05,
                                 "No misspecification detected",
                                 "Possible misspecification")
        )
    })

    write.csv(reset_results, Output.Path(prefix, "Check_RESET_Test.csv"), row.names = FALSE)

    print(reset_results)


    cat("\n=== LINEARITY (RESIDUALS VS FITTED) ===\n")

    ## Plots the residuals against the fitted values and predictors
    ## If most of the two lines overlap (reference line (mean = 0) and conditional mean); no evidence
    ## that assumption of lienarity has been violated
    pdf(Output.Path(prefix, "Linearity_Residuals_vs_Fitted.pdf"))
        for (i in seq_along(my_models_without_influent)) {
            
            ## Plots residuals against fitted values
            model <- my_models_without_influent[[i]]

            plot_df <- data.frame(
                ## fitted values
                yhat = fitted(model),
                ## residuals
                res  = residuals(model)
            )

            p <- ggplot(plot_df, aes(yhat, res)) +
                geom_point() +
                geom_hline(yintercept = 0, color = "red") +
                geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
                labs(title = paste("Residuals vs Fitted - imp", i),
                     x = "Fitted values", y = "Residuals") +
                theme_minimal()

            print(p)
        

            ## Split this model's predictors into continuous vs categorical
            ## bc categorical need to be translated to numeric 
            mf         <- model.frame(model)
            predictors <- attr(terms(model), "term.labels")
            is.num     <- sapply(mf[predictors], is.numeric)
            num.preds  <- predictors[is.num]
            cat.preds  <- predictors[!is.num]

            ## Plots residuals vs predictors
            ## One function for continuous and one for numeric 

            ## Residuals vs each continuous predictor
            for (pred in num.preds) {

                pred_df <- data.frame(x = mf[[pred]], res = residuals(model))

                p2 <- ggplot(pred_df, aes(x, res)) +
                        geom_point() +
                        geom_hline(yintercept = 0, color = "red") +
                        geom_smooth(se = FALSE, method = "loess", formula = y ~ x) +
                        labs(title = paste0("Residuals vs ", pred, " - imp", i),
                            x = pred, y = "Residuals") +
                        theme_minimal()
                print(p2)
            }

            ## Residuals vs each categorical predictor
            ## (as.numeric(as.factor(.)) so a conditional mean line can be drawn;
            ## the underlying variable is still treated as categorical in the model itself)
            for (pred in cat.preds) {

                pred_df <- data.frame(x = as.numeric(as.factor(mf[[pred]])), res = residuals(model))

                p3 <- ggplot(pred_df, aes(x, res)) +
                    geom_point() +
                    geom_hline(yintercept = 0, color = "red") +
                    stat_summary(geom = "line", fun = mean, color = "blue", linewidth = 1.5) +
                    labs(title = paste0("Residuals vs ", pred, " - imp", i),
                         x = pred, y = "Residuals") +
                    theme_minimal()
                print(p3)
            }
        }
    dev.off()



    cat("\nLinearity plots (residuals vs fitted) can be found in Outputs folder; inspect them for non-linearity\n\n")

}


Check.Residuals.Independence <- function(models, prefix) {

    cat("\n=== INDEPENDENCE OF RESIDUALS (DURBIN-WATSON) ===\n")

    dw_results <- map_dfr(seq_along(models), function(i) {

        dw <- durbinWatsonTest(models[[i]])

        data.frame(
            imputation = paste0("imp", i),
            DW        = round(dw$dw, 3),
            p_value   = round(dw$p, 4),
            conclusion = case_when(dw$dw >= 1.5 & dw$dw <= 2.5 ~ "No autocorrelation",
                                    dw$dw < 1.5                  ~ "Positive Autocorrelation",
                                    TRUE                         ~ "Negative Autocorrelation")
            )
    })

     write.csv(dw_results, Output.Path(prefix, "Check_Residuals_Independence.csv"))
}
 
 
Regression <- function(imputed.datasets, imputed.datasets.no.outliers, cohort, prefix, group = NULL, DV, method, IV) {

    ## Keep our subject ids here for when need them
    ## Must mirror the same group filter Fit.Models applies, otherwise subj_ids won't
    ## line up row-for-row with the Cook's distances computed on the filtered data
    subj_ids <- read_rds(imputed.datasets[1])
    if (!is.null(group)) {
        subj_ids <- dplyr::filter(subj_ids, group == !!group)
    }
    subj_ids <- dplyr::select(subj_ids, subj_id)

    ## site & cohort are only meaningful when pooling across the full "all" dataset;
    ## when running on a single dataset (e.g. "SPAD" or "SUICIDE-DECIDE"), they're constant
    ## within that dataset and shouldn't be used as predictors.
    ## This works regardless of whether the caller already included them in IV or not.
    if (cohort == "all") {
        IV <- union(IV, c("site", "cohort"))
    } else {
        IV <- setdiff(IV, c("site", "cohort"))
    }
    cat("\nIVs used for cohort '", cohort, "': ", paste(IV, collapse = ", "), "\n", sep = "")

    ## First, we fit the initial models for all imputations
    my_models <- Fit.Models(imputed.datasets, cohort, group = group, DV, method, IV)

    ## Then we check for outliers with Cook's distance
    ## This function also removes the outliers to give us updated dfs without thos subjects, for each imputation
    ## Subjects are excluded if they are outliers on at least 3 imputations
    imp_dfs_without_influent <- Check.Outliers.Cook(my_models, imputed.datasets, subj_ids, prefix)

    ## Here, we re-fit the models without the outliers 
    my_models_without_influent <- Fit.Models(imputed.datasets.no.outliers, cohort, group = group, DV, method, IV)

    ## Next, we check for outliers with Grubb's test
    ## If it detects outliers, it will quit the environment and you should check the data
    Check.Outliers.Grubb(my_models_without_influent, prefix)

    ## We assess multicollinearity
    ## If it detects outliers, it will quit the environment and you should check the data
    Check.Variance.Inflation.Factor(my_models_without_influent, prefix)

    ## Check that the residuals are normally distributed
    ## using Shapiro Wilk's test and QQ-plot inspection
    Check.Residuals.Normality(my_models_without_influent, prefix)

    ## Check Homoscedasticity with Breusch Pagan test
    ## If heteroscedasticity in 3 or more imputations, compute robust regression models
    homoscedasticity_check <- Check.Homoscedasticity(my_models_without_influent, prefix)
    my_models_without_influent <- homoscedasticity_check$models

    ## Pools our models in one final model, using robust HC3 pooling if needed
    if (homoscedasticity_check$robust) {
        pooled_summary <- Pooling.Models.Robust(my_models_without_influent, prefix)
    } else {
        pooled_summary <- Pooling.Models(my_models_without_influent, prefix)
    }

    ## Check linearity to detect specification errors in the model
    ## Uses Ramsey Regression Equation Specification Error Test (RESET)
    ## Significant p-value = relationship btw predictors and outcomes might not be linear
    Check.Linearity(my_models_without_influent, prefix)

    Check.Residuals.Independence(my_models_without_influent, prefix)
 
    Plots(pooled_summary, prefix)

    cat("\n\n===== All modelling and plots correctly ran! =====\n\n")
}


Plots <- function(pooled_summary, prefix) {
        
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

    ggsave(Output.Path(prefix, "Forest_Plot.pdf"), plot = p)
}