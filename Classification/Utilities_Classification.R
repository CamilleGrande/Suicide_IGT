# Utilities_Classification.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to
#       run the Classification part of the SPAD study


## Model is our actual model output, summary model is its tidy version
Wald.z.test <- function(model, summary_model) {

    ## Significance of predictors (coefficient significance) for model 2
    ## First, compute Wald's z statistic for each coefficient (z = estiamte / SE)
    z <- summary(model)$coefficients / summary(model)$standard.errors
    ## Then, convert z statistic into two tailed p value
    ## pnorm(abs(z)) gives area under the standard normal curve at the left
    ## 1 - pnorm(abs(z)) gives area under the standard normal curve at the right
    ## multiplying by two converts it to a two tailed p value
    p <- 2 * (1 - pnorm(abs(z)))
    cat("\np-value coefficients")
    print(p)

    ## extract p values 
    p_df <- as.data.frame(as.table(p)) |>
        rename(y.level = Var1, term = Var2, p.value = Freq)

    ## add them to summary to make final summary
    summary_model <- summary_model |>
        left_join(p_df, by = c("y.level", "term"))

    return(summary_model)
}


## This function computes relative risk ratios and their 95% CI
## and adds them to our data summary
Relative.Risk.Ratios <- function(model, summary_model) {

    ## Coefficients and SEs from model
    coefs <- summary(model)$coefficients
    ses   <- summary(model)$standard.errors

    ## 95% CI on the log-odds (coefficient) scale
    ci_lower <- coefs - 1.96 * ses
    ci_upper <- coefs + 1.96 * ses

    ## Exponentiate to get RRR and its 95% CI
    rrr <- exp(coefs)
    rrr_lower <- exp(ci_lower)
    rrr_upper <- exp(ci_upper)

    ## rename df so fits our existing summary
    rrr_df <- as.data.frame(as.table(rrr)) |> 
                rename(y.level = Var1, term = Var2, RRR = Freq)
    lower_df <- as.data.frame(as.table(rrr_lower)) |> 
                rename(y.level = Var1, term = Var2, RRR_lower = Freq)
    upper_df <- as.data.frame(as.table(rrr_upper)) |> 
                rename(y.level = Var1, term = Var2, RRR_upper = Freq)

    summary_model <- summary_model |>
                        left_join(rrr_df, by = c("y.level", "term")) |>
                            left_join(lower_df, by = c("y.level", "term")) |>
                                left_join(upper_df, by = c("y.level", "term"))
    
    return(summary_model)
}


## Plot linearity of continuous IVs (K and BDI, so all)
## to the logit transformation of the DV (group)
## n_bins = 4 because arithmically correct to get at least 5 people per bin
## but in reality tails often are n = 1 so should interpret tails w caution
## (i.e., non-linearity in tail might be because only 1 data point here)
Plot.Empirical.Logit <- function(data, predictor, ref, target, n_bins = 4) {

    data |>
        ## ntile() buckets a numeric vector (K or BDI) into groups; 
        ## n groups = nb bins we specify as arg
        mutate(bin = ntile(.data[[predictor]], n_bins)) |>
            ## group data depending on bin nb assignment
            group_by(bin) |>
                summarise(
                        x = mean(.data[[predictor]]),
                        ## computes how many ppl in that bin belong to target grp
                        n_target = sum(.data[["group"]] == target),
                        ## computes how many ppl in that bin belong to ref grp
                        n_ref = sum(.data[["group"]] == ref),
                        ## computes the empirical logit for one bin, i.e. the 
                        ## log-odds of being in the target vs ref category based
                        ## on the actual counts in that bin
                        ## adding 0.5 keeps the calculation finite, as if one group = 0
                        ## in that bin, would give infinite & break the plot
                        logit = log((n_target + 0.5) / (n_ref + 0.5))
                ) |>
                    ggplot(aes(x, logit)) +
                        geom_point(size = 2) +
                        geom_line(color = "black") +
                        geom_smooth(method = "lm", se = F, color = "red", linetype = "dashed") +
                        labs(title = paste(target, "vs", ref, "-", predictor), 
                                x = predictor, y = "logit") +
                        theme_minimal()
}


## Model fitting + Assumptions
Multinomial.Logistic.Regression <- function(data) {

    jena <- read_csv(data)

    cat("\nProportion per group SUICIDE-DECIDE:\n")
    ## Proportion per group
    prop.table(table(jena$group))
    cat("\n\nN per group SUICIDE-DECIDE:\n")
    ## n per group
    xtabs(~ group, data = jena)

    ## -------- MODEL 1 --------
    cat("\n\nFitting Multinomial Logisitc model with depression only\n")
    ## Fitting multinomial logistic with only depression first
    model1 <- multinom(group ~ bdi_zscore, data = jena)
    print(summary(model1))
    summary_model1 <- tidy(model1, conf.int = TRUE)

    ## Compute wald z test and p values to get predictors' significance
    summary_model1 <- Wald.z.test(model1, summary_model1)

    ## get relative risk rations (exponentiated coefficients) and 95% CI
    summary_model1 <- Relative.Risk.Ratios(model1, summary_model1)
    write_csv(summary_model1, "./Classification/Outputs/Summary_Model1.csv")


    ## -------- MODEL 2 --------
    cat("\n\nFitting Multinomial Logisitc model with depression and K\n")
    ## Fitting multinomial logistic with depression and K
    model2 <- multinom(group ~ K_z + bdi_zscore, data = jena)
    ## Look at AIC, should decrease in model 2
    print(summary(model2))
    summary_model2 <- tidy(model2, conf.int = TRUE)

    ## Compute wald z test and p values to get predictors' significance
    summary_model2 <- Wald.z.test(model2, summary_model2)
    ## get relative risk rations (exponentiated coefficients) and 95% CI
    summary_model2 <- Relative.Risk.Ratios(model2, summary_model2)
    write_csv(summary_model2, "./Classification/Outputs/Summary_Model2.csv")

    ## Difference between models using likelihood ratio tests
    ## answers the question "does adding K to the model improves its fit?",
    ## i.e. add predictive value to group membership
    model_comp <- anova(model1, model2, test = "Chisq")
    print(model_comp)
    write_csv(model_comp, "./Classification/Outputs/Comparison_model_1_2.csv")


    ## -------- Model assumptions --------

    ## Multicollinearity
    cat("\n\nTesting assumptions\n1. No multicollinearity between IVs:\n")
    ## Assess correlation between predictors (because only 2 predictors)
    pdf("./Classification/Outputs/Correlation_BDI_K.pdf")
        print(
            ggplot(jena, aes(x = K_z, y = bdi_zscore)) +
                    geom_point() +
                    labs(x = "K z-scored", y = "BDI z-scored") +
                    geom_smooth(method="lm") +
                    theme_minimal()
        )
    dev.off()
    cat("---> plots saved!\n")

    # Calculating Pearson's product-moment correlation
    my_cor <- cor.test(jena$K_z, jena$bdi_zscore, method = "pearson", conf.level = 0.95)
    cat("\n\n---> Pearson correlation:n\n")
    print(my_cor)

    ## Linear relationship between continuous IVs and logit transformation of DV
    cat("\n\n2. Linear relationship between continuous predictors and logit transformation of the DV:\n")
    pdf("./Classification/Outputs/Linearity_w_logit.pdf")
        print(Plot.Empirical.Logit(jena, "K_z", "HC", "PC"))
        print(Plot.Empirical.Logit(jena, "K_z", "HC", "SA"))
        print(Plot.Empirical.Logit(jena, "bdi_zscore", "HC", "PC"))
        print(Plot.Empirical.Logit(jena, "bdi_zscore", "HC", "SA"))
    dev.off()
    cat("---> plots saved!\n")

    ## Outliers on continuous variables
    cat("\n\n3. No outliers / highly influential obs on continuous variables:\n")
    k <- sum(abs(jena$K_z) > 3.29)
    bdi <- sum(abs(jena$bdi_zscore) > 3.29)
    cat("\nSum |K_z| > 3.29: ", k, "\nSum |bdi_z| > 3.29: ", bdi)
    cat("\n\nAll assumptions done!\n")

    return(list(bdi_only = model1, bdi_and_K = model2))
}


Predict.Group <- function(data, model) {

    ## First, make sure group is coded as factor
    data$group <- as.factor(data$group)

    ## Predict group based on model
    test_pred_multi <- predict(model, newdata = data)
    test_pred_multi <- factor(test_pred_multi, levels = levels(data$group))

    ## Gives confusion matrix for classification accuracy
    confusion_matrix <- caret::confusionMatrix(test_pred_multi, data$group)
    cat("\n\nConfusion matrix:\n")
    print(confusion_matrix)
}


## This function creates a data frame for the ROC objects to plot AUC curves
ROC.df <- function(ROC.obj, group, sample) {

    df <- data.frame(
            specificity = ROC.obj$specificities,
            sensitivity = ROC.obj$sensitivities,
            group = group,
            sample = sample,
            AUC_label = paste0(group, " (AUC = ", round(auc(ROC.obj), 3), ")")
        )

    return(df)
}


## runs the prediction and ROC/AUC part
Apply.Model <- function(data.jena, data.spad, model1, model2) {

    bdi_only <- model1
    bdi_and_k <- model2

    ## ---- Training sample ----
    jena <- read_csv(data.jena)

    ## Model 1 (bdi only)
    Predict.Group(jena, bdi_only)

    ## Model 2 (bdi and K)
    Predict.Group(jena, bdi_and_k)


    ## ---- Validation sample ----
    spad <- read_rds(data.spad)
    spad$K_z <- as.numeric(scale(spad$K))

    ## Model 2 only as performed best
    Predict.Group(spad, bdi_and_k)


    ## ---- ROC/AUC ----

    ## get predicted probabilities for each sample
    train_probs <- predict(bdi_and_k, newdata = jena, type = "probs")
    valid_probs <- predict(bdi_and_k, newdata = spad, type = "probs")

    ## compute one-vs-rest ROC for each group and each sample
    roc_train_HC <- roc(response = jena$group == "HC", predictor = train_probs[, "HC"])
    roc_train_PC <- roc(response = jena$group == "PC", predictor = train_probs[, "PC"])
    roc_train_SA <- roc(response = jena$group == "SA", predictor = train_probs[, "SA"])

    roc_valid_HC <- roc(response = spad$group == "HC", predictor = valid_probs[, "HC"])
    roc_valid_PC <- roc(response = spad$group == "PC", predictor = valid_probs[, "PC"])
    roc_valid_SA <- roc(response = spad$group == "SA", predictor = valid_probs[, "SA"])

    ## create AUC table and export it
    auc_table <- data.frame(
        Group = c("HC", "PC", "SA"),
        Training_AUC = c(auc(roc_train_HC), auc(roc_train_PC), auc(roc_train_SA)),
        Validation_AUC = c(auc(roc_valid_HC), auc(roc_valid_PC), auc(roc_valid_SA))
    )
    print(auc_table)
    write_csv(auc_table, "./Classification/Outputs/AUC.csv")

    ## create full ROC df (with ROC for each group and each sample)
    ROC_summary <- rbind(
                    ROC.df(roc_train_HC, "HC", "Training"),
                    ROC.df(roc_train_PC, "PC", "Training"),
                    ROC.df(roc_train_SA, "SA", "Training"),
                    ROC.df(roc_valid_HC, "HC", "Validation"),
                    ROC.df(roc_valid_PC, "PC", "Validation"),
                    ROC.df(roc_valid_SA, "SA", "Validation")
                )
    
    ROC_summary$sample <- factor(ROC_summary$sample, levels = c("Training", "Validation"))

    ## plot curves
    ## Training sample
    pdf("./Classification/Outputs/ROC_Training.pdf")
        plot(roc_train_HC, col = "#A4D984FF", lwd = 3, main = "ROC Curves by Group (One-vs-Rest) Training Sample")
        plot(roc_train_PC, col = "#FCBC52FF", lwd = 3, add = TRUE)
        plot(roc_train_SA, col = "#F588AFFF", lwd = 3, add = TRUE)

        legend("bottomright",
            legend = c(paste0("HC (AUC = ", round(auc(roc_train_HC), 3), ")"),
                        paste0("PC (AUC = ", round(auc(roc_train_PC), 3), ")"),
                        paste0("SA (AUC = ", round(auc(roc_train_SA), 3), ")")),
            col = c("#A4D984FF", "#FCBC52FF", "#F588AFFF"),
            lwd = 3, bty = "n")

        abline(a = 1, b = -1, lty = 2, col = "gray60")
    dev.off()

    ## Validation sample
    pdf("./Classification/Outputs/ROC_Validation.pdf")
        plot(roc_valid_HC, col = "#A4D984FF", lwd = 3, main = "ROC Curves by Group (One-vs-Rest) Validation Sample")
        plot(roc_valid_PC, col = "#FCBC52FF", lwd = 3, add = TRUE)
        plot(roc_valid_SA, col = "#F588AFFF", lwd = 3, add = TRUE)

        legend("bottomright",
            legend = c(paste0("HC (AUC = ", round(auc(roc_valid_HC), 3), ")"),
                        paste0("PC (AUC = ", round(auc(roc_valid_PC), 3), ")"),
                        paste0("SA (AUC = ", round(auc(roc_valid_SA), 3), ")")),
            col = c("#A4D984FF", "#FCBC52FF", "#F588AFFF"),
            lwd = 3, bty = "n")

        abline(a = 1, b = -1, lty = 2, col = "gray60")
    dev.off()
}


Run.Classification <- function(data.jena, data.spad) {

    models <- Multinomial.Logistic.Regression(data.jena)

    model_bdi <- models$bdi_only
    model_bdi_K <- models$bdi_and_K

    Apply.Model(data.jena, data.spad, model_bdi, model_bdi_K)

    cat("\nAll classification script ran!\n")
}