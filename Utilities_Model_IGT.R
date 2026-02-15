# Utilities_Model_IGT.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to run the Driver Script for the modelling of the IGT task



## This function creates the tables for each group in our main file
## It is incorporated in the data loading function
Table.Per.Group <- function(my.data) {

    ## Create a .txt file for each group's data 
    ## These files will be used in our final model
    for (i in unique(my.data$Group)) {

        ## Extract data of the group, and keep only relevant columns
        data_group <- my.data[my.data$Group == i,]
        data_group <- data_group[, c("subjID", "choice", "gain", "loss")]

        ## Write .txt file, to be found in ./1-data directory
        write.table(        
            data_group,        
            file = paste0("./1-data/group_", i, ".txt"),        
            sep = "\t",        
            row.names = FALSE,        
            quote = FALSE        
            )

    }
}

## This function loads our data, and creates the main .txt file
## and .txt files for each group (for the final modeling part)
## Usage example: Loading.Data("./1-data/IGT_data_modelling.csv")
Loading.Data <- function(file) {

    ## Checks files do not already exist and .csv original file present
    if (file.exists("./1-data/all_groups.txt") & file.exists("./1-data/group_0.txt")) {
        return(cat("\nData is already loaded, moving on to the next step\n"))
    }
    if (!file.exists(file)) {
        return(cat("\nCannot find .csv file. Please make sure the data .csv file in in the data folder\n"))
    }

    ## Reads file and keep only relevant columns
    ## Make sure columns are correctly named in .csv file, or it will not work
    ## These names are a prerequisite for the modelling part too
    igt_data <- read.csv(file, header = T, sep = ";")
    all_groups <- igt_data[, c("subjID", "choice", "gain", "loss")]
    
    ## Check your data :)
    cat("_________________________________\n\n")
    cat("Is the data well formatted?\n")
    cat(c("Output should be similar to:\n", "'data.frame': 5700 obs. of 4 variables:\n", "$ subjID: chr ...\n",
        "$ choice: int ...\n", "$ gain: int ...\n", "$ loss: int ...\n\n"))
    cat(str(all_groups), "\n")
    cat("_________________________________\n\n")

    ## Write .txt file, to be found in ./1-data directory
    write.table(
        all_groups,
        file = "./1-data/all_groups.txt",
        sep = "\t",
        row.names = FALSE,
        quote = FALSE
        )

    ## Writes .txt files per group
    Table.Per.Group(igt_data)

}


## This function fits a given model
## The model has to be precised in the first argument
## It is incorporated in Model fitting and Final fitting functions
Model.Fitting <- function(igt.model, my.txt.file, my.iterations, my.warmups) {

    ## Adapt number of iterations and warmup as needed in command line arguments
    my.model <- igt.model(
        data = my.txt.file,
        niter = my.iterations,
        nwarmup = my.warmups,
        nchain = 4,
        ncore = 4
    )

    return(my.model)
}


## This function gives Rhat values for a given model
## It is incorporated in Model fitting and Final fitting functions
my.rhat <- function(my.model) {
    cat("\n________________________\n")
    cat("Check Rhat values: should be less or equal to 1.1\n")
    return(rhat(my.model))
}


## This function creates a trace plot to check our model's chain convergence
## It is incorporated in Model fitting and Final fitting functions
Fitting.Plots <- function(my.model, directory, model.name) {
    trace.plot <- plot(my.model, type = "trace", fontSize=11)
    ggsave(filename= paste0(directory, model.name, "_Trace.pdf"), plot = trace.plot)

    ## For now, not plotting posterior distributions of group-level parameters. If want to plot them, have to make one plot for each parameter but parameters differ between models
    ## If a parameter is of interest, i can plot it per individual with
    ## plotInd(mymodel, parameterIwanttoplot)
    #parameter.plot <- plot(my.model)
    #ggsave(filename= paste0("./2-model_fitting/Plot_", model.name, "_Parameters.pdf"), plot = parameter.plot)
}


## This function saves our model's group level indices in a .csv file
## It is incorporated in Model fitting and Final fitting functions
Save.Indices <- function(my.model, directory, model.name) {
    cat("Indices per participant can be found in a .csv file in the 2-model_fitting directory\n")
    write.csv(my.model$allIndPars, paste0(directory, model.name, "_allIndPars.csv"))
    cat("\n\n________________________\n")
}


## This function saves our model's individual fit indices in a .csv file
## It is incorporated in the Final fitting function
Save.Fit <- function(my.model, directory, model.name) {
    cat("All parameter values per participant can be found in a .csv file in the 4-final_model directory\n")
    write.csv(my.model$fit, paste0(directory, model.name, "_fit.csv"))
    cat("\n\n________________________\n")
}



## This function fits all our different models
## Usage example: Model.Comparison("./1-data/all_groups.txt", 4000, 2000)
Model.Comparison <- function(my.txt.file, my.iterations, my.warmups) {

    cat("\n\n________________________\n")
    cat("Starting PVL delta fit\n\n")

    fit.PVLdelta <- Model.Fitting(igt_pvl_delta, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.PVLdelta)
    Fitting.Plots(fit.PVLdelta, "./2-model_fitting/Plot_", "PVLdelta")
    Save.Indices(fit.PVLdelta, "./2-model_fitting/", "PVLdelta")


    cat("\n\n________________________\n")
    cat("Starting PVL decay fit\n\n")

    fit.PVLdecay <- Model.Fitting(igt_pvl_decay, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.PVLdecay)
    Fitting.Plots(fit.PVLdecay, "./2-model_fitting/Plot_", "PVLdecay")
    Save.Indices(fit.PVLdecay, "./2-model_fitting/", "PVLdecay")


    cat("\n\n________________________\n")
    cat("Starting ORL fit\n\n")

    fit.ORL <- Model.Fitting(igt_orl, my.txt.file, my.iterations, my.warmups)
    my.rhat(fit.ORL)
    Fitting.Plots(fit.ORL, "./2-model_fitting/Plot_", "ORL")
    Save.Indices(fit.ORL, "./2-model_fitting/", "ORL")

    cat("\n\nModel comparison values (LOOIC and values)\nLower values indicate better model performance\n\n")
    return(printFit(fit.PVLdelta, fit.PVLdecay, fit.ORL))

}


Plot.Groups <- function(data.HC, data.PC, data.SA) {
    
    ## Focus on group level parameters
    mu_pars <- c("mu_Arew", "mu_Apun", "mu_K", "mu_betaF", "mu_betaP")

    ## Our three groups
    groups  <- c("Healthy Controls", "Patient Controls", "Suicidants")


    mu_Arew <- mu_Apun <- mu_Adiff <- mu_K <- mu_betaF <- mu_betaP <- data.frame(Arew = rep(NA, 10000), 
                                                                                Apun = rep(NA, 10000),
                                                                                Adiff = rep(NA, 10000),
                                                                                K = rep(NA, 10000),
                                                                                betaF = rep(NA, 10000),
                                                                                betaP = rep(NA, 10000))

    ## Creates empty list to hold our data
    all_dat <- list()

    ## Creates a list that assigns each Group to its data
    all_fits <- list("Healthy Controls" = data.HC, "Patient Controls" = data.PC, "Suicidants" = data.SA)

    for (i in mu_pars) {
        for (j in groups) {
            all_dat[[i]][[j]] <- all_fits[[j]][[i]]
        }
    }

    Arew_m <- melt(all_dat$mu_Arew); names(Arew_m) <- c("iter", "mu_Arew", "Group")
    Apun_m <- melt(all_dat$mu_Apun); names(Apun_m) <- c("iter", "mu_Apun", "Group")
    Adiff_m <- melt(list("Healthy Controls" = all_dat$mu_Arew$"Healthy Controls" - all_dat$mu_Apun$"Healthy Controls",
                        "Patient Controls" = all_dat$mu_Arew$"Patient Controls" - all_dat$mu_Apun$"Patient Controls",
                        "Suicidants" = all_dat$mu_Arew$"Suicidants" - all_dat$mu_Apun$"Suicidants")); names(Adiff_m) <- c("iter", "mu_Adiff", "Group")
    K_m <- melt(all_dat$mu_K); names(K_m) <- c("iter", "mu_K", "Group")
    betaF_m <- melt(all_dat$mu_betaF); names(betaF_m) <- c("iter", "mu_betaF", "Group")
    betaP_m <- melt(all_dat$mu_betaP); names(betaP_m) <- c("iter", "mu_betaP", "Group")


    Arew_m$Group <- Apun_m$Group <- K_m$Group <- betaF_m$Group <- betaP_m$Group <-  Adiff_m$Group <-
    factor(Arew_m$Group, 
            levels = c("Healthy Controls", "Patient Controls", "Suicidants"), 
            labels = c("Healthy Controls", "Patient Controls", "Suicidants"))


    h1 <- ggplot(Arew_m, aes(x = mu_Arew, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.5) + 
        ggtitle(expression(A["+"])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))



    h2 <- ggplot(Apun_m, aes(x = mu_Apun, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.5) + 
        ggtitle(expression(A["-"])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))


    h3 <- ggplot(Adiff_m, aes(x = mu_Adiff, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.5) +
        ggtitle(expression(A[diff])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))   


    h4 <- ggplot(K_m, aes(x = mu_K, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.5) + 
        ggtitle(expression(K)) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))


    h5 <- ggplot(betaF_m, aes(x = mu_betaF, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.5) + 
        ggtitle(expression(beta[F])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))

            
    h6 <- ggplot(betaP_m, aes(x = mu_betaP, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.5) + 
        ggtitle(expression(beta[P])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "right", 
            plot.title = element_text(size = 20))

    h_all <- ggarrange(h1, h2, h3, h4, h5, h6, ncol=2, nrow=3, common.legend= T, legend="right")
    
    ggsave(filename="./4-final_model/Plot_ParametersPerGroup.pdf", plot = h_all)

}

## Plot.HDI(ORL.HC$parVals, ORL.PC$parVals, ORL.SA$parVals)
Plot.HDI <- function(data.HC, data.PC, data.SA) {
    
    my.list <- list("Healthy Controls" = data.HC, "Patient Controls" = data.PC, "Suicidants" = data.SA)
    mu.list <- lapply(my.list, function(x) {
        x[grep("^mu_", names(x))]
    })

    Comparisons <- list(
        "Healthy Controls - Patient Controls" = c("Healthy Controls", "Patient Controls"),
        "Healthy Controls - Suicidants" = c("Healthy Controls", "Suicidants"),
        "Patient Controls - Suicidants" = c("Patient Controls", "Suicidants"))

    mu_pars <- c("mu_Arew", "mu_Apun", "mu_K", "mu_betaF", "mu_betaP")

    all_data <- list()
    hdi_data <- list()

    # Build dataframe to be used in plotting
    for (p in mu_pars) {
        for (comp in names(Comparisons)) {

        g1 <- Comparisons[[comp]][1]
        g2 <- Comparisons[[comp]][2]

        p_g1 <- mu.list[[g1]][[p]]
        p_g2 <- mu.list[[g2]][[p]]

        diff <- p_g1 - p_g2

        df <- data.frame(diff = diff, Parameter = p, Comparison = comp)
        all_data[[length(all_data) + 1]] <- df

        # HDI related
        hdi <- HDIofMCMC(diff)

        hdi_data[[length(hdi_data) + 1]] <- data.frame(
            Parameter = p,
            Comparison = comp,
            low = round(hdi[1], 3),
            high = round(hdi[2], 3))
        }
    }

    plot_df <- bind_rows(all_data)
    hdi_df  <- bind_rows(hdi_data)
    
    p <- ggplot(plot_df, aes(x = diff)) +
            geom_density(color = "#6B6CA3FF", fill = "#969BC7FF", alpha = 0.6) +
            geom_vline(xintercept = 0,linetype = 2, size = 0.8, colour = "#434475FF") +
            geom_segment(data = hdi_df, aes(x = low, xend = high, y = 0, yend = 0), inherit.aes = FALSE, size = 2, colour = "#434475FF") +
            geom_text(data = hdi_df, aes(x = I(0.1), y = I(0.8), label = low, fontface = "bold")) +
            geom_text(data = hdi_df, aes(x = I(0.8), y = I(0.8), label = high, fontface = "bold")) +
            facet_grid2(Parameter ~ Comparison, scales = "free", independent = "all") +
            theme_classic(base_size = 16) +
            theme(axis.line = element_line(color = "#6a6a6aff"), 
                    axis.text = element_text(color = "#6a6a6aff"),
                    axis.ticks = element_line(color = "#6a6a6aff"),
                    strip.text.x = element_text(face = "bold")) +
            xlab("Group Difference") +
            ylab("Density")
        

        pdf("./4-final_model/HDI_group_comp.pdf", width = 12,height = 16)
            print(p)
        dev.off()
}


## This function fits our final model
## Here, the winning model is the ORL model. Adapt code accordingly to your winning model
Final.Model <- function(my.iterations, my.warmups) {

    cat("\n\n________________________\n")
    cat("Starting ORL final fit\n\n")

    ORL.HC <- Model.Fitting(igt_orl, "./1-data/group_0.txt", my.iterations, my.warmups)
    my.rhat(ORL.HC)
    Fitting.Plots(ORL.HC, "./4-final_model/Plot_", "ORL.HC")
    Save.Indices(ORL.HC, "./4-final_model/", "ORL.HC")
    Save.Fit(ORL.HC, "./4-final_model/", "ORL.HC")

    ORL.PC <- Model.Fitting(igt_orl, "./1-data/group_1.txt", my.iterations, my.warmups)
    my.rhat(ORL.PC)
    Fitting.Plots(ORL.PC, "./4-final_model/Plot_", "ORL.PC")
    Save.Indices(ORL.PC, "./4-final_model/", "ORL.PC")
    Save.Fit(ORL.HC, "./4-final_model/", "ORL.PC")

    ORL.SA <- Model.Fitting(igt_orl, "./1-data/group_2.txt", my.iterations, my.warmups)
    my.rhat(ORL.SA)
    Fitting.Plots(ORL.HC, "./4-final_model/Plot_", "ORL.SA")
    Save.Indices(ORL.HC, "./4-final_model/", "ORL.SA")
    Save.Fit(ORL.HC, "./4-final_model/", "ORL.SA")

    Plot.Groups(ORL.HC$parVals, ORL.PC$parVals, ORL.SA$parVals)

    Plot.HDI(ORL.HC$parVals, ORL.PC$parVals, ORL.SA$parVals)
}








Plot.LOOIC <- function() {
    
cat("\nModel comparison values (LOOIC and values)\nLower values indicate better model performance\n\n")
    x <- printFit(fit.PVLdelta, fit.PVLdecay, fit.ORL)
    print(x)

    df <- as.data.frame(rbind(x[c(1,2,3),2]))
    colnames(df) <- rbind("PVLdelta", "PVLdecay", "ORL")

    ggplot(data = df, aes(x = df,))
    
    }
