# Plotting_Utilities.R
# Author: Camille Grandé
# Study: SPAD
# Description: 
#       This script holds the utility functions necessary to plot results of the modelling of the IGT task in the SPAD data

# Plot.Groups("./4-final_model/ORL.HC_parVals.csv", "./4-final_model/ORL.PC_parVals.csv", "./4-final_model/ORL.SA_parVals.csv")
Plot.Groups <- function(csv.HC, csv.PC, csv.SA) {
    
    data.HC <- read.csv(csv.HC, header = T) # check if gives df? Also check separator//took out separator
    data.PC <- read.csv(csv.PC, header = T)
    data.SA <- read.csv(csv.SA, header = T)

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

# check that takes the right columns bc didn't slice it beforehand
    for (i in mu_pars) {
        for (j in groups) {
            all_dat[[i]][[j]] <- all_fits[[j]][[i]]
        }
    }

    # took out iter
    Arew_m <- melt(all_dat$mu_Arew); names(Arew_m) <- c("mu_Arew", "Group")
    Apun_m <- melt(all_dat$mu_Apun); names(Apun_m) <- c("mu_Apun", "Group")
    Adiff_m <- melt(list("Healthy Controls" = all_dat$mu_Arew$"Healthy Controls" - all_dat$mu_Apun$"Healthy Controls",
                        "Patient Controls" = all_dat$mu_Arew$"Patient Controls" - all_dat$mu_Apun$"Patient Controls",
                        "Suicidants" = all_dat$mu_Arew$"Suicidants" - all_dat$mu_Apun$"Suicidants")); names(Adiff_m) <- c("mu_Adiff", "Group")
    K_m <- melt(all_dat$mu_K); names(K_m) <- c("mu_K", "Group")
    betaF_m <- melt(all_dat$mu_betaF); names(betaF_m) <- c("mu_betaF", "Group")
    betaP_m <- melt(all_dat$mu_betaP); names(betaP_m) <- c("mu_betaP", "Group")


    Arew_m$Group <- Apun_m$Group <- K_m$Group <- betaF_m$Group <- betaP_m$Group <-  Adiff_m$Group <-
    factor(Arew_m$Group, 
            levels = c("Healthy Controls", "Patient Controls", "Suicidants"), 
            labels = c("Healthy Controls", "Patient Controls", "Suicidants"))


    h1 <- ggplot(Arew_m, aes(x = mu_Arew, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.7) + 
        ggtitle(expression(A["+"])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))



    h2 <- ggplot(Apun_m, aes(x = mu_Apun, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.7) + 
        ggtitle(expression(A["-"])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))


    h3 <- ggplot(Adiff_m, aes(x = mu_Adiff, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.7) +
        ggtitle(expression(A[diff])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))   


    h4 <- ggplot(K_m, aes(x = mu_K, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.7) + 
        ggtitle(expression(K)) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))


    h5 <- ggplot(betaF_m, aes(x = mu_betaF, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.7) + 
        ggtitle(expression(beta[F])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "none", 
            plot.title = element_text(size = 20))

            
    h6 <- ggplot(betaP_m, aes(x = mu_betaP, group = Group, color = Group, fill = Group)) +
        scale_fill_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        scale_color_manual(values = c("Healthy Controls" = "#A4D984FF", "Patient Controls" = "#FCBC52FF", "Suicidants" = "#F588AFFF")) + 
        geom_density(alpha = 0.7) + 
        ggtitle(expression(beta[P])) + 
        xlab("") +
        ylab("") +
        theme_classic() +
        theme(legend.position = "right", 
            plot.title = element_text(size = 20))

    h_all <- ggarrange(h1, h2, h3, h4, h5, h6, ncol=2, nrow=3, common.legend= T, legend="right")
    
    ggsave(filename="./4-final_model/Plot_Parameters_PerGroup.pdf", plot = h_all)
}



## Plot.HDI(ORL.HC$parVals, ORL.PC$parVals, ORL.SA$parVals)

## After model fitting is complete for both groups,
## evaluate the group difference (e.g., on the 'pi' parameter) by examining the posterior distribution of group mean differences.

#diffDist = output_group1$parVals$mu_pi - output_group2$parVals$mu_pi  # group1 - group2
#HDIofMCMC( diffDist )  # Compute the 95% Highest Density Interval (HDI).
#plotHDI( diffDist )    # plot the group mean differences
Plot.HDI <- function(csv.HC, csv.PC, csv.SA) {
    
    data.HC <- read.csv(csv.HC, header = T) # check if gives df? Also check separator//took out separator
    data.PC <- read.csv(csv.PC, header = T)
    data.SA <- read.csv(csv.SA, header = T)

    my.list <- list("Healthy Controls" = data.HC, "Patient Controls" = data.PC, "Suicidants" = data.SA)
    mu.list <- lapply(my.list, function(x) {
        x[grep("^mu_", names(x))]
    })
    
    mu.list <- lapply(mu.list, function(df) {
                df <- cbind(df[ , 1:2], mu_Adiff = df$mu_Arew - df$mu_Apun, df[ , 3:5])
            })

    Comparisons <- list(
        "Healthy Controls - Patient Controls" = c("Healthy Controls", "Patient Controls"),
        "Healthy Controls - Suicidants" = c("Healthy Controls", "Suicidants"),
        "Patient Controls - Suicidants" = c("Patient Controls", "Suicidants"))

    mu_pars <- c("mu_Arew", "mu_Apun", "mu_Adiff", "mu_K", "mu_betaF", "mu_betaP")

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
            geom_vline(xintercept = 0,linetype = 2, linewidth = 0.8, colour = "#434475FF") +
            geom_segment(data = hdi_df, aes(x = low, xend = high, y = 0, yend = 0), inherit.aes = FALSE, linewidth = 2, colour = "#434475FF") +
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




Plot.LOOIC <- function() {
    
cat("\nModel comparison values (LOOIC and values)\nLower values indicate better model performance\n\n")
    

    df <- as.data.frame(rbind(x[c(1,2,3),2]))
    colnames(df) <- rbind("PVLdelta", "PVLdecay", "ORL")

    ggplot(data = df, aes(x = df,))
    
    }