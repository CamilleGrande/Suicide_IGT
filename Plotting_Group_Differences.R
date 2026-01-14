# Plotting_Group_Differences.R
# Author: Nathaniel Haines (slightly adapted with permission)
# Study: SPAD
# Description: 
#       This script holds the functions to plot the group differences 

library(hBayesDM)
library(rstan)
library(bayestestR)
library(ggplot2)
library(reshape2)
library(ggpubr)
library(cowplot)

source("Plotting_Utilities.R")

#source("R/utils/stan_rhat.R")
#source("R/utils/plot_diff_HDI_info.R")
#source("R/utils/plot_diff.R")



## Loads each fit results per group
## usage example: Load.Data.Fit(orl.group0, orl.group1, orl.group1)
Load.Data.Fit <- function(group0, group1, group2) {
    group0_parVals <- group0$parVals
    group1_parVals <- group1$parVals
    group2_parVals <- group2$parVals

}


## A enlever?
# title <- c(expression(A["+"]), expression(A["-"]), expression(K), 
#            expression(beta[F]), expression(beta[P]))

# p1 <- plot_diff_info(group0_parVals, group1_parVals, title = title, cols = 1)
# p2 <- plot_diff_info(group0_parVals, group2_parVals, title = title, cols = 1)
# p3 <- plot_diff_info(group1_parVals, group2_parVals, title = title, cols = 1)


# p_all <- cowplot::plot_grid(p1, p2, p3, ncol = 3)
# ggsave(filename="~/Desktop/group_compare.pdf", plot = p_all)



## Plots group differences for the ORL parameters
## usage example: ORL.Group.Differences(orl.group0, orl.group1, orl.group1)
ORL.Group.Differences <- function(group0, group1, group2) {
    
    Load.Data.Fit(group0, group1, group2)
    
    ## Focus on group level parameters
    mu_pars <- c("mu_Arew", "mu_Apun", "mu_K", "mu_betaF", "mu_betaP")

    ## Our three groups
    groups  <- c("Healthy Controls", "Patient Controls", "Suicide Attempters")


    mu_Arew <- mu_Apun <- mu_Adiff <- mu_K <- mu_betaF <- mu_betaP <- data.frame(Arew = rep(NA, 10000), 
                                                                                Apun = rep(NA, 10000),
                                                                                Adiff = rep(NA, 10000),
                                                                                K = rep(NA, 10000),
                                                                                betaF = rep(NA, 10000),
                                                                                betaP = rep(NA, 10000))

    ## Creates empty list to hold our data
    all_dat <- list()

    ## Creates a list that assigns each Group to its data
    all_fits <- list("Healthy Controls" = group0_parVals, "Patient Controls" = group1_parVals, "Suicide Attempters" = group2_parVals)

    for (i in mu_pars) {
    for (j in groups) {
        all_dat[[i]][[j]] <- all_fits[[j]][[i]]
    }
    }



    Arew_m <- melt(all_dat$mu_Arew); names(Arew_m) <- c("iter", "mu_Arew", "Group")

    Apun_m <- melt(all_dat$mu_Apun); names(Apun_m) <- c("iter", "mu_Apun", "Group")

    Adiff_m <- melt(list("Healthy Controls" = all_dat$mu_Arew$"Healthy Controls" - all_dat$mu_Apun$"Healthy Controls",
                        "Patient Controls" = all_dat$mu_Arew$"Patient Controls" - all_dat$mu_Apun$"Patient Controls",
                        "Suicide Attempters" = all_dat$mu_Arew$"Suicide Attempters" - all_dat$mu_Apun$"Suicide Attempters")); names(Adiff_m) <- c("iter", "mu_Adiff", "Group")


    K_m <- melt(all_dat$mu_K); names(K_m) <- c("iter", "mu_K", "Group")

    betaF_m <- melt(all_dat$mu_betaF); names(betaF_m) <- c("iter", "mu_betaF", "Group")

    betaP_m <- melt(all_dat$mu_betaP); names(betaP_m) <- c("iter", "mu_betaP", "Group")



    Arew_m$Group <- Apun_m$Group <- K_m$Group <- betaF_m$Group <- betaP_m$Group <-  Adiff_m$Group <-
    factor(Arew_m$Group, 
            levels = c("Healthy Controls", "Patient Controls", "Suicide Attempters"), 
            labels = c("Healthy Controls", "Patient Controls", "Suicide Attempters"))


    h1 <- ggplot(Arew_m, aes(x = mu_Arew, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    scale_color_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    geom_density(alpha = 0.5) + 
    ggtitle(expression(A["+"])) + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(legend.position = "none", 
            plot.title = element_text(size = 20))



    h2 <- ggplot(Apun_m, aes(x = mu_Apun, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    scale_color_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    geom_density(alpha = 0.5) + 
    ggtitle(expression(A["-"])) + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(legend.position = "none", 
            plot.title = element_text(size = 20))


    h3 <- ggplot(Adiff_m, aes(x = mu_Adiff, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    scale_color_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    geom_density(alpha = 0.5) +
    ggtitle(expression(A[diff])) + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(legend.position = "none", 
            plot.title = element_text(size = 20))   


    h4 <- ggplot(K_m, aes(x = mu_K, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    scale_color_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    geom_density(alpha = 0.5) + 
    ggtitle(expression(K)) + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(legend.position = "none", 
            plot.title = element_text(size = 20))


    h5 <- ggplot(betaF_m, aes(x = mu_betaF, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    scale_color_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    geom_density(alpha = 0.5) + 
    ggtitle(expression(beta[F])) + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(legend.position = "none", 
            plot.title = element_text(size = 20))

            
    h6 <- ggplot(betaP_m, aes(x = mu_betaP, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    scale_color_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    geom_density(alpha = 0.5) + 
    ggtitle(expression(beta[P])) + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(legend.position = "right", 
            plot.title = element_text(size = 20))
    


    h_key <- ggplot(betaP_m, aes(x = mu_betaP, group = Group, color = Group, fill = Group)) +
    scale_fill_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    scale_color_manual(values = c("Healthy Controls" = "#5be440ff", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
    geom_density(alpha = 0.5) + 
    ggtitle(expression(beta[P])) + 
    xlab("") +
    ylab("") +
    theme_classic() +
    theme(plot.title = element_text(size = 20))


    h_all <- ggarrange(h1, h2, h3, h4, h5, h6, ncol=2, nrow=3, common.legend= T, legend="right")
    ggsave(filename="./group_pars.pdf", plot = h_all)
    ggsave(filename="./group_pars_key.pdf", plot = h_key)
}

HDI.Group.Differences <- function(group0, group1, group2) {
    
    fit.dataframe <- as.data.frame(Load.Data.Fit(group0, group1, group2))

    diff <- group0_parVals$mu_Arew - group1_parVals$mu_Arew
    hdi  <- HDIofMCMC(diff)
    hdi.annotation.low <- toString(round(hdi[1], 3))
    hdi.annotation.up <- toString(round(hdi[2], 3))

    # might remove plot title and rather put a title once all plots together
    d1 <- ggplot(data = diff.df, aes(x = diff)) +
        geom_density(data = diff.df, color = "#5e5e5eff", fill = "#8f8e8fff", alpha = 0.5) +
        geom_segment(x = 0, xend = 0, y = 0, yend = Inf, size = 1.5, colour = "#c52982ff", linetype = 2) +
        geom_segment(aes(x = hdi[1], y = 0, xend = hdi[2], yend = 0), size = 1.5, colour = "#c52982ff") +
        annotate("text", x = I(0.1), y = I(0.8), label = hdi.annotation.low) + 
        annotate("text", x = I(0.8), y = I(0.8), label = hdi.annotation.up) +        
        ggtitle("Control - Depressed") + 
        xlab("") +
        ylab("") +
        theme_classic(base_size = 20) +
        theme(
            plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text = element_text(color = "#6a6a6aff"),
            axis.ticks = element_line(color = "#ffffffff"),
            axis.line = element_line(color = "#ffffffff")
            ) 

        
}