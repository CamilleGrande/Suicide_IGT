# Plotting_Group_Differences.R
# Author: Nathaniel Haines (slightly adapted with permission)
# Study: SPAD
# Description: 
#       This script holds the functions to plot the group differences 

library(rstan)
library(reshape2)
library(hBayesDM)
library(cowplot)


#source("R/utils/stan_rhat.R")
#source("R/utils/plot_diff_HDI_info.R")
#source("R/utils/plot_diff.R")

source("Plotting_Utilities.R")


orl_group0_parVals <- fit.ORL.group0$parVals
orl_group1_parVals <- fit.ORL.group1$parVals
orl_group2_parVals <- fit.ORL.group2$parVals

title <- c(expression(A["+"]), expression(A["-"]), expression(K), 
           expression(beta[F]), expression(beta[P]))

p1 <- plot_diff_info(orl_group0_parVals, orl_group1_parVals, title = title, cols = 1)
p2 <- plot_diff_info(orl_group0_parVals, orl_group2_parVals, title = title, cols = 1)
p3 <- plot_diff_info(orl_group1_parVals, orl_group2_parVals, title = title, cols = 1)


p_all <- cowplot::plot_grid(p1, p2, p3, ncol = 3)
ggsave(filename="~/Desktop/group_compare.pdf", plot = p_all)


mu_pars <- c("mu_Arew", "mu_Apun", "mu_K", "mu_betaF", "mu_betaP")
groups  <- c("Healthy Controls", "Patient Controls", "Suicide Attempters")
mu_Arew <- mu_Apun <- mu_Adiff <- mu_K <- mu_betaF <- mu_betaP <- data.frame(Arew = rep(NA, 10000), 
                                                                              Apun = rep(NA, 10000),
                                                                              Adiff = rep(NA, 10000),
                                                                              K = rep(NA, 10000),
                                                                              betaF = rep(NA, 10000),
                                                                              betaP = rep(NA, 10000))

all_dat <- list()
all_fits <- list("Healthy Controls" = orl_group0_parVals, "Patient Controls" = orl_group1_parVals, "Suicide Attempters" = orl_group2_parVals)

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
  scale_fill_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  scale_color_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  geom_density(alpha = 0.5) + 
  ggtitle(expression(A["+"])) + 
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 20))



h2 <- ggplot(Apun_m, aes(x = mu_Apun, group = Group, color = Group, fill = Group)) +
  scale_fill_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  scale_color_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  geom_density(alpha = 0.5) + 
  ggtitle(expression(A["-"])) + 
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 20))


h3 <- ggplot(K_m, aes(x = mu_K, group = Group, color = Group, fill = Group)) +
  scale_fill_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  scale_color_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  geom_density(alpha = 0.5) + 
  ggtitle(expression(K)) + 
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 20))


h4 <- ggplot(betaF_m, aes(x = mu_betaF, group = Group, color = Group, fill = Group)) +
  scale_fill_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  scale_color_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  geom_density(alpha = 0.5) + 
  ggtitle(expression(beta[F])) + 
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 20))

        
h5 <- ggplot(betaP_m, aes(x = mu_betaP, group = Group, color = Group, fill = Group)) +
  scale_fill_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  scale_color_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  geom_density(alpha = 0.5) + 
  ggtitle(expression(beta[P])) + 
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 20))

> ggplot(Adiff_m, aes(x = mu_Adiff, group = Group, color = Group, fill = Group)) +
  scale_fill_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  scale_color_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  geom_density(alpha = 0.5) + 
  ggtitle(expression(A[diff])) + 
  xlab("") +
  ylab("") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 20))


h_key <- ggplot(betaP_m, aes(x = mu_betaP, group = Group, color = Group, fill = Group)) +
  scale_fill_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  scale_color_manual(values = c("Healthy Controls" = "#fb9a99", "Patient Controls" = "#1f78b4", "Suicide Attempters" = "#542788")) + 
  geom_density(alpha = 0.5) + 
  ggtitle(expression(beta[P])) + 
  xlab("") +
  ylab("") +
  theme(plot.title = element_text(size = 20))


h_all <- plot_grid(h1, h2, h3, h4, h5, ncol = 3)
ggsave(filename="~/Desktop/group_pars.pdf", plot = h_all)
ggsave(filename="~/Desktop/group_pars_key.pdf", plot = h_key)
