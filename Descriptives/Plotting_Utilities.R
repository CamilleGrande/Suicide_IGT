# Plotting_Utilities.R
# Author: Camille Grandé
# Description:
#     This script holds the plotting functions used in the descriptive stats script

my.barplot <- function(my.df, my.data, directory, suffix) {
  my.plot <- ggplot(my.df |> mutate(across(all_of(my.data), as.character))) +
        #aes(x = reorder(.data[[my.data]], .data[[my.data]], FUN = function(x) -length(x)),
        aes(x = forcats::fct_infreq(.data[[my.data]]),
        fill = .data[[my.data]]) +
        geom_bar(width = 0.5) +
        scale_fill_paletteer_d("ggthemes::Classic_10_Light") +
        labs(x = my.data) +
        theme_classic() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  ggsave(filename = paste0("./1-Descriptives/Outputs/plots/", directory, my.data, suffix), plot = my.plot)
  
}


my.histogram <- function(my.df, my.data, directory, suffix) {
  
  my.mean <- round(mean(my.df[[my.data]], na.rm = T), digits = 3)

  my.plot <- ggplot(my.df) + 
        aes(x = .data[[my.data]]) + 
        geom_histogram(color = "#A1C2EDFF", fill= "#C5DAF6FF", binwidth = 1) + 
        geom_vline(aes(xintercept = my.mean), color = "#4060C8FF", linetype = "dashed", linewidth = 1) +
        annotate("text", x = I(0.8), y = I(0.8), label = paste0("mean = ", my.mean)) +
        theme_classic()

  ggsave(filename = paste0("./1-Descriptives/Outputs/plots/", directory, my.data, suffix), plot = my.plot)
}


my.violinplot <- function(my.df, path, my.names, my.values, extension) {

  long <- pivot_longer(my.df, names(my.df), names_to = my.names, values_to = my.values)
  
  my.plot <- ggplot(long) +
          geom_violin(aes(x = .data[[my.names]], y = .data[[my.values]], fill = .data[[my.names]]), width = 1.7, alpha = 0.8, trim = F) +
          scale_color_paletteer_d("ggthemes::Classic_10_Light") +
          theme_classic() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
          geom_boxplot(aes(x = .data[[my.names]], y = .data[[my.values]]), width = 0.2)

  ggsave(filename = paste0("./1-Descriptives/Outputs/plots/", path, extension), plot = my.plot, width = 14, height = 6)
}