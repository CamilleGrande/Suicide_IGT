# Plotting_Utilities.R
# Author: Camille Grandé
# Description:
#     This script holds the plotting functions used in the descriptive stats script

## group.var = NULL (default) plots the whole sample, same as before.
## Pass a grouping column (e.g. "Group", "GroupSA") to instead get one
## bar per level, faceted by group.
my.barplot <- function(my.df, my.data, path, suffix, group.var = NULL) {
 
  my.df <- my.df |> mutate(across(all_of(my.data), as.character))
 
  if (!is.null(group.var)) {
    my.df <- my.df |> filter(!is.na(.data[[group.var]]))
  }
 
  my.plot <- ggplot(my.df) +
        aes(x = forcats::fct_infreq(.data[[my.data]]),
        fill = .data[[my.data]]) +
        geom_bar(width = 0.5) +
        scale_fill_paletteer_d("ggthemes::Classic_10_Light") +
        labs(x = my.data) +
        theme_classic() +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
 
  if (!is.null(group.var)) {
    my.plot <- my.plot + facet_wrap(vars(.data[[group.var]]))
  }
 
  ggsave(filename = paste0(path, my.data, suffix), plot = my.plot)
  
}
 
 
## group.var = NULL (default) plots one histogram for the whole sample, same
## as before. Pass a grouping column to instead get one facet per level, each
## with its own mean line.
my.histogram <- function(my.df, my.data, path, suffix, group.var = NULL) {
  
  my.df <- my.df |> filter(!is.na(.data[[my.data]]))
 
  if (!is.null(group.var)) {
    my.df <- my.df |> filter(!is.na(.data[[group.var]]))
 
    means <- my.df |>
                group_by(.data[[group.var]]) |>
                    summarise(my.mean = round(mean(.data[[my.data]], na.rm = T), digits = 3))
  } else {
    means <- data.frame(my.mean = round(mean(my.df[[my.data]], na.rm = T), digits = 3))
  }
 
  my.plot <- ggplot(my.df) + 
        aes(x = .data[[my.data]]) + 
        geom_histogram(color = "#A1C2EDFF", fill= "#C5DAF6FF", binwidth = 1) + 
        geom_vline(data = means, aes(xintercept = my.mean), color = "#4060C8FF", linetype = "dashed", linewidth = 1) +
        theme_classic()
 
  if (!is.null(group.var)) {
    my.plot <- my.plot + facet_wrap(vars(.data[[group.var]]))
  } else {
    my.plot <- my.plot + annotate("text", x = I(0.8), y = I(0.8), label = paste0("mean = ", means$my.mean))
  }
 
  ggsave(filename = paste0(path, my.data, suffix), plot = my.plot)
}
 
 
my.violinplot <- function(my.df, path, my.names, my.values, extension) {
 
  long <- pivot_longer(my.df, names(my.df), names_to = my.names, values_to = my.values)
  
  my.plot <- ggplot(long) +
          geom_violin(aes(x = .data[[my.names]], y = .data[[my.values]], fill = .data[[my.names]]), width = 1.7, alpha = 0.8, trim = F) +
          scale_color_paletteer_d("ggthemes::Classic_10_Light") +
          theme_classic() +
          theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
          geom_boxplot(aes(x = .data[[my.names]], y = .data[[my.values]]), width = 0.2)
 
  ggsave(filename = paste0(path, extension), plot = my.plot, width = 14, height = 6)
}