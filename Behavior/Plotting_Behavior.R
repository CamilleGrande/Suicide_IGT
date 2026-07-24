# Plotting_Behavior.R
# Author: Camille Grandé
# Description:
#     This script holds the plotting functions used in the behavioral scores script


Plot.Choice.Prop.Block <- function(IGT.data, my.group, group.labels, suffix, my.width) {

    ## Creates the blocks
    igt <- read_delim(IGT.data, delim = ";") |>
                mutate(choice = recode(choice, `1` = "A", `2` = "B", `3` = "C", `4` = "D")) |>
                    mutate(deck_type = ifelse(choice %in% c("C","D"), "safe", "risky"),
                            block = ceiling(Trial / 20),
                            GroupLabel = group.labels[as.character({{ my.group }})])
    
    ## Compute proportions per group × block × deck
    block_props <- igt |>
                    group_by({{ my.group }}, GroupLabel, block, choice) |>
                        summarise(n = n(), .groups = "drop") |>
                            group_by({{ my.group }}, block) |>
                                mutate(prop = n / sum(n)) |>
                                    ungroup() 

    badlands <- paletteer_d("nationalparkcolors::Badlands")

    plot <- ggplot(block_props, aes(x = block, y = prop, color = choice, group = choice)) +
                geom_line(linewidth = 1.2) +
                geom_point(size = 2) +
                facet_wrap(~ GroupLabel) +
                scale_x_continuous(breaks = 1:5) +
                labs(   x = "Block",
                        y = "Choice proportion",
                        color = "Deck") +
                theme_minimal(base_size = 14) +
                ## Assign colours for risky vs safe: risky = yellow/red, safe = green/blue
                scale_color_manual(
                    values = c(
                    "A" = badlands[1],
                    "B" = badlands[2],
                    "C" = badlands[3],
                    "D" = badlands[4]
                    ))
    
    ggsave(paste0("./Behavior/Outputs/Proportions_per_block", suffix, ".pdf"), plot = plot, width = my.width, height = NA)
}