# Plotting_Behavior.R
# Author: Camille Grandé
# Description:
#     This script holds the plotting functions used in the behavioral scores script


Plot.IGT <- function(IGT.data, suffix, my.width) {

    ## ----- Line plot Choice proportions per block -----

    ## Creates the blocks
    igt <- read_delim(IGT.data, delim = ";") |>
                mutate(choice = recode(choice, `1` = "A", `2` = "B", `3` = "C", `4` = "D")) |>
                    mutate(deck_type = ifelse(choice %in% c("C","D"), "safe", "risky"),
                            block = ceiling(Trial / 20))


    ## If SUICIDE-DECIDE, rename Group_by_suicide as Group and get rid of original Group col (all SA together)
    ## Then create vector with group labels
    if ("Group_by_suicide" %in% names(igt)) {

        group_labels <- c("0" = "Healthy controls", "1" = "Patient controls", "NVSA" = "Non-violent attempters", "VSA" = "Violent attempters")

        igt <- igt |>
                select(-Group) |>
                    mutate(Group = Group_by_suicide, .keep = "unused") |>
                        mutate(GroupLabel = group_labels[as.character(Group)], .keep = "all")
    } else {

        group_labels <- c("0" = "Healthy controls", "1" = "Patient controls", "2" = "Suicide attempters")

        igt <- igt |>
                mutate(GroupLabel = group_labels[as.character(Group)], .keep = "all")
    }


    ## Compute proportions per group × block × deck
    block_props <- igt |>
                    group_by(Group, GroupLabel, block, choice) |>
                        summarise(n = n(), .groups = "drop") |>
                            group_by(Group, block) |>
                                mutate(prop = n / sum(n)) |>
                                    ungroup() 

    badlands <- paletteer_d("nationalparkcolors::Badlands")

    line_plot <- ggplot(block_props, aes(x = block, y = prop, color = choice, group = choice)) +
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
    
    ggsave(paste0("./Behavior/Outputs/Plot_Proportions_per_block", suffix, ".pdf"), plot = line_plot, width = my.width, height = NA)


    ## ----- Boxplot Total net score -----
    net_score <- read_csv(paste0("./Behavior/Outputs/Subject_level_total_scores", suffix, ".csv"))

    net_score <- net_score |>
                    mutate(Group = case_when(
                                            Group == "0" ~ "HC",
                                            Group == "1" ~ "PC",
                                            Group == "2" ~ "SA",
                                            Group == "NVSA" ~ "nvSA",
                                            Group == "VSA" ~ "vSA"
                                            ))

    boxplot <- ggboxplot(
                    net_score, x = "Group", y = "net_total",
                    fill = "Group", palette = c("#719F47FF", "#F2CB05FF", "#DD75D3FF", "#E16305FF"),
                    xlab = "Group", ylab = "Total net score"
                )
    
    ggsave(paste0("./Behavior/Outputs/Plot_Total_net_score", suffix, ".pdf"), plot = boxplot)
}