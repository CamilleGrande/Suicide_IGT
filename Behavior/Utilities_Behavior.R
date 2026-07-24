# Utilities_Behavior.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       behavioral scores stats part of the SPAD study


Behavior.IGT <- function(IGT.data, my.group, suffix) {

    ## Read csv and recode choices as A, B, C or D, and create column with safe/risky deck label
    igt <- read_delim(IGT.data, delim = ";") |>
                mutate(choice = recode(choice, `1` = "A", `2` = "B", `3` = "C", `4` = "D")) |>
                    mutate(deck_type = ifelse(choice %in% c("C","D"), "safe", "risky"),
                            block = ceiling(Trial / 20)) 

    ## Computes the block (Q1, Q2, Q3, Q4, Q5) for each participant
    subj_block_scores <- igt |>
                            group_by(subjID, {{ my.group }}, block, choice) |>
                                ## Computes number of times each subj choose deck A, B, C or D 
                                ## for each block (Q1, Q2, Q3, Q4, Q5)
                                summarise(n = n(), .groups = "drop") |>
                                    ## Pivot summary table so we have 5 rows per subject (Q1, Q2, Q3, Q4, Q5),
                                    ## and a column for each deck (A, B, C, D) with the nb of times they
                                    ## chose each deck for each block. We also have their group
                                    pivot_wider(names_from = choice, values_from = n, values_fill = 0) |>
                                        ## Creates new column with net score per block for each participant
                                        mutate(net_block = (C + D) - (A + B))

    write.csv(subj_block_scores, paste0("./Behavior/Outputs/Subject_level_block_scores", suffix, ".csv"), row.names = F)


    ## Computes the total score for each participant
    subj_total_scores <- igt |>
                            group_by(subjID, {{ my.group }}, choice) |>
                                ## Summarizes total nb of times subjects chose each deck
                                ## 4 rows per subject (A, B, C, D)
                                summarise(n = n(), .groups = "drop") |>
                                    ## Pivots table so we get subjID, Group, and one column per deck
                                    ## So we have only one row per subject
                                    pivot_wider(names_from = choice, values_from = n, values_fill = 0) |>
                                        ## Computes total global score
                                        mutate(net_total = (C + D) - (A + B))

    write.csv(subj_total_scores, paste0("./Behavior/Outputs/Subject_level_total_scores", suffix, ".csv"), row.names = F)


    ## Computes the block (Q1, Q2, Q3, Q4, Q5) for each group (from the participant data)
    group_block_scores <- subj_block_scores |>
                            group_by({{ my.group }}, block) |>
                                summarise(
                                    mean_net_block = mean(net_block, na.rm = TRUE),
                                    sd_net_block   = sd(net_block,   na.rm = TRUE),
                                    n_subj         = n(),
                                    .groups = "drop"
                                )

    write.csv(group_block_scores, paste0("./Behavior/Outputs/Group_level_block_scores", suffix, ".csv"), row.names = F)


    ## Computes the total score for each group (from the participant data)
    group_total_scores <- subj_total_scores |>
                            group_by({{ my.group }}) |>
                                summarise(
                                    mean_net_total = mean(net_total, na.rm = TRUE),
                                    sd_net_total   = sd(net_total,   na.rm = TRUE),
                                    n_subj         = n(),
                                    .groups = "drop"
                                )
    
    write.csv(group_total_scores, paste0("./Behavior/Outputs/Group_level_total_scores", suffix, ".csv"), row.names = F)
}