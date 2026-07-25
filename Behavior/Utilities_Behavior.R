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


## Computes the group differences on IGT variables using the Kruskal Wallis 
## omnibus test and Dunn's test for post-hoc comparisons
IGT.Differences <- function(my.data, iv, my.group, cohort) {

    ## Look at plot to see distribution
    my.plot <- ggboxplot(
                    my.data, x = my.group, y = iv,
                    color = my.group, palette = paletteer_d("nationalparkcolors::Badlands"),
                    xlab = "Group", ylab = iv
                )
    
    ggsave(paste0("./Behavior/Outputs/Boxplot_", iv, cohort, ".pdf"), plot = my.plot)

    ## Must create formula first because cannot subset data
    formula_kw <- as.formula(paste(iv, "~", my.group))

    ## Kruskal-Wallis test (H statistic)
    res_kw <- my.data |>
                    kruskal_test(formula_kw)

    ## Effect size Kruskal-Wallis results (eta-squared)
    eff_size_kw <- my.data |>
                        kruskal_effsize(formula_kw)

    ## Dunn's test, adjusted with Holm
    pairwise_comp_dunn <- my.data |>
                            dunn_test(formula_kw, p.adjust.method = "holm")


    my_results <- list(res_kw, eff_size_kw, pairwise_comp_dunn)
    new_names <- c("KW_Result", "KW_Effect_Size", "Dunn_pairwise_comp")

    my_results <- Map(function(tbl, nm) {
                        dplyr::rename(tbl, !!nm := .y.)
                    }, my_results, new_names)

    my_results <- dplyr::bind_cols(my_results)

    write_csv(my_results, paste0("./Behavior/Outputs/Results_", iv, cohort, ".csv"))
}


IGT.All.Differences <- function(data, vars, my.group, cohort) {

    my.data <- read_csv(data)

    lapply(vars, function(var) {
        IGT.Differences(my.data, var, my.group, cohort)
    })
}


## Net score difference per block
IGT.Differences.Per.Block <- function(data, vars, my.group, cohort) {

    my.data <- readr::read_csv(data)

    blocks <- sort(unique(my.data[["block"]]))

    lapply(blocks, function(b) {

        ## subset data for block b
        data_subset <- my.data[my.data[["block"]] == b, ]

        ## run IGT differences on this subset of data
        IGT.Differences(data_subset, vars, my.group, paste0(cohort, b))
    })
}