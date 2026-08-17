# Utilities_Behavior.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       behavioral scores stats part of the SPAD study


## This function tidies the IGT data
Tidy.IGT <- function(IGT.data, suffix) {

    ## Read csv and recode choices as A, B, C or D, and create column with safe/risky deck label
    igt <- read_delim(IGT.data, delim = ";") |>
                mutate(choice = recode(choice, `1` = "A", `2` = "B", `3` = "C", `4` = "D")) |>
                    mutate(deck_type = ifelse(choice %in% c("C","D"), "safe", "risky"),
                            block = ceiling(Trial / 20))

    ## For SUICIDE-DECIDE, rename Group_by_suicide as Group
    if ("Group_by_suicide" %in% names(igt)) {
        igt <- igt |>
                select(-Group) |>
                    mutate(Group = as.factor(Group_by_suicide), .keep = "unused")
    }

    ## Computes the block (Q1, Q2, Q3, Q4, Q5) for each participant
    subj_block_scores <- igt |>
                            group_by(subjID, Group, block, choice) |>
                                ## Computes number of times each subj choose deck A, B, C or D 
                                ## for each block (Q1, Q2, Q3, Q4, Q5)
                                summarise(n = n(), .groups = "drop") |>
                                    ## Pivot summary table so we have 5 rows per subject (Q1, Q2, Q3, Q4, Q5),
                                    ## and a column for each deck (A, B, C, D) with the nb of times they
                                    ## chose each deck for each block. We also have their group
                                    pivot_wider(names_from = choice, values_from = n, values_fill = 0) |>
                                        ## Creates new column with net score per block for each participant
                                        mutate(net_block = (C + D) - (A + B))

    ## one csv file with all results (n choices for each deck and net score) for each block, for all subj
    write.csv(subj_block_scores, paste0("./Behavior/Outputs/Subject_level_block_scores", suffix, ".csv"), row.names = F)

    ## Computes the total score for each participant
    subj_total_scores <- igt |>
                            group_by(subjID, Group, choice) |>
                                ## Summarizes total nb of times subjects chose each deck
                                ## 4 rows per subject (A, B, C, D)
                                summarise(n = n(), .groups = "drop") |>
                                    ## Pivots table so we get subjID, Group, and one column per deck
                                    ## So we have only one row per subject
                                    pivot_wider(names_from = choice, values_from = n, values_fill = 0) |>
                                        ## Computes total global score
                                        mutate(net_total = (C + D) - (A + B))

    write.csv(subj_total_scores, paste0("./Behavior/Outputs/Subject_level_total_scores", suffix, ".csv"), row.names = F)

    return(list(subj_block_scores, subj_total_scores))
}


## This function computes the descriptive stats (means) for each group
Descriptives.IGT <- function(IGT.data, suffix) {

    ## [1] is for per block (Q1-Q5) score analyses, [2] is for total score analysis
    tidy <- Tidy.IGT(IGT.data, suffix)

    ## Computes the total score for each group (from the participant data)
    group_total_scores <- as.data.frame(tidy[2]) |>
                            group_by(Group) |>
                                summarise(
                                    mean_net_total = mean(net_total, na.rm = TRUE),
                                    sd_net_total   = sd(net_total,   na.rm = TRUE),
                                    n_subj         = n(),
                                    .groups = "drop"
                                )
    
    write.csv(group_total_scores, paste0("./Behavior/Outputs/Mean_total_score", suffix, ".csv"), row.names = F)


    ## Computes the block (Q1, Q2, Q3, Q4, Q5) for each group (from the participant data)
    group_block_scores <- as.data.frame(tidy[1]) |>
                            group_by(Group, block) |>
                                summarise(
                                    mean_net_block = mean(net_block, na.rm = TRUE),
                                    sd_net_block   = sd(net_block,   na.rm = TRUE),
                                    n_subj         = n(),
                                    .groups = "drop"
                                )

    write.csv(group_block_scores, paste0("./Behavior/Outputs/Mean_block_scores", suffix, ".csv"), row.names = F)

    return(tidy)
}


## This function computes the group differences for the IGT behavioral scores
## using p-adjusted values (holm)
Group.Diff.IGT <- function(IGT.data, suffix) {

    ## [1] is for per block (Q1-Q5) score analyses, [2] is for total score analysis
    tidy <- Tidy.IGT(IGT.data, suffix)

    ## total net score and total proportions per deck (A,B,C,D)
    kw_total <- bind_rows(lapply(setdiff(names(as.data.frame(tidy[2])), c("subjID", "Group")), function(iv) {

        res_kw <- kruskal.test(as.formula(paste(iv, "~ Group")), data = as.data.frame(tidy[2]))

        tibble(
            variable = res_kw$data.name,
            statistic = res_kw$statistic,
            df = res_kw$parameter,
            p.value = res_kw$p.value,
            p.adjusted = p.adjust(res_kw$p.value, method = "holm", n = 10)
        )
    }))

    ## now computing net score for each block (Q1-Q5)
    net_score_per_block <- as.data.frame(tidy[1]) |>
                            select(all_of(c("Group", "block", "net_block")))

    kw_block <- bind_rows(lapply(1:5, function(b) {
        
        df_block <- net_score_per_block[net_score_per_block$block == b, ]

        res_kw <- kruskal.test(net_block ~ Group, data = df_block)

        tibble(
            variable = paste0("block ", b, " by Group"),
            statistic = res_kw$statistic,
            df = res_kw$parameter,
            p.value = res_kw$p.value,
            p.adjusted = p.adjust(res_kw$p.value, method = "holm", n = 10)
        )
    }))

    ## Only one significant result (net score block 3 in SPAD)
    significant <- net_score_per_block[net_score_per_block$block == "3", ]
    posthoc <- dunn_test(net_block ~ Group, data = significant, p.adjust.method = "holm")

    summary <- bind_rows(kw_total, kw_block, posthoc)
    write.csv(summary, paste0("./Behavior/Outputs/Group_differences", suffix, ".csv"), row.names = F)
}