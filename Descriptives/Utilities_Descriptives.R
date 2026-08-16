# Utilities_Descriptives.R
# Author: Camille Grandé
# Study: SPAD
# Description:
#       This script holds the utility functions necessary to run the 
#       descriptive stats part of the SPAD study


## This function tidies the SPAD dataset
Tidy.SPAD <- function(ids.IGT, data.SPAD) {

  ids <- read_delim(ids.IGT)|> 
                distinct(subjID) |> 
                    rename(SID = subjID)

  data <- read_csv(data.SPAD, na = c("", " ", "NA", "N/A", "\u00A0\u00A0")) |>
    mutate(across(where(is.character), trimws)) |>
            mutate(SID = SID,
                    group = fct_recode(as.factor(Group), "HC" = "0", "PC" = "1", "SA" = "2"),
                    age = as.integer(Age),
                    sex = fct_recode(as.factor(Sex), "male" = "0", "female" = "1"),
                    mmse_total = as.integer(MMSE_Total),
                    nart_corr = as.integer(NART_Corr),
                    ssi_total = as.integer(SSI_Total),
                    nb_actual_sa_py = as.integer(Nb_Actual_SA_PY),
                    actual_lethality_most_lethal = as.factor(Actual_lethality_most_lethal),
                    sis_total = as.integer(SIS_Total),
                    bdi13_total = as.integer(BDI13_Total),
                    diploma = fct_recode(as.factor(Diplom), "less_secondary" = "1", "secondary" = "2", "secondary" = "3", 
                                            "secondary" = "4", "high_school" = "5", "bachelor" = "6", "masters" = "7", "doctorate" = "8"),
                    antidep = fct_recode(as.factor(X.antidep.), "no" = "0", "yes" = "1"),
                    antipsy = fct_recode(as.factor(X.antipsy.), "no" = "0", "yes" = "1"),
                    benzo = fct_recode(as.factor(X.benzo.), "no" = "0", "yes" = "1"),
                    antianx = fct_recode(as.factor(X.antianx), "no" = "0", "yes" = "1"),
                    igt_q1 = as.numeric(IGT_IndQ1),
                    igt_q2 = as.numeric(IGT_IndQ2),
                    igt_q3 = as.numeric(IGT_IndQ3),
                    igt_q4 = as.numeric(IGT_IndQ4),
                    igt_q5 = as.numeric(IGT_IndQ5),
                    igt_total = as.numeric(IGT_IndTotal),
                    igt_rt = as.numeric(IGT_RT),
                    igt_rt_safe = as.numeric(IGT_RTSafe),
                    igt_rt_risk = as.numeric(IGT_RTRisk),
                    gonogo_mean_rt = as.numeric(GoNogo_Moy),
                    gonogo_corr = as.integer(GoNogo_Corr),
                    gonogo_comm = as.integer(GoNogo_Fal),
                    gonogo_omi = as.integer(GoNogo_Omi),
                    fluverb_p = as.integer(X.FluVerb_P),
                    fluverb_ani = as.integer(X.FluVerb_Ani),
                    .keep = "none") |>
                        ## Keep only people with IGT data (= those used in modelling)
                        inner_join(ids, by = "SID") |>
                            select(-SID) |>
                              ## HC don't take any ttt
                              mutate(
                                antidep = case_when(group == "HC" ~ "no", TRUE ~ antidep),
                                antipsy = case_when(group == "HC" ~ "no", TRUE ~ antipsy),
                                benzo = case_when(group == "HC" ~ "no", TRUE ~ benzo),
                                antianx = case_when(group == "HC" ~ "no", TRUE ~ antianx),
                                .keep = "all"
                              )

  return(data)
}


## This function tidies the SUICIDE-DECIDE dataset
Tidy.SUICIDEDECIDE <- function(ids.IGT, data.SUICIDEDECIDE) {

  ids <- read_delim(ids.IGT)|> 
                distinct(subjID) |> 
                    rename(SID = subjID)

  ## don't have nart and wbct 
  data <- read_csv(data.SUICIDEDECIDE, na = c("", " ", "NA", "N/A", "\u00A0\u00A0")) |>
            mutate(SID = subjID,
                    group = fct_recode(as.factor(GroupSA), "HC" = "0", "PC" = "1", "nvSA" = "2", "vSA" = "3"),
                    age = as.integer(Age),
                    sex = fct_recode(as.factor(Sex), "male" = "0", "female" = "1"),
                    nb_actual_sa_py = as.integer(Nb_Actual_SA_PY),
                    actual_lethality_most_lethal = as.factor(Actual_lethality_most_lethal),
                    sis_total = as.integer(SIS_Total),
                    bdi2_total = as.integer(BDI2_sum),
                    diploma = fct_recode(as.factor(Highest.level.of.education), "special_sch" = "Sonderschule", "secondary_general_sch" = "Hauptschule",
                                "secondary_sch" = "Realschule", "academic_secondary_sch" = "Gymnasium/EOS", "University" = "Universität", "Univ_Applied_Sc" = "Fachhochschule"),
                    dopamine_agonist = fct_recode(as.factor(Dopamine.agonist), "no" = "0", "yes" = "1"),
                    nmda_antagonist = fct_recode(as.factor(NMDA.antagonist), "no" = "0", "yes" = "1"),
                    benzo = fct_recode(as.factor(Benzodiazepines), "no" = "0", "yes" = "1"),
                    lithium = as.factor(Lithium.salts),
                    gonogo_mean_rt = as.numeric(gonogo_mean_RT),
                    gonogo_corr = as.integer(gonogo_total_correct),
                    gonogo_comm = as.integer(gonogo_total_commissions),
                    gonogo_omi = as.integer(gonogo_total_omissions),
                    fluverb_p = as.integer(FluVerb_P),
                    fluverb_ani = as.integer(FluVerb_Ani),
                    .keep = "none") |>
                      ## HC don't take any ttt
                              mutate(
                                dopamine_agonist = case_when(group == "HC" ~ "no", TRUE ~ dopamine_agonist),
                                nmda_antagonist = case_when(group == "HC" ~ "no", TRUE ~ nmda_antagonist),
                                benzo = case_when(group == "HC" ~ "no", TRUE ~ benzo),
                                lithium = case_when(group == "HC" ~ "no", lithium == "0" ~ "no", lithium == "1" ~ "yes", TRUE ~ lithium),
                                .keep = "all"
                              )

  return(data)
}


## This function creates a numeric summary for all our variables (keep numeric only first)
Numeric.Summary <- function(vars) {

  numeric_summary <- vars |>
                        group_by(group) |>
                            summarise(across(everything(),
                                        list(
                                            n = ~ sum(!is.na(.x), na.rm = T),
                                            mean = ~ mean(.x, na.rm = T),
                                            sd = ~ sd(.x, na.rm = T),
                                            median = ~ median(.x, na.rm = T),
                                            iqr = ~ IQR(.x, na.rm = T),
                                            min = ~ min(.x, na.rm = T),
                                            max = ~ max(.x, na.rm = T),
                                            skewness = ~ as.numeric(skewness(.x, na.rm = T)), 
                                            kurtosis = ~ as.numeric(kurtosis(.x, na.rm = T))
                                        ), .names = "{.col}.{.fn}"
                                    ), .groups = "drop") |>
                                            pivot_longer(cols = -group, names_to = c("variable", ".value"), names_sep = "\\.")

  return(numeric_summary)
}


## This function creates a frequency summary for all our variables (keep factor only first)
Frequency.Summary <- function(vars) {

  frequency_summary <- vars |>
                        pivot_longer(cols = -group, names_to = "variable", values_to = "level") |>
                            group_by(group, variable) |>
                                mutate(total_n = n(),
                                        valid_n = sum(!is.na(level))) |>
                                    filter(!is.na(level)) |>
                                        count(group, variable, level, total_n, valid_n, name = "n") |>
                                            group_by(group, variable) |>
                                                mutate(pct_valid = n / valid_n * 100,
                                                        pct_total = n / total_n * 100,
                                                        pct_valid_cum = cumsum(pct_valid),
                                                        pct_total_cum = cumsum(pct_total)) |>
                                                    ungroup()
  
  return(frequency_summary)
}


## Chi square and pairwise comparison (proportion test) for each dataset
Chi.Square <- function(data.frequency, dataset = NULL) {

  my.tests <- bind_rows(lapply(setdiff(names(data.frequency), "group"), function(var) {

    test <- summary(table(data.frequency$group, data.frequency[[var]]))

    tibble(
      variable = var,
      n = test$n.cases,
      statistic = test$statistic,
      df = test$parameter,
      p.value = test$p.value
    )
  }))

  ## diploma is not binary, and actual lethality only in SA
  frequency_vars_pw <- select(data.frequency, -all_of(c("actual_lethality_most_lethal", "diploma")))

  my.posthoc <- bind_rows(lapply(setdiff(names(frequency_vars_pw), "group"), function(var) { 

      prop <- pairwise.prop.test(
        x = table(frequency_vars_pw$group, frequency_vars_pw[[var]]),
        n = rowSums(table(frequency_vars_pw$group, frequency_vars_pw[[var]])),
        p.adjust.method = "none",
        correct = F
      )

      if (dataset == "SPAD") {
        tibble(
          variable = var,
          method = prop$method,
          p_HC_PC = prop$p.value[1],
          p_HC_SA = prop$p.value[2],
          p_PC_SA = prop$p.value[4]
        )

      } else if (dataset == "SUICIDEDECIDE") {
        tibble(
          variable = var,
          method = prop$method,
          p_HC_PC = prop$p.value[1],
          p_HC_nvSA = prop$p.value[2],
          p_HC_vSA = prop$p.value[3],
          p_PC_nvSA = prop$p.value[5],
          p_PC_vSA = prop$p.value[6],
          p_nvSA_vSA = prop$p.value[9]
        )
      }   
  }))

  summary <- bind_rows(my.tests, my.posthoc)
  return(summary)
}


## Kruskal wallis and dunn test (pairwise comp)
Kruskal.Wallis <- function(data.numeric, dataset = NULL) {

  if (dataset == "SPAD") {
    my.data <- select(data.numeric, -any_of(c("ssi_total", "nb_actual_sa_py", "sis_total")))
  } else if (dataset == "SUICIDEDECIDE") {
    my.data <- select(data.numeric, -any_of(c("ssi_total")))
  }

  my.tests <- bind_rows(lapply(setdiff(names(my.data), "group"), function(var) {

    ## cannot have [[var]] inside formula, must create it with paste
    test <- kruskal.test(as.formula(paste(var, "~ group")), data = my.data)
    
    tibble(
      variable = test$data.name,
      n = sum(!is.na(my.data[[var]]), na.rm = T),
      statistic = test$statistic,
      df = test$parameter,
      p.value = test$p.value
    )

  }))

  my.posthoc <- bind_rows(lapply(setdiff(names(my.data), "group"), function(var) { 

      dunn <- my.data |>
        ## cannot have [[var]] inside formula, must create it with paste
        dunn_test(as.formula(paste(var, "~ group")), p.adjust.method = "none")

  }))

  summary <- bind_rows(my.tests, my.posthoc)
  return(summary)
}


## This function runs the descriptive stats part of SPAD
SPAD.Descriptives <- function(ids.IGT, data.SPAD) {

  ## tidying the data
  tidy <- Tidy.SPAD(ids.IGT, data.SPAD)

  write_rds(tidy, "./Descriptives/Outputs/Tidy_SPAD.rds")

  ## Separate numeric and frequency vars for summaries
  numeric_vars <- tidy |>
                    select(all_of(c("group", "age", "mmse_total", "nart_corr", "ssi_total", "nb_actual_sa_py", "sis_total", "bdi13_total", 
                                    "igt_q1", "igt_q2", "igt_q3", "igt_q4", "igt_q5", "igt_total", "igt_rt", "igt_rt_safe", "igt_rt_risk", 
                                    "gonogo_mean_rt", "gonogo_corr", "gonogo_comm", "gonogo_omi", "fluverb_p", "fluverb_ani")))

  frequency_vars <- tidy |>
                    select(all_of(c("group", "sex", "actual_lethality_most_lethal", "diploma", "antidep", "antipsy", "benzo", "antianx")))

  numeric_sum <- Numeric.Summary(numeric_vars)
  write.csv(numeric_sum, "./Descriptives/Outputs/Numeric_Descriptives_SPAD.csv", row.names = FALSE)

  frequency_sum <- Frequency.Summary(frequency_vars)
  write.csv(frequency_sum, "./Descriptives/Outputs/Frequency_Descriptives_SPAD.csv", row.names = FALSE)

  ## Chi square test of independence
  chi_sq <- Chi.Square(frequency_vars, dataset = "SPAD")
  write.csv(chi_sq, "./Descriptives/Outputs/ChiSquare_Descriptives_SPAD.csv", row.names = FALSE)

  ## Kruskal Wallis
  kw <- Kruskal.Wallis(numeric_vars, dataset = "SPAD")
  write.csv(kw, "./Descriptives/Outputs/KruskalWallis_Descriptives_SPAD.csv", row.names = FALSE)

}


## This function runs the descriptive stats part of SUICIDE-DECIDE
SUICIDEDECIDE.Descriptives <- function(ids.IGT, data.SUICIDEDECIDE) {

  ## tidying the data
  tidy <- Tidy.SUICIDEDECIDE(ids.IGT, data.SUICIDEDECIDE)

  write_rds(tidy, "./Descriptives/Outputs/Tidy_SUICIDEDECIDE.rds")

  ## Separate numeric and frequency vars for summaries
  numeric_vars <- tidy |>
                    select(all_of(c("group", "age", "nb_actual_sa_py", "sis_total", "bdi2_total", 
                                    "gonogo_mean_rt", "gonogo_corr", "gonogo_comm", "gonogo_omi", "fluverb_p", "fluverb_ani")))

  frequency_vars <- tidy |>
                    select(all_of(c("group", "sex", "actual_lethality_most_lethal", "diploma", "dopamine_agonist", "nmda_antagonist",
                                    "benzo", "lithium")))

  numeric_sum <- Numeric.Summary(numeric_vars)
  write.csv(numeric_sum, "./Descriptives/Outputs/Numeric_Descriptives_SUICIDEDECIDE.csv", row.names = FALSE)

  frequency_sum <- Frequency.Summary(frequency_vars)
  write.csv(frequency_sum, "./Descriptives/Outputs/Frequency_Descriptives_SUICIDEDECIDE.csv", row.names = FALSE)

  ## Chi square test of independence
  chi_sq <- Chi.Square(frequency_vars, dataset = "SUICIDEDECIDE")
  write.csv(chi_sq, "./Descriptives/Outputs/ChiSquare_Descriptives_SUICIDEDECIDE.csv", row.names = FALSE)

  ## Kruskal Wallis
  kw <- Kruskal.Wallis(numeric_vars, dataset = "SUICIDEDECIDE")
  write.csv(kw, "./Descriptives/Outputs/KruskalWallis_Descriptives_SUICIDEDECIDE.csv", row.names = FALSE)

}