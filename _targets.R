library(targets)

packages <- c("nlstools", "cowplot", "broom", "tidyverse")
tar_option_set(packages = packages, format = "qs")
options(dplyr.summarise.inform = FALSE)

# for interactive session only
# sapply(packages, library, character.only=TRUE)

files_R <- list.files("R", pattern="*.R$", full.names=TRUE)
sr_ <- sapply(files_R, source)

make_cases <- list(
  tar_target(cases,list(
    `1` = list(value = "direct_bc_death",
               others = c("indirect_bc_death", "unrelated_death", "unrelated_bc_death"),
               name = "Direct BC deaths / all deaths"),
    `2` = list(value = "unrelated_death",
               others = c("indirect_bc_death", "direct_bc_death", "unrelated_bc_death"),
               name = "Unrelated deaths / all deaths"),
    `3` = list(value = "direct_bc_death",
               others = c("indirect_bc_death", "unrelated_death", "unrelated_bc_death", "survival"),
               name = "Direct BC deaths / all outcomes"),
    `4` = list(value = "unrelated_death",
               others = c("indirect_bc_death", "direct_bc_death", "unrelated_bc_death", "survival"),
               name = "Unrelated deaths / all outcomes")
  ))
)


read_data <- list(
  tar_target(data_file, "data/surv.txt", format="file"),
  tar_target(raw_data, read_tsv(data_file, col_types = cols())),
  tar_target(set, process_data(raw_data))
)

filter_data <- list(
  tar_target(summ_other_deaths_together, set$summ %>% 
    mutate(Outcome = recode(Outcome,
      "indirect_bc_death" = "other death",
      "unrelated_bc_death" = "other death",
      "unrelated_death" = "other death")
  )),
  tar_target(summ_survival_excluded_1, set$summ %>% 
    filter(Outcome != "survival") %>% 
    mutate(Outcome = recode(Outcome,
      "indirect_bc_death" = "other bc death",
      "unrelated_bc_death" = "other bc death",
      "unrelated_death" = "unrelated death")
    )),
  tar_target(summ_survival_excluded_2, set$summ %>% 
    filter(Outcome != "survival") %>% 
    mutate(Outcome = recode(Outcome,
      "indirect_bc_death" = "other death",
      "unrelated_bc_death" = "other death",
      "unrelated_death" = "other death")
    )),
  tar_target(summ_other_outcomes_together, set$summ %>% 
    mutate(Outcome = recode(Outcome,
      "indirect_bc_death" = "other outcome",
      "unrelated_bc_death" = "other outcome",
      "unrelated_death" = "other outcome",
      "survival" = "other outcome")
    )),
  tar_target(over_75, group_age(filter(set$summ, Midage > 75))),
  tar_target(summ_sym, set$summ %>% filter(Diagnostic=="symptomatic")),
  tar_target(summ_both, set$summ %>%
    group_by(Age, Outcome) %>%
    summarise(n = sum(n), Midage = Midage[1]) %>%
    mutate(Diagnostic = "both")),
  tar_target(summ_screen, set$summ %>% filter(Diagnostic=="screening"))
)

figures <- list(
  tar_target(fig_basic_grouping, plot_data(set$summ)),
  tar_target(fig_other_deaths_together, plot_data(summ_other_deaths_together)),
  tar_target(fig_survival_excluded_1, plot_data(summ_survival_excluded_1)),
  tar_target(fig_survival_excluded_2, plot_data(summ_survival_excluded_2)),
  tar_target(fig_other_outcomes_together, plot_data(summ_other_outcomes_together)),
  tar_target(fig_diagnostic, plot_data(set$summ, what="Diagnostic")),
  
  tar_target(fig_screen, plot_screen(summ_other_outcomes_together)),
  tar_target(fig_death_diagnostic, plot_death(set$summ)),
  
  tar_target(fig_proportions, map(cases, function(case) {
      plot_outcome_proportion(set$summ, value = case$value, others = case$others, tit = case$name, err="se")   
    }) %>% plot_grid(plotlist = ., ncol=1)
  ),
  
  tar_target(fig_75_count, plot_group_age(over_75, what="count")),
  tar_target(fig_75_proportion, plot_group_age(over_75, what="proportion")),
  tar_target(fig_75_proportion_stack, plot_group_age(over_75, what="proportion", pos="stack")),
  
  tar_target(fig_age_comp_count, plot_age_comparison(set$summ, what="count")),
  tar_target(fig_age_comp_proportion, plot_age_comparison(set$summ, what="proportion")),
  tar_target(fig_75_details, plot_outcome_proportion(set$summ, value = cases[[4]]$value, others = cases[[4]]$others, tit = cases[[4]]$name, err="ci") + coord_cartesian(ylim=c(0,0.5))),
  tar_target(fig_75_alternative, plot_alternative_75(over_75, set$summ)),
  tar_target(tc_sym, time_course(summ_sym, cases[[1]]$value, cases[[1]]$others, tit=cases[[1]]$name, with.half = TRUE)),
  tar_target(tc_both, time_course(summ_both, cases[[1]]$value, cases[[1]]$others, tit=cases[[1]]$name, with.half = TRUE)),
  tar_target(tc_screen, time_course(summ_screen, cases[[1]]$value, cases[[1]]$others, tit=cases[[1]]$name, with.half = TRUE)),
  
  tar_target(fig_logreg_prediction, plot_logit_prediction(dbc_logit)),
  tar_target(fig_logreg_diff, plot_logit_decrease(dbc_logit))
  
)

tables <- list(
  tar_target(tab_chisq, map_df(cases, function(case) {
    cs <- chi2_comparison(set$summ, value = case$value, others = case$others)
    tibble(Group = case$name, chi2 = cs$chi2, p = cs$p)
  }) %>% 
    mutate_at(vars(chi2, p), ~signif(.,3)) %>% mutate_at(vars(chi2, p), as.character)),
  
  tar_target(tab_fisher, make_fisher_tables(set$summ, cases)),
  tar_target(tab_fisher_sel, tab_fisher %>% select(case, Age, bc_screen, all_screen, bc_sym, all_sym, odds_ratio, p.value, FDR)),
  
  tar_target(tab_fisher_75, fisher_75(over_75)),
  
  tar_target(tab_75_death_proportions, set$summ %>% filter(Midage > 75) %>% death_propotions()),
  tar_target(tab_71_75_death_proportions, set$summ %>% filter(Age == "71-75") %>% death_propotions())
)


regression <- list(
  tar_target(dbc_model, glm(death ~ Diagnostic + Midage, data=set$logreg, family="binomial")),
  tar_target(dbc_logit, logit_prediction(dbc_model))
)

sesinfo <- list(
  tar_target(session_info, sessionInfo())
)


c(
  make_cases,
  read_data,
  filter_data,
  regression,
  tables,
  figures,
  sesinfo
)
