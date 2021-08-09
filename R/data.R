process_data <- function(raw) {
  full <- raw %>% 
    rename(Diagnostic = "Diagnostic route") %>% 
    mutate(Outcome = tolower(Outcome)) %>% 
    filter(Diagnostic != "other" & Age != "16-20" & Age != "21-25") %>% 
    mutate(Diagnostic = recode(Diagnostic, "sympto" = "symptomatic", "screen" = "screening")) %>% 
    mutate_at(vars(Outcome, Diagnostic, Age), as_factor)
  summ <- full %>%
    group_by(Outcome, Diagnostic, Age) %>%
    tally %>%
    ungroup %>% 
    separate(Age, c("start", "end"), remove=FALSE) %>% 
    mutate(end = ifelse(end == "", "90", end)) %>% 
    mutate_at(vars(start, end), as.numeric) %>% 
    mutate(Midage = (start - 1 + end)/2) %>% 
    select(-start, -end)
  logreg <- full %>% 
    filter(Outcome != "survival") %>% 
    mutate(death = as.integer(Outcome == "direct_bc_death") %>% as_factor()) %>%
    separate(Age, c("start", "end"), remove=FALSE) %>%
    mutate(end = ifelse(end == "", "90", end)) %>%
    mutate_at(vars(start, end), as.numeric) %>%
    mutate(Midage = (start - 1 + end)/2) %>%
    select(-start, -end)
  list(
    full = full,
    summ = summ,
    logreg = logreg
  )
}