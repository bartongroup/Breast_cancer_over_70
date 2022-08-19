find_outcome_proportion <- function(d, value, others) {
  ds <- d %>% 
    group_by(Diagnostic, Age) %>% 
    summarise(s = sum(n[Outcome == value]), f = sum(n[Outcome %in% others]), n = s + f,  p = s / n, Midage = Midage[1]) %>%
    filter(s > 0 & f > 0) %>% 
    ungroup
  
  properr <- ds %>%
    mutate(row = row_number()) %>% 
    group_by(row) %>%
    do({
      ci <- suppressWarnings(prop.test(.$s, .$n)$conf.int)
      se <- sqrt(.$p * (1 - .$p) / .$n)
      tibble(p_lo = ci[1], p_up = ci[2], se = se)
    }) %>% 
    ungroup
  
  bind_cols(ds, select(properr, p_lo, p_up, se))
}

fit_poly <- function(d, value, others, ...) {
  d %>% 
    find_outcome_proportion(value, others) %>% 
    select(x=Midage, y=p, se, Diagnostic) %>% 
    group_split(Diagnostic) %>% 
    map_df(function(w) {
      m <- nls(y ~ A + B*x + C*x^2 + D*x^3, start=list(A=0.2, B=0, C=0, D=0), weights=1/se^2, data=w)
      xd <- tibble(x=seq(min(w$x),max(w$x),1))
      tibble(Diagnostic=w$Diagnostic[1], x=xd$x, y=predict(m, xd))
    })
}


chi2_comparison <- function(d, value, others) {
  P <- d %>% 
    find_outcome_proportion(value, others) %>% 
    select(Age, Diagnostic, p, se) %>% 
    group_split(Diagnostic)
  cx <- inner_join(P[[1]], P[[2]], by="Age") %>% 
    mutate(se2 = se.x^2 + se.y^2, chi2 = (p.x - p.y)^2 / se2)
  chi2 <- sum(cx$chi2)
  dof <- nrow(cx) - 1
  p <- 1 - pchisq(chi2, dof)
  list(chi2 = chi2, dof = dof, p = p)
}


make_fisher_tables <- function(d, cases) {
  map_dfr(cases, function(case) {
    c_screen <- d %>%
      filter(Diagnostic=="screening") %>% 
      find_outcome_proportion(case$value, case$others)
    c_sym <- d %>%
      filter(Diagnostic=="symptomatic") %>% 
      find_outcome_proportion(case$value, case$others)
    c_join <- inner_join(c_screen, c_sym, by="Age") %>% 
      nest(data = c(s.x, f.x, s.y, f.y)) %>% 
      mutate(
        test = map(data, ~fisher.test(matrix(c(.x$s.x, .x$s.y, .x$f.x, .x$f.y), nrow=2))),
        tidied = map(test, broom::tidy)
      ) %>% 
      unnest(c(tidied, data)) %>% 
      rename(bc_screen = s.x, all_screen = n.x, bc_sym = s.y, all_sym = n.y, odds_ratio=estimate) %>% 
      mutate(case = case$name)
    c_join$FDR <- p.adjust(c_join$p.value, method="BH")
    c_join
  })
}


group_age <- function(d) {
  d <- d %>% 
    mutate(Outcome = recode(Outcome, "unrelated_bc_death" = "unrelated_death")) %>% 
    group_by(Diagnostic, Outcome) %>% 
    summarise(n = sum(n)) %>% 
    ungroup %>% 
    group_by(Diagnostic) %>% 
    mutate(p = n / sum(n), tot = sum(n)) %>%
    ungroup
  
  err <- map_df(1:nrow(d), function(i) {
    r <- d[i, ]
    conf <- prop.test(r$n, r$tot, conf.level=0.95)$conf.int
    tibble(p_lo = conf[1], p_up = conf[2])
  })
  
  bind_cols(d, err)
}

fisher_75 <- function(over_75) {
  d <- over_75 %>%
    mutate(s = tot - n) %>% 
    pivot_wider(id_cols=Outcome, names_from=Diagnostic, values_from=c(n, s)) %>% 
    nest(data = -Outcome) %>% 
    mutate(
      test = map(data, ~fisher.test(matrix(c(.x$n_symptomatic, .x$s_symptomatic, .x$n_screening, .x$s_screening), nrow=2))),
      tidied = map(test, broom::tidy)
    ) %>% 
    unnest(c(tidied, data)) %>% 
    select(-c(test, method, alternative))
  d$FDR <- p.adjust(d$p.value, method="BH")
  d
}


death_propotions <- function(d) {
  d %>%
    #mutate(Outcome = recode(Outcome, direct_bc_death="bc_death", indirect_bc_death="bc_death")) %>% 
    group_by(Outcome, Diagnostic) %>%
    summarise(count = sum(n)) %>%
    ungroup() %>% 
    group_by(Diagnostic) %>% 
    mutate(total = sum(count), prop = count / total) %>%
    ungroup() %>% 
    pivot_wider(id_cols=Diagnostic, names_from=Outcome, values_from=c(count, prop))
}

# we fit data with a straight line parametrized by
# slope and age at which proportion is 0.5 (parameter Half).
# This allows us to calculate the error of the half-life
time_course_fit <- function(d, value, others, min.age) {
  dx <- d %>%  
    find_outcome_proportion(value, others) %>% 
    filter(Midage >= min.age) %>% 
    select(x = Midage, y = p, se)
  m <- nls(y ~ 0.5 - Slope*Half + Slope*x, start=list(Slope=-0.1, Half=70), weights=1/se^2, data=dx)
  cbind(summary(m)$coefficients, nlstools::confint2(m)) %>% 
    as.data.frame %>%
    rownames_to_column(var="Parameter")
}

time_course <- function(d, value, others, min.age=37.5, tit=NULL, with.half=FALSE) {
  # fit time course with a linear function, using nls to include errors
  coef <- time_course_fit(d, value, others, min.age=min.age)
  slope <- coef$Estimate[1]
  half <- coef$Estimate[2]
  half.err <- (coef$`97.5 %`[2] - coef$`2.5 %`[2]) / 2
  
  xx <- c(min.age, 87.5)
  mod <- tibble(x = xx, y = 0.5 - slope*half + xx * slope)
  g <- plot_fit_data(d, value, others, FITFUN=NULL, tit=tit) +
    geom_line(data=mod, aes(x=x, y=y), group=1, colour=1)
  if(with.half) {
    g <- g + 
      #geom_tile(x=halfx, width=halfx.err, y=0.5, height=0.5, fill="grey80") +
      geom_vline(xintercept = half, linetype = "dotted", colour="grey50")
  }
  list(plot=g, coef=coef, half=half, half.err=half.err)
}


logit_prediction <- function(dbc_mod) {
  m <- tibble(Midage = seq(25, 90, 1))
  dummy <- bind_rows(
    m %>% add_column(Diagnostic = "screening"),
    m %>% add_column(Diagnostic = "symptomatic")
  ) %>% 
    mutate(Diagnostic = factor(Diagnostic, levels = c("symptomatic", "screening")))
  pred_dbc <- predict(dbc_mod, dummy, se.fit = TRUE)
  dummy %>% 
    mutate(
      pred = pred_dbc$fit,
      pred_se = pred_dbc$se.fit,
      prob = exp(pred) / (1 + exp(pred)),
      prob_se = pred_se * exp(pred) / (1 + exp(pred))^2   # error propagation
    )
}



tabulate_group_deaths <- function(summ) {
  s1 <- summ %>% 
    mutate(group = case_when(
      Midage < 50 ~ "<50",
      Midage >= 50 & Midage < 70 ~ "50-70",
      Midage > 70 ~ ">70"
    )) %>%
    group_by(group, Diagnostic) %>% 
    summarise(
      n_direct_bc_death = sum(n[Outcome == "direct_bc_death"]),
      n_total_death = sum(n[Outcome != "survival"])
    ) %>% 
    ungroup()
  s2 <- s1 %>% 
    group_by(group) %>% 
    summarise(n_direct_bc_death = sum(n_direct_bc_death), n_total_death = sum(n_total_death)) %>% 
    mutate(Diagnostic = "Both")
  bind_rows(s1, s2) %>% 
    mutate(prop = n_direct_bc_death / n_total_death)
}