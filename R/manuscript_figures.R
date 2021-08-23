make_figure_1 <- function(set) {
  ds <- set$summ %>% 
    group_by(Age) %>% 
    summarise(direct = sum(n[Outcome == "direct_bc_death"]), s = sum(n[Outcome != "survival"]), n = sum(n), p = s / n)
  
  properr <- ds %>%
    mutate(row = row_number()) %>% 
    group_by(row) %>%
    do({
      ci <- suppressWarnings(prop.test(.$s, .$n)$conf.int)
      se <- sqrt(.$p * (1 - .$p) / .$n)
      tibble(p_lo = ci[1], p_up = ci[2], se = se)
    }) %>% 
    ungroup
  ds <- bind_cols(ds, select(properr, p_lo, p_up, se))
  
  pl <- ggplot(ds, aes(x=Age, y=p)) +
    theme_bw() +
    theme(
      panel.grid.minor.y = element_blank(),
      axis.text.x = element_text(angle=40, hjust=1)
    ) +
    geom_errorbar(aes(ymin=p_lo, ymax=p_up), width=0.2) +
    geom_point() +
    labs(x="Age group at diagnosis", y="All cause mortality") +
    scale_y_continuous(expand=c(0,0), limits=c(0,1))
  
  file <- "fig_1.pdf"
  ggsave(file.path("pdf", file), plot = pl, device = "pdf", width=4, height=3)
  
  tab <- ds %>% 
    mutate(Survivals = n - s, `Percentage died` = p * 100) %>% 
    select(Age, `Group size` = n, Survivals, `Deaths of any causes` = s, `Deaths directly to BC` = direct)

  list(
    plot = pl,
    table = tab,
    file = file
  )
}


make_figure_2 <- function(cases, dbc_logit, summ_sym, summ_screen) {
  m <- dbc_logit %>% mutate(x=Midage, y=prob)
  
  g1 <- plot_death_prop(summ_sym, cases, mod=filter(m, Diagnostic == "symptomatic")) + labs(title="Symptomatic patients")
  g2 <- plot_death_prop(summ_screen, cases, mod=filter(m, Diagnostic == "screening")) + labs(title="Screened patients", y=NULL)
  pl <- cowplot::plot_grid(g1, g2, nrow=1, align="h")
  
  file <- "fig_2.pdf"
  ggsave(file.path("pdf", file), plot = pl, device = "pdf", width=8, height=3)
  
  list(
    plot = pl,
    file = file
  )
}


make_figure_3 <- function(set) {
  g1 <- set$summ %>%
    filter(Midage < 75) %>%
    group_age() %>%
    plot_group_age(what="proportion", x="Outcome", group="Diagnostic", ymax=1) +
      theme(axis.text.x = element_text(angle=40, hjust=1)) +
      ggtitle("26-75") + theme(legend.position = "none") +
      labs(y="Proportion", fill="Diagnostic route")
  
  g2 <- set$summ %>%
    filter(Midage > 75) %>%
    group_age() %>%
    plot_group_age(what="proportion", x="Outcome", group="Diagnostic", ymax=1) +
      theme(axis.text.x = element_text(angle=40, hjust=1)) +
      ggtitle("76+") + 
      labs(y=NULL)
  
  pl <- cowplot::plot_grid(g1, g2, nrow=1, align = "h", rel_widths = c(1,1.3))
  
  file <- "fig_3.pdf"
  ggsave(file.path("pdf", file), plot = pl, device = "pdf", width=8, height=3)
  
  list(
    plot = pl,
    file = file
  )
}


make_figure_4 <- function() {
  ass71 <- tribble(
    ~assessment, ~n,
    "High risk", 122,
    "Intermediate risk", 396,
    "Low risk", 143,
    "Invasivness/grade unknown", 17,
    "Benign biopsy", 293,
    "Further imaging only", 1309
  ) %>% 
    mutate(prop = n / sum(n)) %>% 
    mutate(assessment = as_factor(assessment)) %>% 
    filter(assessment != "Invasivness/grade unknown")
  
  pl <- ggplot(ass71, aes(x=fct_rev(assessment), y=n)) +
    theme_bw() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    geom_col() +
    geom_text(aes(label=n), hjust=0, nudge_y = 15, size=2.6) +
    coord_flip() +
    scale_y_continuous(expand=c(0,0), limits=c(0, max(ass71$n)*1.14)) +
    labs(x=NULL, y="Patient count")
  
  file <- "fig_4.pdf"
  ggsave(file.path("pdf", file), plot = pl, device = "pdf", width=4, height=2.5)
  
  list(
    plot = pl,
    file = file
  )
  
}
