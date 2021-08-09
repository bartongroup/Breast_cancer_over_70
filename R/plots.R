okabe_ito_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

plot_data <- function(d, what="Outcome") {
  d %>% 
    group_by(Age) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup %>% 
  ggplot(aes_string(x=what, y="p")) +
    theme_linedraw() +
    geom_col(aes(fill=Diagnostic)) +
    facet_wrap(~Age, ncol=13) +
    scale_fill_manual(values = okabe_ito_palette) +
    theme(axis.text.x = element_text(angle=45, hjust=1), panel.grid = element_blank()) +
    scale_y_continuous(expand=c(0,0), limits=c(0,1))
}

plot_screen <- function(d) {
  d %>% 
    group_by(Age, Outcome) %>% 
    summarise(s = sum(n[Diagnostic == "screening"]), tot = sum(n), p = s / tot) %>% 
    ungroup %>% 
  ggplot(aes(x=Outcome, y=p, fill=Outcome)) +
    theme_linedraw() +
    geom_col() +
    facet_wrap(~Age, ncol=13, scales = "free_y") +
    scale_fill_manual(values = okabe_ito_palette) +
    theme(axis.text.x = element_text(angle=45, hjust=1), panel.grid = element_blank()) +
    labs(x=NULL, y="Screen proportion")
}


plot_death <- function(d) {
  d %>% 
    mutate(Diagnostic = recode(Diagnostic, "sympto" = "unscreened", "other" = "unscreened")) %>% 
    group_by(Age, Diagnostic) %>% 
    summarise(s = sum(n[Outcome == "direct_bc_death"]), tot = sum(n), f = tot - s, p = s / tot) %>%
    mutate(lab = paste0(s, "/", tot)) %>% 
    ungroup %>% 
  ggplot(aes(x=Diagnostic, y=p)) +
    theme_linedraw() +
    geom_col(fill="goldenrod", colour="black") +
    facet_wrap(~Age, ncol=7, scales = "free_y") +
    scale_fill_manual(values = okabe_ito_palette) +
    theme(axis.text.x = element_text(angle=45, hjust=1), panel.grid = element_blank()) +
    labs(x=NULL, y="Direct BC death proportion") +
    geom_text(aes(label = lab), vjust=1.2, size=3)
}


plot_outcome_proportion <- function(d, value, others, tit=NULL, err="se") {
  if(is.null(tit)) tit <- paste(value, "proportion")
  pd <- position_dodge(width=0.35)
  d <- d %>% 
    find_outcome_proportion(value, others)
  if(err == "se") {
    d <- d %>% mutate(p_lo = p - se, p_up = p + se)
  }
  ggplot(d, aes(x=Age, y=p, group=Diagnostic, colour = Diagnostic)) +
    theme_linedraw() +
    theme(panel.grid = element_blank(), axis.text.x=element_text(angle=45, hjust=1)) +
    geom_errorbar(aes(x=Age, ymin=p_lo, ymax=p_up, colour=Diagnostic), width=0.35, position=pd) +
    geom_point(size=1.5, position=pd) +
    geom_vline(xintercept = seq(0.5,20,1), colour="grey90") +
    labs(x = "Age group", y = "Proportion", title=tit) +
    scale_colour_manual(values = okabe_ito_palette) +
    scale_y_continuous(limits=c(0,1))
}


plot_group_age <- function(d, what="count", pos="dodge", x="Diagnostic", group="Outcome", ymax=NA) {
  d <- d %>%
    mutate(Outcome = recode_factor(Outcome, 
                                   "direct_bc_death" = "Direct BC death",
                                   "indirect_bc_death" = "Indirect BC death",
                                   "unrelated_death" = "Unrelated death",
                                   "survival" = "Survival"
    )) %>% 
    mutate(
      x = !!as.name(x),
      group = !!as.name(group),
      y = if(what == "count") n else p
    )
  
  
  if(!is.null(ymax)) {
    max.y <- ymax
  } else if(pos == "stack") {
    max.y <- 1.05
  } else if(what == "count") {
    max.y <- max(d$y) * 1.03
  } else {
    max.y <- max(d$p_up) * 1.03
  }
  
  g <- ggplot(d, aes(x=x, y=y, group=group, fill=group)) +
    geom_col(position = pos, width=0.6, colour="grey30") +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    scale_fill_manual(values=okabe_ito_palette) +
    scale_y_continuous(expand=c(0,0), limits=c(0, max.y)) +
    labs(x=NULL, y=what)
  if(pos == "dodge" & what == "proportion") {
    g <- g + geom_errorbar(aes(ymin=p_lo, ymax=p_up), position=position_dodge(width=0.6), width=0.2, colour="grey30")
  }
  g
}


plot_age_comparison <- function(d, ages=c("71-75", "76-80", "81-85", "86+"), what="proportion") {
  map(ages, function(age) {
    d %>% 
      filter(Age == age) %>% 
      group_age %>% 
      plot_group_age(what=what) +
      ggtitle(age) +
      theme(legend.position = "none")
  }) %>% 
    plot_grid(plotlist=., nrow=1)
}


plot_fit_data <- function(d, value, others, FITFUN=fit_poly, ..., tit=NULL, with.smooth=FALSE, min.age=35, max.y=1, level=0.95) {
  if(is.null(tit)) tit <- paste(value, "proportion")
  
  dat <- d %>% 
    find_outcome_proportion(value, others)
  
  g <- ggplot(data=dat, aes(x=Midage, y=p, group=Diagnostic, colour = Diagnostic)) +
    theme_linedraw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    geom_errorbar(aes(x=Midage, ymin=p-se, ymax=p+se, colour=Diagnostic), width=0.2) +
    geom_point(size=1.5) +
    labs(x = "Age group", y = "Proportion", title=tit) +
    scale_colour_manual(values = okabe_ito_palette) +
    scale_y_continuous(limits=c(0,max.y), expand=c(0,0))
  if(!is.null(FITFUN)) {
    mod <- FITFUN(d, value, others, ...)
    g <- g +   geom_line(data=mod, aes(x=x, y=y, colour=Diagnostic))
  }
  if(with.smooth) {
    g <- g + geom_smooth(data=filter(dat, Midage>min.age), method="lm", colour="black", size=0.5, level=level)
  }
  g
}

plot_alternative_75 <- function(over_75, summ) {
  g1 <- plot_group_age(over_75, what="proportion", x="Outcome", group="Diagnostic") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    ggtitle("75+")
  g2 <- summ %>%
    filter(Midage < 75) %>%
    group_age() %>%
    plot_group_age(what="proportion", x="Outcome", group="Diagnostic") +
    theme(axis.text.x = element_text(angle=45, hjust=1)) +
    ggtitle("26-70")
  
  plot_grid(g1, g2, nrow=1)
}

plot_logit_prediction <- function(dbc_logit) {
  ggplot(dbc_logit, aes(x=Midage, y=prob, ymin = prob-1.96*prob_se, ymax = prob+1.96*prob_se)) +
    theme_bw() +
    geom_line(aes(colour = Diagnostic)) +
    geom_ribbon(aes(fill=Diagnostic), alpha=0.3) +
    scale_colour_manual(values = okabe_ito_palette) +
    scale_fill_manual(values = okabe_ito_palette) +
    labs(x="Age", y="Probability of direct BC death") +
    scale_y_continuous(expand=c(0,0), limits=c(0,1))
}

plot_logit_decrease <- function(dbc_logit) {
  dbc_logit %>% 
    group_split(Diagnostic) %>% 
    map_dfr(function(d) {
      tibble(
        age = (d$Midage + 0.5)[-1],
        dif = diff(d$prob),
        Diagnostic = first(d$Diagnostic)
      )
    }) %>% 
    ggplot(aes(x=age, y=-100*dif, colour=Diagnostic)) +
    theme_bw() +
    geom_line() +
    scale_color_manual(values=okabe_ito_palette) +
    labs(x="Age", y="Annual probability decrease (per cent)")
}