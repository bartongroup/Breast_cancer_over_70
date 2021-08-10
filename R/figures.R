plot_death_prop <- function(d, cases, mod=NULL, min.age=37.5) {
  value <- cases[[1]]$value
  others <- cases[[1]]$others
  
  dat <- d %>% 
    find_outcome_proportion(value, others)
  
  if(is.null(mod)) {
    coef <- time_course_fit(d, value, others, min.age=min.age)
    slope <- coef$Estimate[1]
    half <- coef$Estimate[2]
    xx <- c(min.age, 87.5)
    mod <- tibble(x = xx, y = 0.5 - slope*half + xx * slope)
  }
  
  half <- approx(x=mod$y, y=mod$x, xout=0.5)$y
  print(half)
  
  ggplot(data=dat, aes(x=Midage, y=p)) +
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    geom_errorbar(aes(x=Midage, ymin=p-se, ymax=p+se), width=1) +
    geom_point(size=1.5) +
    labs(x = "Age at diagnosis", y = "Proportion of deaths directly due to BC") +
    scale_y_continuous(limits=c(0, 1), expand=c(0, 0)) +
    geom_line(data=mod, aes(x=x, y=y), group=1) +
    geom_vline(xintercept = half, linetype = "dotted", colour="grey50") +
    scale_x_continuous(expand=c(0,0), limits=c(15,95))
}
