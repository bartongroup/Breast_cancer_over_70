cases_scr <- 10100
cases_sym <- 15862
deaths_scr <- 819
deaths_sym <- 4620

p3 <- cases_scr / (cases_scr + cases_sym)

p1 <- deaths_sym / cases_sym
p2 <- deaths_scr / cases_scr
R = p2 / p1

lmb <- 0.25
t <- 10
Es <- (1 - exp(-lmb * t)) / lmb
