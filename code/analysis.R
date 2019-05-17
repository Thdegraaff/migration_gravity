######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")

######################
# Get subsample of data
######################

nr <- 20

######################
# Read in data
######################

load(file = "./data/derived/migration.Rda")

d <- data

d$origin <- as.numeric(as.factor(d$code_o))
d$destination <- as.numeric(as.factor(d$code_d))
d <- select(d, -code_o, -code_d)
d <- d %>% filter(origin <= nr, destination <= nr)

d$log_distance <- log(d$distance) - mean(log(d$distance))
d$pop_o <- log(d$pop_o) - mean(log(d$pop_o) )
d$pop_d <- log(d$pop_d) - mean(log(d$pop_d) )
d$soc_d <- log(d$socialhousing_d + 0.0001 )
d$soc_o <- log(d$socialhousing_o + 0.0001 )
d$soc_d <- d$soc_d - mean(d$soc_d)
d$soc_o <- d$soc_o - mean(d$soc_o)
d$hom_o <- log(d$homeowners_o) - mean(log(d$homeowners_o))
d$hom_d <- log(d$homeowners_d) - mean(log(d$homeowners_d))

d_m1 <- d[ , c("migrants", "log_distance", "pop_d", 
               "pop_o", "soc_o", "soc_d",
               "destination", "origin")]

m1 <- map2stan(
  alist(
    migrants ~ dzipois(p,  mu ), 
    logit(p) <- ap,
    log(mu) <- a + bd * log_distance + 
      bpo * pop_o + bpd * pop_d + 
      bso * soc_o + bsd * soc_d, 
    c(a, bd, bso, bsd, bpo, bpd) ~ dnorm(0,2),
    ap ~ dnorm(0,1)
  ),
  data = d_m1, iter = 3000, warmup = 1000, chains = 3, cores = 3
)

m2 <- map2stan(
  alist(
    migrants ~ dpois( lambda ), 
    log(lambda) <- a_origin[origin]  + 
                   a_destination[destination] + 
                   b_d*log_distance, 
    b_d ~ dnorm(-1,1),
    a_origin[origin] ~ dnorm(0,5),
    a_destination[destination] ~ dnorm(0,5)
  ),
  data = d, iter = 3000, warmup = 1000, chains =1, cores = 7
)

m3 <- map2stan(
  alist(
    migrants ~ dzipois(p,  lambda ), 
    logit(p) <- ap,
    log(lambda) <- a + a_origin[origin]  + 
      a_destination[destination] + 
      b_d*log_distance, 
    a_origin[origin] ~ dnorm(0, sigma_origin),
    a_destination[destination] ~ dnorm(0,sigma_destination),
    c(a, b_d, ap) ~ dnorm(0,10),
    sigma_origin ~ dcauchy(0,1), 
    sigma_destination ~ dcauchy(0,1)
  ),
  data = d, iter = 3000, warmup = 1000, chains = 4, cores = 4
)

m4 <- map2stan(
  alist(
    migrants ~ dpois( lambda ), 
    log(lambda) <- a + A + B + b_d*log_distance, 
      A <- b_po * pop_o + bso * soc_o + bho * hom_o + 
           a_origin[origin] * sigma_origin,
      B <- b_pd * pop_d + bsd * soc_d + bhd * hom_d + 
           a_destination[destination] * sigma_destination,
      a_origin[origin] ~ dnorm(0, 2),
      a_destination[destination] ~ dnorm(0, 2),
      a ~ dnorm(0,2),
      c(b_d, b_po, b_pd, bso, bsd, bho, bhd) ~ dnorm(0,2),
      sigma_origin ~ dcauchy(0,2), 
      sigma_destination ~ dcauchy(0,2)
  ), data = d, iter = 3000, warmup = 2000, chains = 4, cores = 4
)

pairs(m1)

precis(m1, depth = 1)
precis(m2, depth = 1)
precis(m3, depth = 1)
precis(m4, depth = 2)

compare(m1, m2, m3, m4)

