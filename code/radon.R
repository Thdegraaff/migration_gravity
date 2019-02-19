######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")
library("HLMdiag")
library("rstan")

data(radon)
str(radon)
radon <- radon[, 1:4]

radon <- radon %>%
  rename(logradon = log.radon) %>%
  group_by(county) %>%
  mutate(xbar = mean(basement))

radon <- as.data.frame(radon)

m2 <- map2stan(
  alist(
    logradon ~ dnorm(mu, sigma),
    mu <- a_county[county] + b1 * uranium + b2 * basement + b3*xbar,
    a_county[county] ~ dnorm(a,sigma_county),
    a ~ dnorm(0,1),
    c(b1, b2, b3) ~ dnorm(0,5),
    c(sigma, sigma_county) ~ dcauchy(0,1)
  ), 
  data = radon, chains = 4, warmup = 1000, iter = 4000, cores = 2
)
