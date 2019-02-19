######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")

load(file = "./data/derived/migration.Rda")

d <- data %>%
  select(migrants, distance) %>%
  mutate(logdistance = log(distance))
d <- as.data.frame(d)

m2 <- map(
  alist(
    migrants ~ dpois( lambda ),
    log(lambda) <- a + bd * logdistance,
    a ~ dnorm(5,1), 
    bd ~ dnorm(-1,1)
  ), 
  data = d, 
)

n <- 1000

x1 <- rnorm(n, 10, 0.02)
beta1 <- rnorm(n, 1, 0.05)
c <- runif(n, 3,4)

mu <- log(beta1*x1 + c)

y <- rpois(n, mu)

d <-data.frame(y, x1)

m2 <- map(
  alist(
    y ~ dpois( mu1 ),
    log(mu1) <- a + bd * x1,
    a ~ dnorm(0,10), 
    bd ~ dnorm(0,10)
  ), 
  data = d
)
