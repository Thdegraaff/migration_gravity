library("rethinking")
data(Kline)
d <- Kline
d$logpop <- log(d$population)
d$society <- 1:10


m12.6 <- map2stan(
  alist(
    total_tools ~ dpois(mu),
    log(mu) <- a + a_society[society] + bp*logpop, 
    a ~ dnorm(0,10), 
    bp ~ dnorm(0,1), 
    a_society[society] ~ dnorm(0, sigma_society),
    sigma_society ~ dcauchy(0,1)
  ), data = d, iter = 4000, chains = 3
)

