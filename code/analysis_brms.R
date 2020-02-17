######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")
library("brms")
library("shinystan")

set.seed(5)

######################
# Get subsample of data
######################


nr <- 380 # for 2018 it is 380

######################
# Read in data
######################

load(file = "./data/derived/migration_2018.Rda")

d <- data

cor_d <- d %>% select(
  homeowners_d, socialhousing_d, private_rent_d, pop_d
)

round(cor(cor_d, use = "complete.obs"), 2)


d_t <- data %>%
  filter(code_o =="GM0363")

d$origin <- as.numeric(as.factor(d$code_o))
d$destination <- as.numeric(as.factor(d$code_d))
d <- select(d, -code_o, -code_d)
d <- d %>% filter(origin <= nr, destination <= nr)

d <- d %>%
  filter(housevalue_o > 0, housevalue_d >0)

d$log_distance <- log(d$distance) - mean(log(d$distance))
d$pop_o <- log(d$pop_o) - mean(log(d$pop_o) )
d$pop_d <- log(d$pop_d) - mean(log(d$pop_d) )
d$soc_d <- log(d$socialhousing_d + 0.001 )
d$soc_o <- log(d$socialhousing_o + 0.001 )
d$pri_d <- log(d$private_rent_d + 0.001 )
d$pri_o <- log(d$private_rent_o + 0.001 )
d$soc_d <- d$soc_d - mean(d$soc_d)
d$soc_o <- d$soc_o - mean(d$soc_o)
d$pri_d <- d$pri_d - mean(d$pri_d)
d$pri_o <- d$pri_o - mean(d$pri_o)
d$hv_d <- log(d$housevalue_d) - mean(log(d$housevalue_d))
d$hv_o <- log(d$housevalue_o) - mean(log(d$housevalue_o))
d$hom_o <- log(d$homeowners_o) - mean(log(d$homeowners_o))
d$hom_d <- log(d$homeowners_d) - mean(log(d$homeowners_d))

################ Analysis #############

#m1 <- brm(migrants~0 + log_distance, family = poisson("log"), data = d)
# m2 <- brm(migrants~ pop_d + pop_o +hom_d + hom_o + soc_o + soc_d + log_distance +
#                 (1 | destination) + (1 | origin),
#               prior = c(prior(normal(0, 2), class = Intercept),
#                         prior(normal(0, 2), class = b),
#                         prior(cauchy(0, 1), class = sd)),
#               family = poisson("log"), data = d, iter = 2000, warmup = 1000, cores = 4, chains = 4)

# m2_nb <- brm(Migrants~ pop_d + pop_o + hom_d + hom_o + soc_d + soc_o + 
#                log_distance +
#                  (1 + soc_d | destination ) + (1 + soc_o | origin),
#                 prior = c(prior(normal(0, 2), class = Intercept),
#                          prior(normal(0, 2), class = b),
#                          prior(cauchy(0, 1), class = sd),
#                          prior(gamma(0.01, 0.01), class = shape)),
#                 family = negbinomial, data = d, iter = 3000, 
#                 warmup = 1000, cores = 4, chains = 3,
#                 control = list(adapt_delta = 0.95) )

# this one seems to work as social housing is negative, housing value as well, but interaction between 
# housing value and social housing is positive!
# m2_interaction <- brm(Migrants~ log_distance +  pop_d + pop_o + hom_o + hom_d + soc_o + soc_d + hv_o  + hv_d +soc_o:hv_o + soc_d:hv_d +
#                  (1 | destination ) + (1 | origin),
#                 prior = c(prior(normal(0, 2), class = Intercept),
#                          prior(normal(0, 2), class = b),
#                          prior(cauchy(0, 1), class = sd),
#                          prior(gamma(0.01, 0.01), class = shape)),
#                 family = negbinomial, data = d, iter = 2000,
#                 warmup = 1000, cores = 6, chains = 6) #,
#                 #control = list(adapt_delta = 0.9) )

# This one works where home < soc (=0) < priv
m2_total <- brm(Migrants~ 0 + log_distance +  pop_d + pop_o + hom_o + hom_d + soc_o + soc_d + pri_o + pri_d +
                        (1 | destination ) + (1 | origin),
                      prior = c(prior(normal(0, 2), class = Intercept),
                                prior(normal(0, 2), class = b),
                                prior(cauchy(0, 1), class = sd),
                                prior(gamma(0.01, 0.01), class = shape)),
                      family = negbinomial, data = d, iter = 2000,
                      warmup = 1000, cores = 6, chains = 6) #,
#control = list(adapt_delta = 0.9) )

# Did the analysis, but zero inflated parameter very close to zero, and model_weights give
# 69% weight on non-zero inflated model and 31% on zero-inflated model. 
# m2_zinb <- brm(migrants~ pop_d + pop_o +hom_d + hom_o + soc_o + soc_d + log_distance +
#             (1 | destination) + (1 | origin),
#            prior = c(prior(normal(0, 2), class = Intercept),
#                     prior(normal(0, 2), class = b),
#                     prior(cauchy(0, 1), class = sd),
#                     prior(gamma(0.01, 0.01), class = shape)),
#           family = zero_inflated_negbinomial, data = d, iter = 4000, warmup = 1000, cores = 4, chains = 4)
