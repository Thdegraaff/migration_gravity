######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")
library("brms")
library("shinystan")

######################
# Get subsample of data
######################

nr <- 100

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

################ Analysis #############

#m1 <- brm(migrants~0 + log_distance, family = poisson("log"), data = d)
# m2 <- brm(migrants~ pop_d + pop_o +hom_d + hom_o + soc_o + soc_d + log_distance +
#                 (1 | destination) + (1 | origin),
#               prior = c(prior(normal(0, 2), class = Intercept),
#                         prior(normal(0, 2), class = b),
#                         prior(cauchy(0, 1), class = sd)),
#               family = poisson("log"), data = d, iter = 2000, warmup = 1000, cores = 4, chains = 4)

m2_nb <- brm(migrants~ pop_d + pop_o +hom_d + hom_o + soc_o + soc_d + log_distance +
                 (1 | destination) + (1 | origin),
               prior = c(prior(normal(0, 2), class = Intercept),
                         prior(normal(0, 2), class = b),
                         prior(cauchy(0, 1), class = sd),
                         prior(gamma(0.01, 0.01), class = shape)),
               family = negbinomial, data = d, iter = 4000, warmup = 1000, cores = 4, chains = 4)

# Did the analysis, but zero inflated parameter very close to zero, and model_weights give
# 69% weight on non-zero inflated model and 31% on zero-inflated model. 
# m2_zinb <- brm(migrants~ pop_d + pop_o +hom_d + hom_o + soc_o + soc_d + log_distance +
#             (1 | destination) + (1 | origin),
#            prior = c(prior(normal(0, 2), class = Intercept),
#                     prior(normal(0, 2), class = b),
#                     prior(cauchy(0, 1), class = sd),
#                     prior(gamma(0.01, 0.01), class = shape)),
#           family = zero_inflated_negbinomial, data = d, iter = 4000, warmup = 1000, cores = 4, chains = 4)
