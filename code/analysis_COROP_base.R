######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")

set.seed(5)

######################
# Read in data
######################

load(file = "./data/derived/migration_COROP.Rda")
df <- df %>%
  filter(year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | year == 2017 | year == 2018 | year == 2019)

nr_regions = max(df$destination)

m_data <- list(
  origin = df$origin,
  destination = df$destination,  
  year = as.factor(df$year),
  did = df$did,
  mAB = df$mAB,
  mBA = df$mBA,
  nr_regions = max(df$destination),
  nr = max(df$did),
  ldist = df$ldist, 
  lpopA = df$lpopA,
  lpopB = df$lpopB,
  lhomA = df$lhomA,
  lsocA = df$lsocA,
  lhomB = df$lhomB,
  lsocB = df$lsocB,
  dhomA = df$dhomA,
  dsocA = df$dsocA,
  dhomB = df$dhomB,
  dsocB = df$dsocB, 
  dpriA = df$dpriA,
  dpriB = df$dpriB  
)

m_base <- ulam(
  alist(
    mAB ~ poisson( lambdaAB ),
    mBA ~ poisson( lambdaBA ),
    log(lambdaAB) <- cons + 
      b_popA * lpopA + b_popB * lpopB + 
      b_dist*ldist,
    log(lambdaBA) <- cons + 
      b_popB * lpopA + b_popA * lpopB + 
      b_dist*ldist,
    b_dist ~ normal(-1, 0.5),
    c(b_popA, b_popB) ~ normal(1, 0.5),
    cons ~ normal(3,3)
  ), data = m_data , chains = 4 , cores = 4 , iter = 4000, warmup = 1000, log_lik = TRUE )

precis(m_base)
save(m_base, file = "./output/m_base.rda")

m_base_housing <- ulam(
  alist(
    mAB ~ poisson( lambdaAB ),
    mBA ~ poisson( lambdaBA ),
    log(lambdaAB) <- cons + 
      b_popA * lpopA + b_popB * lpopB + 
      b_hA  * lhomA + b_hB * lhomB + b_sA * lsocA + b_sB * lsocB + 
      b_dist*ldist,
    log(lambdaBA) <- cons + 
      b_popB * lpopA + b_popA * lpopB + 
      b_hA  * lhomB + b_hB * lhomA + b_sA * lsocB + b_sB * lsocA +    
      b_dist*ldist,
    b_dist ~ normal(-1, 0.5),
    c(b_popA, b_popB) ~ normal(1, 0.5),
    c(b_hA, b_sA, b_hB, b_sB) ~ normal(0, 0.5),
    cons ~ normal(4,2)
  ), data = m_data , chains = 4 , cores = 4 , iter = 4000, warmup = 1000, log_lik = TRUE )

precis(m_base_housing)
save(m_base_housing, file = "./output/m_base_housing.rda")

rm(list = ls()) # Remove all objects