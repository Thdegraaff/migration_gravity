# Read in libraries

library(tidyverse)
library(rethinking)

# First read in commuting database

df_com <- read.csv(file="./data/derived/commuting_2016_corop.csv", header = TRUE)
df_com <- df_com %>%
  mutate (jobs = as.integer(jobs * 10),
          log_distance = log(distance))


m1 <- map2stan(
  alist(
    jobs~ dpois( lambda ), 
    log(lambda) <- a + 
      a_origin[from_id]  + 
      a_destination[to_id] + 
      b_d*log_distance, 
    a_origin[from_id] ~ dnorm(0, sigma_origin),
    a_destination[to_id] ~ dnorm(0,sigma_destination),
    c(a, b_d) ~ dnorm(0,10),
    sigma_origin ~ dcauchy(0,1), 
    sigma_destination ~ dcauchy(0,1)
  ),
  data = df_com, iter = 3000, warmup = 1000, chains = 3, cores = 3
)

