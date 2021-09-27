######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")
library("brms")
library("rcartocolor")
library("ggplot2")
library("dutchmasters")

set.seed(5)

######################
# Set Dutch masters theme
######################

# theme_pearl_earring <- function(light_color = "#E8DCCF", 
#                                 dark_color = "#100F14", 
#                                 my_family = "Courier",
#                                 ...) {
#   
#   theme(line = element_line(color = light_color),
#         text = element_text(color = light_color, family = my_family),
#         strip.text = element_text(color = light_color, family = my_family),
#         axis.text = element_text(color = light_color),
#         axis.ticks = element_line(color = light_color),
#         axis.line = element_blank(),
#         legend.background = element_rect(fill = dark_color, color = "transparent"),
#         legend.key = element_rect(fill = dark_color, color = "transparent"),
#         panel.background = element_rect(fill = dark_color, color = light_color),
#         panel.grid = element_blank(),
#         plot.background = element_rect(fill = dark_color, color = dark_color),
#         strip.background = element_rect(fill = dark_color, color = "transparent"),
#         ...)
#   
# }
# 
# # now set `theme_pearl_earring()` as the default theme
# theme_set(theme_pearl_earring())

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

m_temporal <- ulam(
  alist(
    mAB ~ poisson( lambdaAB ),
    mBA ~ poisson( lambdaBA ),
    log(lambdaAB) <- cons + 
      b_popA * lpopA + b_popB * lpopB + 
      b_dist*ldist + 
      b_hA[year] * lhomA + b_hB[year] * lhomB + b_sA[year] * lsocA + b_sB[year] * lsocB + 
      gr[origin,1] + gr[destination,2] +   
      y[year] + 
      d[did,1],
    log(lambdaBA) <- cons + 
      b_popB * lpopA + b_popA * lpopB + 
      b_dist*ldist + 
      b_hA[year] * lhomB + b_hB[year] * lhomA + b_sA[year] * lsocB + b_sB[year] * lsocA + 
      gr[destination,1] + gr[origin,2] +   
      y[year] + 
      d[did, 2],
    b_dist ~ normal(-1.5, 0.5),
    c(b_popA, b_popB) ~ normal(1, 0.5),
    cons ~ normal(3,3),
    y[year] ~ normal(0, sigma_y),
    b_hA[year] ~ normal(0, sigma_hA),
    b_hB[year] ~ normal(0, sigma_hB),
    b_sA[year] ~ normal(0, sigma_sA),
    b_sB[year] ~ normal(0, sigma_sB),
    sigma_y ~ dexp(1),
    sigma_hA ~ dexp(1),
    sigma_hB ~ dexp(1),
    sigma_sA ~ dexp(1),
    sigma_sB ~ dexp(1),
    
    
    ## gr matrix of varying effects
    transpars> matrix[nr_regions, 2]: gr <-
      compose_noncentered( sigma_gr , L_Rho_d1 , z1),
    matrix[2,nr_regions]: z1 ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]: L_Rho_d1 ~ lkj_corr_cholesky( 2 ),
    vector[2]: sigma_gr ~ dexp(1),
    
    ## dyad effects
    transpars> matrix[nr,2]:d <-
      compose_noncentered( rep_vector(sigma_d,2) , L_Rho_d2 , z2 ),
    matrix[2,nr]:z2 ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]: L_Rho_d2 ~ lkj_corr_cholesky( 2 ),
    sigma_d ~ exponential(1),
    
    ## compute correlation matrix for groups
    gq> matrix[2,2]:Rho_1 <<- Chol_to_Corr( L_Rho_d1 ),
    
    ## compute correlation matrix for dyads
    gq> matrix[2,2]:Rho_2 <<- Chol_to_Corr( L_Rho_d2 )
  ), data = m_data , chains = 4 , cores = 4 , iter = 4000, warmup = 1000, log_lik = TRUE )

precis(m_temporal)
save(m_temporal, file = "./output/corop_temporal.rda")
load(file = "./output/corop_temporal.rda")

par <- precis(m_temporal , depth=2 , pars=c("b_hA", "b_sA", "b_hB", "b_sB") )
d_par <- t(rbind( par@.Data[[1]], par@.Data[[2]]) ) 
names <- rep(c("Home-ownership origin", "Social renting origin", "Home-ownership destination", "Social renting destination"), each = 8) 
years <- rep(seq(2012, 2019, 1), 4)

d_co <- tibble(names, years, coef = d_par[,1], sterr = d_par[,2]) %>%
  mutate(
    lower = coef - 1.96 * sterr,
    upper = coef + 1.96 * sterr,
  )

g <- ggplot(data = d_co, aes(y = coef, x = years)) +
  geom_point(size = 2, colour = "cornflowerblue", alpha = 1) + 
  geom_line(aes(group = 1), colour = "cornflowerblue", size = 1, alpha = 0.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), colour="cornflowerblue", size = 1, width = 0.1, alpha = 0.5) +
  theme_minimal() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))  +
  facet_wrap(~ names) +
  labs(
    x         = "Years (2012-2019)",
    y         = "Estimates")

ggsave(g, file="./fig/temporal_variation.pdf", width  = 200, height = 140, units = "mm")

############## Calculate migration flows within an between regions 2012-2019

growth_within <- (1023747 - 869002)/869002
growth_between <- (757522 - 609859)/609859
