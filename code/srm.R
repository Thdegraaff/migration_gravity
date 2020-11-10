######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")

######################
# Get subsample of data
######################

nr <- 380

######################
# Read in data
######################

load(file = "./data/derived/migration_2018.Rda")

d <- data

cor_d <- d %>% select(
  homeowners_d, socialhousing_d, private_rent_d, pop_d, hhgrootte_d
)

round(cor(cor_d, use = "complete.obs"), 2)

d_t <- data %>%
  filter(code_o =="GM0363")

d$origin <- as.numeric(as.factor(d$code_o))
d$destination <- as.numeric(as.factor(d$code_d))
d <- select(d, -code_o, -code_d)
d <- d %>% filter(origin <= nr, destination <= nr)

# d <- d %>%
#   filter(housevalue_o > 0, housevalue_d >0)

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
# d$hv_d <- log(d$housevalue_d) - mean(log(d$housevalue_d))
# d$hv_o <- log(d$housevalue_o) - mean(log(d$housevalue_o))
d$hom_o <- log(d$homeowners_o) - mean(log(d$homeowners_o))
d$hom_d <- log(d$homeowners_d) - mean(log(d$homeowners_d))
d$hhsize_d <- log(d$hhgrootte_d) - mean(log(d$hhgrootte_d))
d$hhsize_o <- log(d$hhgrootte_o) - mean(log(d$hhgrootte_o))

mig_data <- list(
  migrants  = d$Migrants,
  # N = nrow(d),
  N_regions = nr,
  origin = d$origin,
  destination = d$destination,
  log_distance = d$log_distance,
  pop_o = d$pop_o,
  pop_d = d$pop_d,
  hom_o = d$hom_o,
  hom_d = d$hom_d,
  soc_o = d$soc_o,
  soc_d = d$soc_d  
)

m2_alt <- ulam(
  alist(
    migrants ~ poisson( lambda ),
    log(lambda) <- a + gr[origin,1]  + gr[destination,2] + 
      b_pop_o*pop_o + b_pop_d*pop_d  + 
      b_hom_o*hom_o + b_hom_d*hom_d  +
      b_soc_o*soc_o + b_soc_d*soc_d  + 
      b_d*log_distance,
    c(b_pop_o, b_pop_d, b_hom_o, b_hom_d, b_soc_o, b_soc_d) ~ dnorm(0,1),
    b_d ~ dnorm(-1, 1),
    a ~ dnorm(1, 0.5),
         
    ## gr matrix of varying effects

    transpars> matrix[N_regions,2]: gr <-
      compose_noncentered( sigma_gr , L_Rho_d , z ),
    matrix[2,N_regions]: z ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]:L_Rho_d ~ lkj_corr_cholesky( 2 ),
    vector[2]:sigma_gr ~ dexp(1)
    
    
    # vector[2]:gr[N_regions] ~ multi_normal(0,Rho_gr,sigma_gr),
    # Rho_gr ~ lkj_corr(2),
    # sigma_gr ~ exponential(2)
  ),
  data = mig_data, iter = 4000, warmup = 1500, chains = 4, cores = 4
)

# save(m2, file = "./output/m_srm.rda")
 load(file = "./output/m_srm.rda")


o_group <- d %>% 
  group_by(origin) %>%
  summarize(pop_o_m = mean(pop_o),
            soc_o_m = mean(soc_o),
            hom_o_m = mean(hom_o)
  )

d_group <- d %>% 
  group_by(destination) %>%
  summarize(pop_d_m = mean(pop_d),
            soc_d_m = mean(soc_d),
            hom_d_m = mean(hom_d)
  )

precis(m2, depth = 1 , corr = true)
precis( m2 , depth=3 , pars=c("Rho_gr","sigma_gr") )

post <- extract.samples(m2 )

plot(post$b_soc_d, post$bp_soc_d, col=rangi2)
m1 <- lm(post$b_soc_d~post$b_pop_d + post$b_hom_d)
plot(post$b_soc_o, post$b_pop_o, col = rangi2)
m2 <- lm(post$b_soc_o~post$b_pop_o + post$b_hom_o)

ori <- sapply( 1:nr , function(i) post$a + post$gr[,i,1] )
des <- sapply( 1:nr , function(i) post$a + post$gr[,i,2] )
Eo_mu <- apply( exp(ori) , 2 , mean )
Ed_mu <- apply( exp(des) , 2 , mean )

plot( NULL , xlim=c(0,6) , ylim=c(0,6) , xlab="generalized origin" ,
      ylab="generalized destination" , lwd=1.5 )
abline(a=0,b=1,lty=2)

library(ellipse)
for ( i in 1:nr) {
  Sigma <- cov( cbind( ori[,i] , des[,i] ) )
  Mu <- c( mean(ori[,i]) , mean(des[,i]) )
  for ( l in c(0.5) ) {
    el <- ellipse( Sigma , centre=Mu , level=l )
    lines( exp(el) , col=col.alpha("black",0.5) )
  }
}

points( Eo_mu , Ed_mu , pch=21 , bg="white" , lwd=1.5 )

# m2 <- ulam(
#   alist(
#     migrants ~ dgampois( lambda, scale ),
#     log(lambda) <- a + gr[origin,1]  + gr[destination,2] + 
#       b_pop_o*pop_o + b_pop_d*pop_d  + 
#       b_hom_o*hom_o + b_hom_d*hom_d  +
#       b_soc_o*soc_o + b_soc_d*soc_d  + 
#       b_d*log_distance,
#     c(a, b_pop_o, b_pop_d, b_hom_o, b_hom_d, b_soc_o, b_soc_d) ~ normal(0,1),
#     b_d ~ dnorm(-1,1),
#     scale ~ dcauchy(0, 2),
#     
#     ## gr matrix of varying effects
#     vector[2]:gr[N_households] ~ multi_normal(0,Rho_gr,sigma_gr),
#     Rho_gr ~ lkj_corr(2),
#     sigma_gr ~ exponential(2)
#   ),
#   data = mig_data, iter = 3000, warmup = 1500, chains = 4, cores = 4,
#   constraints=list(scale="lower=0"),
#   start=list(scale=2) 
# )
