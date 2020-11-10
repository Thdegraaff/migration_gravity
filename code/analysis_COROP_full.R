######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")
library("brms")
library("rcartocolor")
library("ellipse")

set.seed(5)

######################
# Read in data
######################

load(file = "./data/derived/migration_COROP.Rda")
load(file = "./data/derived/dmat.Rda")

 df <- df %>%
     filter(year == 2012 | year == 2013 | year == 2014 | year == 2015 | year == 2016 | year == 2017)

cor_df <- df %>% select(
  lhomA, lhomB, lsocA, lsocB, lrentA, lrentB, lpopA, lpopB
)

round(cor(cor_df, use = "complete.obs"), 2)

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
  lrentA = df$lrentA,
  lrentB = df$lrentB, 
  dmat = dmat
)

pairs( ~ lpopA + lpopB, data = df, col= rangi2)
pairs( ~ lsocA + lsocB, data = df, col= rangi2)

m <- ulam(
  alist(
    mAB ~ poisson( lambdaAB ),
    mBA ~ poisson( lambdaBA ),
    log(lambdaAB) <- cons + 
      lpopA + lpopB + 
      b_dist*ldist +  
      b_hA  * lhomA + b_hB * lhomB + b_sA * lsocA + b_sB * lsocB + 
      gsA[origin] + gsB[destination] +
      gr[origin, 1] + gr[destination, 2] +
      y[year] + 
       d[did,1],
    log(lambdaBA) <- cons + 
      lpopA + lpopB + 
      b_dist*ldist +  
      b_hA  * lhomB + b_hB * lhomA + b_sA * lsocB + b_sB * lsocA + 
      gsA[destination] + gsB[origin] +   
      gr[destination, 1] + gr[origin, 2] +
      y[year] + 
       d[did, 2],
    b_dist ~ normal(-1.5, 0.5),
    c(b_popA, b_popB) ~ normal(1, 0.5),
    c(b_hA, b_sA, b_hB, b_sB) ~ normal(0, 1),
    cons ~ normal(3,3),
    y[year] ~ normal(4.5, sigma_y),
    sigma_y ~ dexp(2),

    ## gr matrix of varying effects
    transpars> matrix[nr_regions, 2]: gr <-
      compose_noncentered( sigma_gr , L_Rho_d1 , z1),
    matrix[2,nr_regions]: z1 ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]: L_Rho_d1 ~ lkj_corr_cholesky( 2 ),
    vector[2]: sigma_gr ~ dexp(1),
    
    ## gs matrix and vectors of spatial varying regional effects
    # transpars> vector[nr_regions]: gs_A <<- L_SIGMA * gr[ , 1],
    # transpars> vector[nr_regions]: gs_B <<- L_SIGMA * gr[ , 2],
    transpars> vector[nr_regions]: gsA <<- L_SIGMA * gr[ , 1],
    transpars> vector[nr_regions]: gsB <<- L_SIGMA * gr[ , 2],
    transpars> matrix[nr_regions, nr_regions]:L_SIGMA <<- cholesky_decompose( SIGMA ),
    transpars> matrix[nr_regions, nr_regions]:SIGMA <- cov_GPL2(dmat, etasq, rhosq, 0.01),
    etasq ~ dexp( 4 ),
    rhosq ~ dexp( 0.4 ),

    ## dyad effects
    transpars> matrix[nr,2]:d <-
      compose_noncentered( rep_vector(sigma_d,2) , L_Rho_d2 , z2 ),
    matrix[2,nr]:z2 ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]: L_Rho_d2 ~ lkj_corr_cholesky( 2 ),
    sigma_d ~ exponential(1),

    ## compute correlation matrix for groups
    gq> matrix[2,2]:Rho_1 <<- Chol_to_Corr( L_Rho_d1 ),
    
    ## compute correlation matrix for dyads
    gq> matrix[2,2]:Rho_2 <<- Chol_to_Corr( L_Rho_d2 ),
    
    ## compute correlation matrix for spatial weight
    gq> matrix[40,40]:Rho_sp <<- Chol_to_Corr( L_SIGMA )
  ), data = m_data , chains = 4 , cores = 4 , iter = 2000, warmup = 1000 )

precis(m)
save(m, file = "./output/corop_null_model_full.rda")

precis( m , depth=3 , pars=c("Rho_sp") ) #, "Rho_d","sigma_d") )
precis( m , depth=3 , pars=c("gsA") ) #, "Rho_d","sigma_d") )
precis( m , depth=3 , pars=c("Rho_1", "Rho_2", "sigma_d", "sigma_gr") ) #, "Rho_d","sigma_d") )
pairs(m@stanfit, pars=c("b_sA", "b_sB", "b_hA", "b_hB") )
pairs(m@stanfit, pars=c("b_popA", "b_popB") )#, "b_sd", "b_so") )
traceplot(m, pars=c("b_dist", "sigma_d", "etasq") )
trankplot(m, pars=c("b_dist","b_sA", "b_sB", "b_hA", "b_hB", "sigma_d") )

post <- extract.samples( m )
ori <- sapply( 1:nr_regions , function(i) post$cons + post$gsA[,i] )
des <- sapply( 1:nr_regions , function(i) post$cons + post$gsB[,i] )
Eo_mu <- apply( exp(ori) , 2 , mean )
Ed_mu <- apply( exp(des) , 2 , mean )

plot( NULL , xlim=c(0 , 700) , ylim=c(0 , 300) , xlab="generalized origin" ,
      ylab="generalized destination" , lwd= 1.5 )
abline(a=0, b=1, lty=2)

# ellipses1
for ( i in 1:nr_regions ) {
  Sigma <- cov( cbind( ori[,i] , des[,i] ) )
  Mu <- c( mean(ori[,i]) , mean(des[,i]) )
  for ( l in c(0.5) ) {
    el <- ellipse( Sigma , centre=Mu , level=l )
    lines( exp(el) , col=col.alpha("black",0.75) )
  }
}
# household means
points( Eo_mu , Ed_mu , pch=21 , bg="black" , lwd=1.5 )

dy1 <- apply( post$d[,,1] , 2 , mean )
dy2 <- apply( post$d[,,2] , 2 , mean )
plot( dy1 , dy2, col = col.alpha(rangi2, 0.7) )
abline(0, 0, lty = 2)
abline(v = 0, lty =2)
abline(a=0, b=1, lty=2)

# plot the posterior median covariance function
plot( NULL , xlab="distance (hundred km)" , ylab="spatial covariance" ,
      xlim=c(0,3.5) , ylim=c(0,0.7) )

# compute posterior mean covariance
x_seq <- seq( from=0 , to=3.5 , length.out=100 )
pmcov <- sapply( x_seq , function(x) post$etasq*exp(-post$rhosq*x^2) )
pmcov_mu <- apply( pmcov , 2 , mean )
lines( x_seq , pmcov_mu , lwd=5 )

# plot 50 functions sampled from posterior
for ( i in 1:100 )
  curve( post$etasq[i]*exp(-post$rhosq[i]*x^2) , add=TRUE ,
         col=col.alpha("black",0.2) )

library(rstan)
example(stan_model, run.dontrun = TRUE)
