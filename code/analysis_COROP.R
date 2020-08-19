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
  lrentB = df$lrentB
)

pairs( ~ lpopA + lpopB, data = df, col= rangi2)
pairs( ~ lsocA + lsocB, data = df, col= rangi2)

m <- ulam(
  alist(
    mAB ~ poisson( lambdaAB ),
    mBA ~ poisson( lambdaBA ),
    log(lambdaAB) <- cons + 
      b_popA * lpopA + b_popB * lpopB + 
      b_dist*ldist + 
      b_hA  * lhomA + b_hB * lhomB + b_sA * lsocA + b_sB * lsocB + 
      #b_hA_int  * lhomA * lpopA + b_hB_int * lhomB * lpopB + b_sA_int * lsocA * lpopA + b_sB_int * lsocB * lpopB + 
      gr[origin,1] + gr[destination,2] +   
      y[year] + 
      d[did,1],
    log(lambdaBA) <- cons + 
      b_popB * lpopA + b_popA * lpopB + 
       b_dist*ldist + 
       b_hA  * lhomB + b_hB * lhomA + b_sA * lsocB + b_sB * lsocA + 
       #b_hB_int  * lhomA * lpopA + b_hA_int * lhomB * lpopB + b_sB_int * lsocA * lpopA + b_sA_int * lsocB * lpopB + 
      gr[destination,1] + gr[origin,2] +   
      y[year] + 
      d[did, 2],
    b_dist ~ normal(-1.5, 0.5),
    c(b_popA, b_popB) ~ normal(1, 0.5),
    c(b_hA, b_sA, b_hB, b_sB) ~ normal(0, 1),
    #c(b_hA_int, b_sA_int, b_hB_int, b_sB_int) ~ normal(0, 1),
    cons ~ normal(3,3),
    y[year] ~ normal(0, sigma_y),
    sigma_y ~ dexp(2),
    
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
  ), data = m_data , chains = 4 , cores = 4 , iter = 4000, warmup = 1000 )

precis(m)
save(m, file = "./output/corop_final_model.rda")
precis( m , depth=3 , pars=c("Rho_1", "Rho_2", "sigma_d", "sigma_gr") ) #, "Rho_d","sigma_d") )
pairs(m@stanfit, pars=c("b_sA", "b_sB", "b_hA", "b_hB", "b_popA", "b_popB") )
pairs(m@stanfit, pars=c("b_popA", "b_popB") )#, "b_sd", "b_so") )
traceplot(m, pars=c("b_dist"))#, "b_sA", "b_sB", "b_hA", "b_hB", "sigma_y") )
trankplot(m, pars=c("b_dist","b_sA", "b_sB", "b_hA", "b_hB", "sigma_d") )

post <- extract.samples( m )
ori <- sapply( 1:nr_regions , function(i) post$cons + post$gr[,i,1] )
des <- sapply( 1:nr_regions , function(i) post$cons + post$gr[,i,2] )
Eo_mu <- apply( exp(ori) , 2 , mean )
Ed_mu <- apply( exp(des) , 2 , mean )

plot( NULL , xlim=c(0 , 400) , ylim=c(0 , 300) , xlab="generalized origin" ,
      ylab="generalized destination" , lwd= 1.5 )
abline(a=0, b=1, lty=2)

# ellipses1
for ( i in 1:nr_regions ) {
  Sigma <- cov( cbind( ori[,i] , des[,i] ) )
  Mu <- c( mean(ori[,i]) , mean(des[,i]) )
  for ( l in c(0.95) ) {
    el <- ellipse( Sigma , centre=Mu , level=l )
    lines( exp(el) , col=col.alpha("black",0.5) )
  }
}
# household means
points( Eo_mu , Ed_mu , pch=21 , bg="white" , lwd=1.5 )

dy1 <- apply( post$d[,,1] , 2 , mean )
dy2 <- apply( post$d[,,2] , 2 , mean )
plot( dy1 , dy2, col = col.alpha(rangi2, 0.7) )
abline(0, 0, lty = 2)
abline(v = 0, lty =2)
abline(a=0, b=1, lty=2)
