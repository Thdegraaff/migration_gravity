## R code 14.30
library(rethinking)
data(KosterLeckie)

## R code 14.31
kl_data <- list(
  N = nrow(kl_dyads),
  N_households = max(kl_dyads$hidB),
  did = kl_dyads$did,
  hidA = kl_dyads$hidA,
  hidB = kl_dyads$hidB,
  giftsAB = kl_dyads$giftsAB,
  giftsBA = kl_dyads$giftsBA
)

m14.7 <- ulam(
  alist(
    giftsAB ~ poisson( lambdaAB ),
    giftsBA ~ poisson( lambdaBA ),
    log(lambdaAB) <- a + gr[hidA,1] + gr[hidB,2] + d[did,1] ,
    log(lambdaBA) <- a + gr[hidB,1] + gr[hidA,2] + d[did,2] ,
    a ~ normal(0,1),
    
    ## gr matrix of varying effects
    vector[2]:gr[N_households] ~ multi_normal(0,Rho_gr,sigma_gr),
    Rho_gr ~ lkj_corr(4),
    sigma_gr ~ exponential(1),
    
    ## dyad effects
    transpars> matrix[N,2]:d <-
      compose_noncentered( rep_vector(sigma_d,2) , L_Rho_d , z ),
    matrix[2,N]:z ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]:L_Rho_d ~ lkj_corr_cholesky( 8 ),
    sigma_d ~ exponential(1),
    
    ## compute correlation matrix for dyads
    gq> matrix[2,2]:Rho_d <<- Chol_to_Corr( L_Rho_d )
  ), data=kl_data , chains=4 , cores=4 , iter=2000 )

## R code 14.32
precis( m14.7 , depth=3 , pars=c("Rho_gr","sigma_gr") )

## R code 14.33
post <- extract.samples( m14.7 )
g <- sapply( 1:25 , function(i) post$a + post$gr[,i,1] )
r <- sapply( 1:25 , function(i) post$a + post$gr[,i,2] )
Eg_mu <- apply( exp(g) , 2 , mean )
Er_mu <- apply( exp(r) , 2 , mean )

## R code 14.34
plot( NULL , xlim=c(0,8.6) , ylim=c(0,8.6) , xlab="generalized giving" ,
      ylab="generalized receiving" , lwd=1.5 )
abline(a=0,b=1,lty=2)

# ellipses
library(ellipse)
for ( i in 1:25 ) {
  Sigma <- cov( cbind( g[,i] , r[,i] ) )
  Mu <- c( mean(g[,i]) , mean(r[,i]) )
  for ( l in c(0.5) ) {
    el <- ellipse( Sigma , centre=Mu , level=l )
    lines( exp(el) , col=col.alpha("black",0.5) )
  }
}
# household means
points( Eg_mu , Er_mu , pch=21 , bg="white" , lwd=1.5 )

## R code 14.35
precis( m14.7 , depth=3 , pars=c("Rho_d","sigma_d") )

## R code 14.36
dy1 <- apply( post$d[,,1] , 2 , mean )
dy2 <- apply( post$d[,,2] , 2 , mean )
plot( dy1 , dy2 )
