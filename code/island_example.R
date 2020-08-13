## R code 14.37
# load the distance matrix
library(rethinking)
data(islandsDistMatrix)

# display (measured in thousands of km)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml","Ti","SC","Ya","Fi","Tr","Ch","Mn","To","Ha")
round(Dmat,1)

## R code 14.38
# linear
curve( exp(-1*x) , from=0 , to=4 , lty=2 )
# squared
curve( exp(-1*x^2) , add=TRUE )

## R code 14.39
data(Kline2) # load the ordinary data, now with coordinates
d <- Kline2
d$society <- 1:10 # index observations

dat_list <- list(
  T = d$total_tools,
  P = d$population,
  society = d$society,
  Dmat=islandsDistMatrix )

m14.8 <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- (a*P^b/g)*exp(k[society]),
    vector[10]:k ~ multi_normal( 0 , SIGMA ),
    matrix[10,10]:SIGMA <- cov_GPL2( Dmat , etasq , rhosq , 0.01 ),
    c(a,b,g) ~ dexp( 1 ),
    etasq ~ dexp( 2 ),
    rhosq ~ dexp( 0.5 )
  ), data=dat_list , chains=4 , cores=4 , iter=2000 )

## R code 14.40
precis( m14.8 , depth=3 )

m14.8nc <- ulam(
  alist(
    T ~ dpois(lambda),
    lambda <- (a*P^b/g)*exp(k[society]),
    
    # non-centered Gaussian Process prior
    transpars> vector[10]: k <<- L_SIGMA * z,
    vector[10]: z ~ normal( 0 , 1 ),
    transpars> matrix[10,10]: L_SIGMA <<- cholesky_decompose( SIGMA ),
    transpars> matrix[10,10]: SIGMA <- cov_GPL2( Dmat , etasq , rhosq , 0.01 ),
    
    c(a,b,g) ~ dexp( 1 ),
    etasq ~ dexp( 2 ),
    rhosq ~ dexp( 0.5 )
  ), data=dat_list , chains=4 , cores=4 , iter=2000 )

precis( m14.8nc  , depth=3 , pars=c("SIGMA") ) #, "Rho_d","sigma_d") )
