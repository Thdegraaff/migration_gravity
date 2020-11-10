library(rethinking)

# prep data
data( UCBadmit )
UCBadmit$male <- as.integer(UCBadmit$applicant.gender=="male")
UCBadmit$dept <- rep( 1:6 , each=2 )
UCBadmit$applicant.gender <- NULL

# varying intercepts model
m_glmm1 <- suppressMessages(
  ulam(
  alist(
    admit ~ binomial(applications,p),
    logit(p) <- a[dept] + b*male,
    a[dept] ~ normal( abar , sigma ),
    abar ~ normal( 0 , 4 ),
    sigma ~ half_normal(0,1),
    b ~ normal(0,1)
  ), data=UCBadmit )
)

example(cxxfunction, package = "inline", run.dontrun = TRUE)

library(readr)

Rcpp::sourceCpp(code = 
                  '
#include <Rcpp.h>
using namespace Rcpp; 

// [[Rcpp::export]]
int throw_exception() { 
  std::stringstream errmsg; errmsg << "this is an exception";
  throw std::domain_error(errmsg.str()); 
  return 0;
}
'
)

throw_exception()