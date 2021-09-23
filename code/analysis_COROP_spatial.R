######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")
library("brms")
library("rcartocolor")
library("ellipse")
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
load(file = "./data/derived/dmat.Rda")

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
      b_popA * lpopA + b_popB * lpopB + 
      b_dist*ldist + 
      b_hA  * lhomA + b_hB * lhomB + b_sA * lsocA + b_sB * lsocB + 
      #b_hA_int  * lhomA * lpopA + b_hB_int * lhomB * lpopB + b_sA_int * lsocA * lpopA + b_sB_int * lsocB * lpopB + 
      gsA[origin] + gsB[destination] +   
      y[year] + 
      d[did,1],
    log(lambdaBA) <- cons + 
      b_popB * lpopA + b_popA * lpopB + 
      b_dist*ldist + 
      b_hA  * lhomB + b_hB * lhomA + b_sA * lsocB + b_sB * lsocA + 
      #b_hB_int  * lhomA * lpopA + b_hA_int * lhomB * lpopB + b_sB_int * lsocA * lpopA + b_sA_int * lsocB * lpopB + 
      gsA[destination] + gsB[origin] +   
      y[year] + 
      d[did, 2],
    b_dist ~ normal(-1.5, 0.5),
    c(b_popA, b_popB) ~ normal(1, 0.5),
    c(b_hA, b_sA, b_hB, b_sB) ~ normal(0, 1),
    #c(b_hA_int, b_sA_int, b_hB_int, b_sB_int) ~ normal(0, 1),
    cons ~ normal(3,3),
    y[year] ~ normal(0, sigma_y),
    sigma_y ~ dexp(2),

    ## gs matrix and vectors of spatial varying regional effects
    # transpars> vector[nr_regions]: gs_A <<- L_SIGMA * gr[ , 1],
    # transpars> vector[nr_regions]: gs_B <<- L_SIGMA * gr[ , 2],
    transpars> vector[nr_regions]: gsA <<- L_SIGMA * zsA,
    transpars> vector[nr_regions]: gsB <<- L_SIGMA * zsB,
    vector[nr_regions]: zsA ~ normal( 0 , 1 ),
    vector[nr_regions]: zsB ~ normal( 0 , 1 ),
    transpars> matrix[nr_regions, nr_regions]:L_SIGMA <<- cholesky_decompose( SIGMA ),
    transpars> matrix[nr_regions, nr_regions]:SIGMA <- cov_GPL2(dmat, etasq, rhosq, 0.01),

    etasq ~ dexp( 4 ),
    rhosq ~ dexp( 0.4 ),

    ## gr matrix of varying effects
    # transpars> matrix[nr_regions, 2]: gr <-
    #   compose_noncentered( sigma_gr , L_Rho_d1 , z1),
    # matrix[2,nr_regions]: z1 ~ normal( 0 , 1 ),
    # cholesky_factor_corr[2]: L_Rho_d1 ~ lkj_corr_cholesky( 2 ),
    # vector[2]: sigma_gr ~ dexp(1),
    
    ## dyad effects
    transpars> matrix[nr,2]:d <-
      compose_noncentered( rep_vector(sigma_d,2) , L_Rho_d2 , z2 ),
    matrix[2,nr]:z2 ~ normal( 0 , 1 ),
    cholesky_factor_corr[2]: L_Rho_d2 ~ lkj_corr_cholesky( 2 ),
    sigma_d ~ exponential(1),


    ## compute correlation matrix for groups
    # gq> matrix[2,2]:Rho_1 <<- Chol_to_Corr( L_Rho_d1 ),
    
    ## compute correlation matrix for dyads
    gq> matrix[2,2]:Rho_2 <<- Chol_to_Corr( L_Rho_d2 ),
    
    ## compute correlation matrix for spatial weight
    gq> matrix[40,40]:Rho_sp <<- Chol_to_Corr( L_SIGMA )
  ), data = m_data , chains = 4 , cores = 4 , iter = 4000, warmup = 1000 )

precis(m)
save(m, file = "./output/corop_model_spatial.rda")

precis( m , depth=3 , pars=c("Rho_sp") ) #, "Rho_d","sigma_d") )
precis( m , depth=3 , pars=c("gsA") ) #, "Rho_d","sigma_d") )
precis( m , depth=3 , pars=c("Rho_2", "sigma_d") ) #, "Rho_d","sigma_d") )
pairs(m@stanfit, pars=c("b_sA", "b_sB", "b_hA", "b_hB") )
pairs(m@stanfit, pars=c("b_popA", "b_popB") )#, "b_sd", "b_so") )
traceplot(m, pars=c("b_dist"))#, "b_sA", "b_sB", "b_hA", "b_hB", "sigma_y") )
trankplot(m, pars=c("b_dist","b_sA", "b_sB", "b_hA", "b_hB", "sigma_d") )

post <- extract.samples( m )
g <- sapply( 1:nr_regions , function(i) post$cons + post$gsA[,i] )
r <- sapply( 1:nr_regions , function(i) post$cons + post$gsB[,i] )
tibble(g = exp(g[, 1]),
       r = exp(r[, 1])) %>% 
  ggplot(aes(x = g, y = r)) +
  geom_abline(color = "#FCF9F0", linetype = 2, alpha = 1/3) + # white "#FCF9F0" # gold "#B1934A"
  geom_point(color = "#B1934A", alpha = 1/3, size = 1/4) +
  stat_ellipse(type = "norm", level = .5, size = 1/2, color = "#80A0C7") +
  stat_ellipse(type = "norm", level = .9, size = 1/2, color = "#80A0C7") +
  labs(x = expression(giving[italic(i)==1]),
       y = expression(receiving[italic(i)==1])) +
  coord_equal(xlim = c(0, 700),
              ylim = c(0, 250))

data_scatter <- rbind(exp(g), exp(r)) %>% 
  data.frame() %>% 
  set_names(1:nr_regions) %>% 
  mutate(parameter = rep(c("g", "r"), each = n() / 2),
         iter      = rep(1:10000, times = 2)) %>% 
  pivot_longer(-c(parameter, iter),
               names_to = "region") %>% 
  pivot_wider(names_from = parameter,
              values_from = value) %>% 
  group_by(region) %>% 
  mutate(mu_g = mean(g),
         mu_r = mean(r)) %>% 
  nest(data = c("g", "r", "iter")) 
  
p_scatter<-  ggplot(data = data_scatter, aes(group = region)) +
  geom_abline(color = "#FCF9F0", linetype = 2, alpha = 1/3) +
  stat_ellipse(data = . %>% unnest(data),
               aes(x = g, y = r),
               type = "norm", level = .5, size = 1/2, alpha = 1/2, color = "#80A0C7") +
  geom_point(aes(x = mu_g, y = mu_r),
             color = "#DCA258") +
  labs(x = "generalized push",
       y = "generalized pull") +
  coord_equal(xlim = c(0, 400),
              ylim = c(0, 400))

pdf(file = "./fig/scatter.pdf" ,width=5,height=4) 
p_scatter_spatial
dev.off()

dy1 <- apply( post$d[,,1] , 2 , mean )
dy2 <- apply( post$d[,,2] , 2 , mean )

p_dyad <- ggplot(data = data.frame(dy1, dy2), aes(dy1, dy2), color = col.alpha("blue",1)) + 
  geom_point(color = "#8B9DAF", alpha = 1/2, size = 1/2) +
  xlab("Region i in dyad")+ 
  ylab("region j in dyad") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  geom_hline(yintercept=0, color = "#FCF9F0", linetype = 2, alpha = 1/3) + 
  geom_vline(xintercept=0, color = "#FCF9F0", linetype = 2, alpha = 1/3) + 
  geom_abline(intercept = 0, slope = 1,  color = "#FCF9F0", linetype = 2, alpha = 1/3)
p_dyad

pdf(file = "./fig/dyad.pdf" ,width=5,height=4) 
p_dyad_spatial
dev.off()

eta2 <- mean(post$etasq)
rho2 <- mean(post$rhosq)

# Create plot and add first layer
p <- ggplot(data = data.frame(x = 0:1, y = 0:1), 
            aes(x = x, y = y) ) + 
            ylim(0, 0.5) +
            xlab("distance (hundred kilometers)")+ 
            ylab("spatial covariance") + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Then cycle in all the curve
for (i in 1:25) {
  p <- p + stat_function(fun = function(x, i) (post$etasq[i]*exp(-post$rhosq[i]*x^2) ), size = 1/4, alpha = 1/4, color = "cornflowerblue", args=list(i=i)) 
}
p <- p + stat_function(fun = function(x) (eta2*exp(-rho2*x^2) ), color = "cornflowerblue", size = 1) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
print(p)

ggsave(p, file="./fig/spatial_autocorrelation.pdf", width  = 100, height = 80, units = "mm")
