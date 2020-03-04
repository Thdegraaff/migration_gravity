######################
# Read in libraries
######################

library("tidyverse")
library("rethinking")
library("brms")
library("shinystan")
library("rcartocolor")

set.seed(5)

######################
# Get subsample of data
######################


nr <- 380 # for 2018 it is 380

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

d <- d %>%
  filter(housevalue_o > 0, housevalue_d >0)

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
d$hv_d <- log(d$housevalue_d) - mean(log(d$housevalue_d))
d$hv_o <- log(d$housevalue_o) - mean(log(d$housevalue_o))
d$hom_o <- log(d$homeowners_o) - mean(log(d$homeowners_o))
d$hom_d <- log(d$homeowners_d) - mean(log(d$homeowners_d))
d$hhsize_d <- log(d$hhgrootte_d) - mean(log(d$hhgrootte_d))
d$hhsize_o <- log(d$hhgrootte_o) - mean(log(d$hhgrootte_o))

################ Analysis #############

m_fixed_p <- brm(Migrants~ 0 + as.factor(origin) + as.factor(destination) +  log_distance,
                      prior = c(prior(normal(0, 2), class = b)),
                      family = poisson, data = d, iter = 4000,
                      warmup = 2000, cores = 4, chains = 4) 

pred_fixed_p <- predict(m_fixed_p)
d_pred_p <- cbind(d, pred_fixed_p)

check_fix_p <- d_pred_p %>% 
  group_by(origin) %>%
  summarize(sum_act  = sum(Migrants),
            sum_pred = sum(Estimate)
  )

m_varying_p <- brm(Migrants ~  log_distance + (1|origin) + (1|destination),
               prior = c(prior(normal(0, 2), class = Intercept),
                         prior(normal(0, 2), class = b),
                         prior(cauchy(0, 1), class = sd)),
               family = poisson, data = d, iter = 4000,
               warmup = 2000, cores = 4, chains = 4) 

pred_varying_p <- predict(m_varying_p)
d_varying_p <- cbind(d, pred_varying_p)

check_varying_p <- d_varying_p %>% 
  group_by(origin) %>%
  summarize(sum_act  = sum(Migrants),
            sum_pred = sum(Estimate)
  )

m_fixed_nb <- brm(Migrants~ 0 + as.factor(origin) + as.factor(destination) +  log_distance,
               prior = c(prior(normal(0, 2), class = b),
                         prior(gamma(0.01, 0.01), class = shape)),
               family = negbinomial, data = d, iter = 4000,
               warmup = 2000, cores = 4, chains = 4) 

pred_fixed_nb <- predict(m_fixed_nb)
d_pred_nb <- cbind(d, pred_fixed_nb)

check_fix_nb <- d_pred_nb %>% 
  group_by(origin) %>%
  summarize(sum_act  = sum(Migrants),
            sum_pred = sum(Estimate)
  )

m_varying_nb <- brm(Migrants ~  log_distance + (1|origin) + (1|destination),
                 prior = c(prior(normal(0, 2), class = Intercept),
                           prior(normal(0, 2), class = b),
                           prior(cauchy(0, 1), class = sd),
                           prior(gamma(0.01, 0.01), class = shape)),
                 family = negbinomial, data = d, iter = 4000,
                 warmup = 2000, cores = 4, chains = 4) 

pred_varying_nb <- predict(m_varying_nb)
d_varying_nb <- cbind(d, pred_varying_nb)

check_varying_nb <- d_varying_nb %>% 
  group_by(origin) %>%
  summarize(sum_act  = sum(Migrants),
            sum_pred = sum(Estimate)
  )

m_full_p <- brm(Migrants ~  log_distance +  pop_d + pop_o + hom_o + hom_d + 
                     soc_o + soc_d +
                     (1|origin) + (1|destination),
                   prior = c(prior(normal(0, 2), class = Intercept),
                             prior(normal(0, 2), class = b),
                             prior(cauchy(0, 1), class = sd)),
                   family = poisson, data = d, iter = 4000,
                   warmup = 2000, cores = 4, chains = 4) 

m_full_p <- add_criterion(m_full_p, "waic")
m_fixed_p <- add_criterion(m_fixed_p, "waic")
m_varying_p <- add_criterion(m_varying_p, "waic")
m_fixed_nb <- add_criterion(m_fixed_nb, "waic")
m_varying_nb<- add_criterion(m_varying_nb, "waic")

# compare the WAIC estimates
w <- loo_compare(m_full_p, m_fixed_p, m_varying_p, m_fixed_nb, m_varying_nb, 
                 criterion = "waic")

print(w, simplify = F)

cbind(waic_diff = w[, 1] * -2,
      se        = w[, 2] * 2)

model_weights(m_fixed_p, m_varying_p, m_full_p, 
              weights = "waic") %>% 
  round(digits = 2)

w[3:5, 7:8] %>% 
  data.frame() %>% 
  rownames_to_column(var = "model_name") %>% 
  
  ggplot(aes(x    = model_name, 
             y    = waic, 
             ymin = waic - se_waic, 
             ymax = waic + se_waic)) +
  geom_pointrange(shape = 21, color = carto_pal(7, "BurgYl")[7], fill = carto_pal(7, "BurgYl")[5]) +
  coord_flip() +
  labs(x = NULL, y = NULL,
       title = "My custom WAIC plot") +
  theme_classic() +
  theme(text             = element_text(family = "Courier"),
        axis.ticks.y     = element_blank(),
        panel.background = element_rect(fill = alpha(carto_pal(7, "BurgYl")[3], 1/4)))
