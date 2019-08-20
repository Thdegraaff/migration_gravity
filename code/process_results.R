library(shinystan)
library(brms)
library(tidyverse)
library(bayesplot)
library(ggthemes)
library(sf)
library(RColorBrewer)
library(cowplot)
library(hrbrthemes)

######################
# Get subsample of data
######################

nr <- 393

load(file = "./output/m_nb.Rda")

samples <- posterior_samples(m2_neg)
samples <- samples %>%
  rename(`Intercept` = `b_Intercept`, 
         `log(pop_i)` = `b_pop_o`,
         `log(pop_j)` = `b_pop_d`,
         `log(home_i)` = `b_hom_o`,
         `log(home_j)` = `b_hom_d`,
         `log(soc_i)` = `b_soc_o`,
         `log(soc_j)` = `b_soc_d`,
         `log(dist_ij)` = `b_log_distance`,
         `sigma_o` = `sd_origin__Intercept`,
         `sigma_d` = `sd_destination__Intercept`,
         `tau` = `shape`
         )

forestplot <- mcmc_intervals(samples, pars = c("Intercept", "log(pop_i)", "log(pop_j)", "log(home_i)", 
                                   "log(home_j)", "log(soc_i)", "log(soc_j)", "log(dist_ij)", 
                                   "sigma_o", "sigma_d", "tau")) +
  theme_economist() +
  geom_vline(xintercept = 0)

pdf(file = "./fig/forestplot.pdf", width = 4, height = 5)
forestplot
dev.off()

######################
# Read in data
######################

load(file = "./data/derived/migration.Rda")

d <- data

d$origin <- as.numeric(as.factor(d$code_o))
d$destination <- as.numeric(as.factor(d$code_d))
d <- select(d, -code_o, -code_d)
d <- d %>% filter(origin <= nr, destination <= nr)

d$log_distance <- log(d$distance) - mean(log(d$distance))
d$pop_o <- log(d$pop_o) - mean(log(d$pop_o) )
d$pop_d <- log(d$pop_d) - mean(log(d$pop_d) )
d$soc_d <- log(d$socialhousing_d + 0.0001 )
d$soc_o <- log(d$socialhousing_o + 0.0001 )
d$soc_d <- d$soc_d - mean(d$soc_d)
d$soc_o <- d$soc_o - mean(d$soc_o)
d$hom_o <- log(d$homeowners_o) - mean(log(d$homeowners_o))
d$hom_d <- log(d$homeowners_d) - mean(log(d$homeowners_d))

######################
# Create new data and difference
######################

new_d <- d %>% 
  mutate(
    hom_o = ifelse(origin == 132, hom_o + 0.1, hom_o),
    hom_d = ifelse(destination == 132, hom_d + 0.1, hom_d),
    soc_o = ifelse(origin == 132, soc_o + 0.1, soc_o),
    soc_d = ifelse(destination == 132, soc_d + 0.1, soc_d),
  )

fit_old <- fitted(m2_neg,  nsamples = 1000, scale = "response")
mean_old <- fit_old[ , 1]
fit_new <- fitted(m2_neg, nsamples = 1000, newdata = new_d, scale = "response")
mean_new <- fit_new[ , 1]


mean_diff <- mean_new - mean_old

######################
# adjust dataframe
######################

new_d <- data.frame(new_d, mean_diff)

######################
# read in map
######################

# Define colour palette

myPal = colorRampPalette(brewer.pal(9,"PRGn"))(100)

# Load map

municipalities <- st_read(dsn = "./data/src/gem_2015.shp")
st_crs(municipalities) = 28992

# Filter out water areas (WATER = "JA") and
# Belgium areas (Baarle Nassau; WATER = "B")

municipalities <- municipalities %>%
  filter(WATER == "NEE")

diff_in <- new_d %>%
  filter(destination == 132) %>%
  select(origin, mean_diff) %>%
  rbind(c("132", 0)) %>%
  arrange(as.numeric(origin)) %>%
  mutate(mean_diff = as.numeric(mean_diff))

diff_out <- new_d %>%
  filter(origin == 132) %>%
  select(destination, mean_diff) %>%
  rbind(c("132", 0)) %>%
  arrange(as.numeric(destination)) %>%
  mutate(mean_diff = as.numeric(mean_diff))

municipalities$diff_in <- diff_in[ , 2]
municipalities$diff_out <- diff_out[ , 2]

p_diff_in <- ggplot() + geom_sf(data = municipalities, aes(fill = diff_in), lwd = 0.4) + 
  scale_fill_distiller("Difference \nin in-flow ", direction = -1 ) +
  scale_color_gradient(high = "white", low = "red") + 
  theme_bw() 
p_diff_out <- ggplot() + geom_sf(data = municipalities, aes(fill = diff_out), lwd = 0.4) + 
  scale_fill_distiller("Difference \nin out-flow",  direction = -1 ) +
  scale_color_gradient(high = "white", low = "red") + 
  theme_bw() 

pdf(file = "./fig/p_diff_in.pdf" ,width = 9, height = 8) 
p_diff_in 
dev.off()

pdf(file = "./fig/p_diff_out.pdf" ,width = 9, height = 8) 
p_diff_out
dev.off()

######################
#  make histogram fitted
######################


######################
# first get migration data
######################

migration <- read.csv2(file = "./data/src/Tussen_gemeenten_verhuisde_personen_20122018_120551.csv", 
                       header = TRUE)
migration <- drop_na(migration) # drop all municapalities with NA; now dataset is 393 * 393 - 393
i <- sapply(migration, is.factor)
migration[i] <- lapply(migration[i], as.character)

data <- rename(migration, 
               origin = Regio.van.vertrek, 
               destination = Regio.van.vestiging,
               migrants= Tussen.gemeenten.verhuisde.personen..aantal.)

######################
# Then get fitted data
######################

fit <- as.data.frame(round(fit_old) )

######################
# Create new data frame
######################

nr_obs <- nr * (nr - 1)

fit_data <- data.frame(
  type = c( rep("Observed", nr_obs), rep("Predicted", nr_obs) ),
  Migrants = c( data$migrants, fit$Estimate) 
)

# fit_large <- filter(fit, Estimate >= 20)
# fit_small <- filter(fit, Estimate < 20)
# hist_fit_small <- ggplot(data = fit_small, aes(Estimate)) + 
#   geom_histogram(col = "black", fill = "forest green", alpha = 0.7, bins = 20) +
#   theme_bw()
# hist_fit_large <- ggplot(data = fit_large, aes(Estimate)) + 
#   geom_histogram(col = "black", fill = "forest green", alpha = 0.7, bins = 20) +
#   scale_x_continuous(breaks=seq(20, 120000, 25000)) +
#   theme_bw()
# hist_fit <- plot_grid(hist_fit_small, hist_fit_large, labels = c("Small flows", "Large flows"), label_x = 0.5, label_y = 0.96) 

# p <- fit_data %>%
#   ggplot( aes(x=value, fill=type)) +
#   geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
#   scale_fill_manual(values=c("#69b3a2", "#404080")) +
#   theme_ipsum() +
#   labs(fill="")

fit_large <- filter(fit_data, Migrants >= 20 & Migrants <= 4020)
fit_small <- filter(fit_data, Migrants < 20)
hist_fit_small <- ggplot(data = fit_small, aes(Migrants, fill = type)) + 
                         geom_histogram( color="black", alpha=0.7, position = 'dodge' , bins = 20) +
                         scale_fill_manual(values=c("forest green", "deepskyblue")) +
                         theme_bw() +
                         labs(fill="") 
hist_fit_large <- ggplot(data = fit_large, aes(Migrants, fill = type)) + 
                          geom_histogram( color="black", alpha=0.7, position = 'dodge', bins = 20) +
                          scale_x_continuous(breaks=seq(20, 4020, 1000)) +
                          scale_fill_manual(values=c("forest green", "deepskyblue")) +
                          theme_bw() +
                          labs(fill="")
hist_fit <- plot_grid(hist_fit_small + theme(legend.position = "none"), 
                      hist_fit_large + theme(legend.position = "none"), 
                      labels = c("Small flows", "Large flows"), 
                      label_x = 0.5, label_y = 0.96) 

legend_b <- get_legend(hist_fit_small + theme(legend.position="bottom"))

hist_fit <- plot_grid( hist_fit, legend_b, ncol = 1, rel_heights = c(1,.1) )

pdf(file = "./fig/hist_fit.pdf" ,width=8,height=4) 
hist_fit
dev.off()
