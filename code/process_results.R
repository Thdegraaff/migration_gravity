library(shinystan)
library(brms)
library(tidyverse)
library(bayesplot)
library(ggthemes)
library(sf)
library(RColorBrewer)
library(cowplot)
library(hrbrthemes)
library(dutchmasters)
library(rethinking)

######################
# Set Dutch masters theme
######################

theme_pearl_earring <- function(light_color = "#E8DCCF", 
                                dark_color = "#100F14", 
                                my_family = "Courier",
                                ...) {
  
  theme(line = element_line(color = light_color),
        text = element_text(color = light_color, family = my_family),
        strip.text = element_text(color = light_color, family = my_family),
        axis.text = element_text(color = light_color),
        axis.ticks = element_line(color = light_color),
        axis.line = element_blank(),
        legend.background = element_rect(fill = dark_color, color = "transparent"),
        legend.key = element_rect(fill = dark_color, color = "transparent"),
        panel.background = element_rect(fill = dark_color, color = light_color),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = dark_color, color = dark_color),
        strip.background = element_rect(fill = dark_color, color = "transparent"),
        ...)
  
}

# now set `theme_pearl_earring()` as the default theme
theme_set(theme_pearl_earring())

######################
# Read in data
######################

######################
# Get subsample of data
######################

nr <- 40

load(file = "./output/corop_final_model.rda")


######## From rethinking package

precis(m)
list <-  extract.samples( m )
samples <- data.frame(list[1:10])
samples <- samples[, c(1:8)]

samples <- samples %>%
  rename(`Intercept` = `cons`, 
         `log(pop_i)` = `b_popA`,
         `log(pop_j)` = `b_popB`,
         `log(home_i)` = `b_hA`,
         `log(home_j)` = `b_hB`,
         `log(soc_i)` = `b_sA`,
         `log(soc_j)` = `b_sB`,
         `log(dist_ij)` = `b_dist`
         )


forestplot <- mcmc_intervals(samples, pars = c("Intercept", "log(pop_i)", "log(pop_j)", "log(home_i)", 
                                   "log(home_j)", "log(soc_i)", "log(soc_j)", "log(dist_ij)"))  +
  geom_vline(xintercept = c(-2, -1, 0, 1, 2, 3 , 4), linetype = c(2, 2, 1, 2, 2, 2,2), size = c(1/4, 1/4, 1/2, 1/4, 1/4, 1/4, 1/4), color = "#FCF9F0", alpha = 1/4) 

pdf(file = "./fig/forestplot.pdf", width = 4, height = 5)
forestplot
dev.off()

######################
# Read in data
######################

load(file = "./data/derived/migration_COROP.Rda")

df <- df %>% filter(year == 2018)

nr_regions = max(df$destination)

mig_data <- list(
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

#############################
# Careful, will take some time
##############################

samplesp <- extract.samples(m, n = 500)

p <- link(m, data = mig_data, post = samplesp)
mABp <- round(colMeans(p$lambdaAB))
mBAp <- round(colMeans(p$lambdaBA))


predict <- c(mABp, mBAp)
migrants <- c(df$mAB, df$mBA)

######################
# Create new data frame
######################

nr_obs <- nr * (nr - 1)

fit_data <- data.frame(
  type = c( rep("Observed", nr_obs), rep("Predicted", nr_obs) ),
  Migrants = c(migrants, predict) 
)

fit_large <- filter(fit_data, Migrants >= 100)
fit_small <- filter(fit_data, Migrants < 100)
hist_fit_small <- ggplot(data = fit_small, aes(Migrants, fill = type)) + 
  geom_histogram( color = "#100F14" , bins = 10, position = 'dodge') +
  scale_fill_manual(values=c("#DCA258", "#80A0C7")) +
  labs(fill="") 
hist_fit_large <- ggplot(data = fit_large, aes(Migrants, fill = type)) + 
  geom_histogram(color = "#100F14", bins = 10, position = 'dodge') +
  scale_x_continuous(breaks=seq(100, 6100, 1000)) +
  scale_fill_manual(values=c("#DCA258", "#80A0C7")) +
  labs(fill="")
hist_fit <- plot_grid(hist_fit_small + theme(legend.position = "none"), 
                      hist_fit_large + theme(legend.position = "none"), 
                      labels = c("Small flows", "Large flows"), 
                      label_x = 0.4, label_y = 0.96) 

legend_b <- get_legend(hist_fit_small + theme(legend.position="bottom"))

hist_fit <- plot_grid( hist_fit, legend_b, ncol = 1, rel_heights = c(1,.1) )

pdf(file = "./fig/hist_fit.pdf" ,width=8,height=4) 
hist_fit
dev.off()

######################
# Create new data and difference the pmean predicted outcomes
######################

new_d <- d %>% 
  mutate(
    homeowners_o = ifelse(origin == 122, homeowners_o + 10, homeowners_o),
    homeowners_d = ifelse(destination == 122, homeowners_d + 10, homeowners_d),
    socialhousing_o = ifelse(origin == 122, socialhousing_o - 10, socialhousing_o),
    socialhousing_d = ifelse(destination == 122, socialhousing_d - 10, socialhousing_d),
  )

new_d$soc_d <- log(new_d$socialhousing_d + 0.001 )
new_d$soc_o <- log(new_d$socialhousing_o + 0.001 )
new_d$soc_d <- new_d$soc_d - mean(new_d$soc_d)
new_d$soc_o <- new_d$soc_o - mean(new_d$soc_o)
new_d$hom_o <- log(new_d$homeowners_o) - mean(log(new_d$homeowners_o))
new_d$hom_d <- log(new_d$homeowners_d) - mean(log(new_d$homeowners_d))

new_mig_data <- list(
  migrants  = d$Migrants,
  # N = nrow(d),
  N_regions = nr,
  origin = d$origin,
  destination = d$destination,
  log_distance = d$log_distance,
  pop_o = d$pop_o,
  pop_d = d$pop_d,
  hom_o = new_d$hom_o,
  hom_d = new_d$hom_d,
  soc_o = new_d$soc_o,
  soc_d = new_d$soc_d  
)

p_new <- link(m2, data = new_mig_data, post = samplesp)
predict_new <- round(colMeans(p_new) )
mean_diff <- predict_new - predict

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

municipalities <- st_read(dsn = "./data/src/2018/gemeente_2018_v2.shp")
st_crs(municipalities) = 28992

# Filter out water areas (WATER = "JA") and
# Belgium areas (Baarle Nassau; WATER = "B")

municipalities <- municipalities %>%
  filter(WATER == "NEE")

diff_in <- new_d %>%
  filter(destination == 122) %>%
  select(origin, mean_diff) %>%
  rbind(c("122", 0)) %>%
  arrange(as.numeric(origin)) %>%
  mutate(mean_diff = as.numeric(mean_diff))

sum(diff_in$mean_diff)

diff_out <- new_d %>%
  filter(origin == 122) %>%
  select(destination, mean_diff) %>%
  rbind(c("122", 0)) %>%
  arrange(as.numeric(destination)) %>%
  mutate(mean_diff = as.numeric(mean_diff))

sum(diff_out$mean_diff)

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


