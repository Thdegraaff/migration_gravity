######################
# Read in libraries
######################

library("tidyverse")
library("sf")
library("cowplot")
library("spatialrisk")
library("rethinking")
library("dutchmasters")

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
# Read in Shapefile
# we use the package spatialrisk for this
######################

nr_corop <- 40

data("nl_corop")
regions <- nl_corop

# Calculate distances

region_cent <- st_centroid(regions)
distance <- st_distance(region_cent)
dmat <- matrix(distance/100000, 40, 40) # in hundred kilometer
rownames(distance) <- region_cent$corop_nr
colnames(distance) <- region_cent$corop_nr
distance <- list(
  origin = as.integer( rownames(distance)[row(distance)] %||% row(distance) ),
  destination = as.integer( colnames(distance)[col(distance)] %||% col(distance) ),
  distance = distance/100000
) %>% 
  map_dfc(as.vector)

######################
# Read in csv files
######################

verh <- read.csv2(file = "./data/src/COROP/verhuizingen_corop.csv", header = FALSE, skip = 1)
verh$V1<- as.character(verh$V1)
d <- separate(verh, V1,  c("id", "destination", "origin", "year", "number"))
d <- d %>%
  mutate(
    destination = as.integer(str_remove(destination, "CR") ),
    origin = as.integer(str_remove(origin, "CR") ),
    year = as.integer(str_remove(year, "JJ00") ),
    number = as.integer(number)
  ) %>%
  select( -id ) %>% # we do not need this variable
  filter(destination != origin, year != 2011, origin <= nr_corop, destination <= nr_corop) # not interested in within migration

d_in <- d %>% 
  filter(destination == 23) %>% 
  group_by(year) %>%
  summarise(total_in = sum(number))

d_out <- d %>% 
  filter(origin == 23) %>% 
  group_by(year) %>%
  summarise(total_out = sum(number))

mig_adam <- right_join(d_in, d_out, by = "year") %>%
  mutate(net_out = total_out - total_in)

d_for_plot <- d

bev <- read.csv2(file = "./data/src/COROP/bevolking.csv", sep = ";", header = FALSE, skip = 1)
bev$V1<- as.character(bev$V1)
d_bev <- separate(bev, V1,  c("id", "Geslacht", "Leeftijd", "BurgerlijkeStaat", "COROP", "year", "population"))
d_bev <- d_bev %>%
  mutate(
    corop = as.integer(str_remove(COROP, "CR") ),
    year = as.integer(str_remove(year, "JJ00") ),
    population = as.numeric(population)
  ) %>%
  select( corop, year, population ) %>%
  filter(year != 2011, year != 2019, corop <= nr_corop)

d_wonen <- read.csv(file = "./data/src/COROP/corop_woningen_totaal.csv", header = TRUE)
d_wonen$corop <- rep(1:40, each = 8)
d_wonen <- d_wonen %>%
  mutate(
    ownership = total_own/total_houses,
    socialrent = total_social_rent/total_houses,
    rent = total_other/total_houses
    ) %>% 
  select(corop, year,  ownership, socialrent, rent) %>%
  filter(corop <= nr_corop)

save(d_wonen, file="./data/derived/d_wonen.Rda")

######################
# pairs plot
######################

df_pairs <- d_wonen %>%
  left_join(d_bev, by = c("corop" = "corop", "year" = "year") ) %>%
  filter(year == 2015)
pairs(~ ownership + socialrent + rent + population, data = df_pairs, col = rangi2)

######################
# descriptives
######################

cor(df_pairs)

plot_adam <- ggplot(mig_adam, aes(x = year, y= net_out)) + 
  geom_bar(stat = "identity", fill = "#EEDA9D", color = "#DCA258") + 
  ylab("Net outmigration") + 
  xlab("Year") + 
  ggtitle("Net interregional outmigration of Amsterdam ")
plot_adam

######################
# Merge databases 
######################

d <- left_join(d, distance, by = c("origin" = "origin", "destination" = "destination") ) 
d <- left_join(d, d_bev, by = c("origin" = "corop", "year" = "year") ) 
d <- d %>%
  rename(popA = population)
d <- left_join(d, d_bev, by = c("destination" = "corop", "year" = "year") ) 
d <- d %>%
  rename(popB = population)
d <- left_join(d, d_wonen, by = c("origin" = "corop", "year" = "year") ) 
d <- d %>%
  rename(
    homA = ownership,
    socA = socialrent,
    rentA = rent
    )
d <- left_join(d, d_wonen, by = c("destination" = "corop", "year" = "year") ) 
d <- d %>%
  rename(
    homB = ownership,
    socB = socialrent, 
    rentB = rent
    )

######################
# Demean data
######################

d <- d %>%
  group_by(year) %>%
  mutate(
    ldist = log(distance) - mean( log(distance) ),
    lpopA = log(popA) - mean( log(popA) ),
    lpopB = log(popB) - mean( log(popB) ),
    lhomA = log(homA) - mean( log(homA) ),
    lhomB = log(homB) - mean( log(homB) ),
    lsocA = log(socA) - mean( log(socA) ),
    lsocB = log(socB) - mean( log(socB) ),
    lrentA = log(rentA) - mean( log(rentA) ),
    lrentB = log(rentB) - mean( log(rentB) )
  )  

######################
# tranform to dyad database
######################

origin <- integer(0)
destination <- integer(0)
for (i in 1:(nr_corop -1) ) {
  origin_new <- rep(i, nr_corop - i )
  destination_new <- seq(i+1, nr_corop)
  origin <- c(origin, origin_new)
  destination <- c(destination, destination_new)
}
df <- data.frame(origin, destination)

nr_d <- nrow(df)
df$did <- seq(1, nr_d, 1)
# we have 7 years
df <- rbind(df, df, df, df, df, df, df)
df$year <- rep(2012:2018, each = nr_d)

# start joining
df <- left_join(df, d, by = c("origin" = "origin", "destination" = "destination", "year" = "year") )
df <- df %>%
  rename(
    mAB = number,
    )
d_temp <- select(d, origin, destination, year, number)
df <- left_join(df, d_temp, by = c("origin" = "destination", "destination" = "origin", "year" = "year") )
df <- df %>%
  rename(
    mBA = number,
  )

######################
# First descriptive plot
######################

plot(df$mAB, df$mBA, 
     xlab = "Migrants from A to B", ylab = "Migrants from B to A",
     xlim = c(0,7000), ylim = c(0, 7000))
abline(a = 0, b = 1, lty = 2)

d_wonen %>%
  gather(key = "housing_type", value = "percentage", ownership, socialrent) %>%
  ggplot(aes(percentage, fill = housing_type) ) + geom_histogram(position="dodge")+ facet_wrap(d_wonen$year)

hist_panel <- d %>%
  ggplot(aes(number) ) + geom_histogram(position="dodge") +facet_wrap(d_temp$year)

########################
# Make descriptive plots
########################

housing <- d_wonen %>%
  filter(year == 2018) %>%
  gather(key = "housing_type", value = "percentage", ownership, socialrent) %>%
  mutate(
    percentage = 100 * percentage
  )

variable_names <- c(
  "socialrent" = "Social Housing" ,
  "ownership" = "Homeownership"
)

hist_housing <- ggplot(data = housing, aes(x = percentage)) + 
  geom_histogram(aes(y = ..density..) ,fill = "#EEDA9D", color = "#DCA258", breaks=seq(0, 100, by=5), position = "identity") +
  facet_wrap(~ housing_type, labeller = labeller(housing_type= variable_names)) +
  labs(x = "Percentage (%)", y = "")

d_for_plot <- d_for_plot %>%
  filter(year == 2018)
data_mig_large <- filter(d_for_plot, number >= 100)
data_mig_small <- filter(d_for_plot, number < 100)
hist_mig_small <- ggplot(data = data_mig_small, aes(number)) + 
  geom_histogram(fill = "#EEDA9D", color = "#DCA258", bins = 10) +
  xlab("Interregional migrants") + ylab("")
hist_mig_large <- ggplot(data = data_mig_large, aes(number)) + 
  geom_histogram(fill = "#EEDA9D", color = "#DCA258", bins = 10) +
  scale_x_continuous(breaks=seq(100, 7100, 1000)) +
  xlab("Interregional migrants") + ylab("")
hist_mig <- plot_grid(hist_mig_small, hist_mig_large, labels = c("Small flows", "Large flows"), label_x = 0.4, label_y = 0.96)
hist_mig

pdf(file = "./fig/hist_mig_corop.pdf" ,width=8,height=4) 
hist_mig
dev.off()

pdf(file = "./fig/hist_housing_corop.pdf" ,width=8,height=4) 
hist_housing 
dev.off()

##########################
# Save resulting database
##########################

save(d_bev, file="./data/derived/population.Rda")
save(df, file="./data/derived/migration_COROP.Rda")
save(dmat, file="./data/derived/dmat.Rda")

rm(list = ls()) # Remove all objects
