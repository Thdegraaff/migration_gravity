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
  map_dfc(as.vector) %>%
  mutate(distance = replace(distance, distance == 0, 0.01))

######################
# Read in csv files
######################

verh <- read.csv2(file = "./data/src/COROP/verhuizingen_corop.csv", header = FALSE, skip = 1)
verh <- read.csv2(file = "./data/src/COROP/verhuizingen_corop_2011_2020.csv", header = FALSE, skip = 1, sep = ";")
verh$V1<- as.character(verh$V1)
d <- separate(verh, V1,  c("year", "destination", "origin", "number"), sep = ";")
d <- d %>%
  select(-destination, -origin) %>%
  mutate(
    destination = rep(1:nr_corop, each = 10 * nr_corop),
    origin = rep(1:nr_corop, nr_corop, each = 10),
    year = as.integer(year),
    number = as.integer(number)
  ) %>%
  filter(year != 2011, origin != destination)

d_for_plot <- d

bev <- read.csv2(file = "./data/src/COROP/bevolking_2011_2020.csv", sep = ";", header = FALSE, skip = 1)
d_bev <- separate(bev, V1,  c("Geslacht", "Leeftijd", "BurgerlijkeStaat", "year", "corop", "population"), sep = ";")
d_bev <- d_bev %>%
  select(population, BurgerlijkeStaat ) %>%
  mutate(
    year = rep(2011:2020, 120),
    corop = rep(1:40, 3, each = 10),
    population = as.numeric(population)
  ) %>%
  pivot_wider(names_from = BurgerlijkeStaat, values_from = population) %>%
  rename(
    population = '"Totaal burgerlijke staat"',
    married = '"Gehuwd"',
    divorced = '"Gescheiden"'
  ) %>%
  filter(year != 2011)

d_wonen <- read.csv2(file = "./data/src/COROP/corop_woningen_2012_2020.csv", header = FALSE, skip = 1)
d_wonen <- separate(d_wonen, V1,  c("totaal", "corop", "year", "total_houses", "total_own","total_rent", "total_social_rent", "total_other", "unknown"), sep = ";")
d_wonen <- d_wonen %>%
  filter(totaal == "Totaal") %>%
  select(-corop, -year, -totaal) %>%
  mutate(
    year = rep(2012:2020, 40),
    corop = rep(1:40, each = 9),
    total_houses = as.numeric(total_houses),
    total_own = as.numeric(total_own),
    total_rent = as.numeric(total_rent),
    total_social_rent = as.numeric(total_social_rent),
    total_other = as.numeric(total_other),
    ownership = total_own/total_houses,
    socialrent = total_social_rent/total_houses,
    rent = total_other/total_houses    
  )  %>% 
  group_by(corop) %>%
  mutate(
    d_own = lead(total_own) - total_own,
    d_social_rent = lead(total_social_rent) - total_social_rent,
    d_private_rent = lead(total_other) - total_other
  ) %>%
  select(corop, year,  ownership, socialrent, rent, total_houses, total_own, total_social_rent, d_own, d_social_rent, d_private_rent)

save(d_wonen, file="./data/derived/d_wonen.Rda")

######################
# pairs plot
######################

df_pairs <- d_wonen %>%
  left_join(d_bev, by = c("corop" = "corop", "year" = "year") ) %>%
  filter(year == 2015)
pairs(~ d_social_rent + d_own + population, data = df_pairs, col = rangi2)

######################
# descriptives
######################

cor(df_pairs)

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
  mutate(
    hhsizeA = popA/total_houses 
  ) %>%
  rename(
    dhomA = d_own,
    dsocA = d_social_rent,
    dpriA = d_private_rent,
    homA = ownership,
    socA = socialrent,     
    rentA = rent,
    marriedA = married.x,
    divorcedA = divorced.x,
    marriedB = married.y,
    divorcedB = divorced.y,
    ) %>%
  select(-total_houses)
d <- left_join(d, d_wonen, by = c("destination" = "corop", "year" = "year") ) 
d <- d %>%
  mutate(
    hhsizeB = popB/total_houses 
  ) %>%
  rename(
    dhomB = d_own,
    dsocB = d_social_rent,
    dpriB = d_private_rent,
    homB = ownership,
    socB = socialrent, 
    rentB = rent
    )%>%
  select(-total_houses)

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
    dhomA = (dhomA - mean(dhomA, na.rm = TRUE) )/sd(dhomA, na.rm = TRUE),
    dhomB = (dhomB - mean(dhomB, na.rm = TRUE) )/sd(dhomB, na.rm = TRUE),
    dsocA = (dsocA - mean(dsocA, na.rm = TRUE) )/sd(dsocA, na.rm = TRUE),
    dsocB = (dsocB - mean(dsocB, na.rm = TRUE) )/sd(dsocB, na.rm = TRUE),    
    dpriA = (dpriA - mean(dpriA, na.rm = TRUE) )/sd(dpriA, na.rm = TRUE),
    dpriB = (dpriB - mean(dpriB, na.rm = TRUE) )/sd(dpriB, na.rm = TRUE),     
    lrentA = log(rentA) - mean( log(rentA) ),
    lrentB = log(rentB) - mean( log(rentB) ),
    lhhsizeA = log(hhsizeA) - mean( log(hhsizeA) ),
    lhhsizeB = log(hhsizeB) - mean( log(hhsizeB) )    
  )  

######################
# transform to dyad database
######################

origin <- integer(0)
destination <- integer(0)
for (i in 1:(nr_corop - 1) ) {
  origin_new <- rep(i, nr_corop - i)
  destination_new <- seq(i + 1, nr_corop)
  origin <- c(origin, origin_new)
  destination <- c(destination, destination_new)
}
df <- data.frame(origin, destination)

nr_d <- nrow(df)
df$did <- seq(1, nr_d, 1)
# we have 9 years
df <- rbind(df, df, df, df, df, df, df, df, df)
df$year <- rep(2012:2020, each = nr_d)

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
