######################
# Read in libraries
######################

library("tidyverse")
library("sf")
library("cowplot")

######################
# Read in csv files
######################

muni <- read.csv2(file = "./data/src/2018/Gebieden_in_Nederland_2018_10022020_174721.csv", header = TRUE)
muni <- muni[ ,1:2] # only keep first two variables
names(muni)[2] <- "code"

migration <- read.csv2(file = "./data/src/2018/Tussen_gemeenten_verhuisde_personen_10022020_172654.csv", 
                       header = TRUE)
migration <- drop_na(migration) # drop all municapalities with NA; now dataset is 380 * 380 - 380
i <- sapply(migration, is.factor)
migration[i] <- lapply(migration[i], as.character)

######################
# Merge databases 
######################

data <- rename(migration, 
               origin = Regio.van.vertrek, 
               destination = Regio.van.vestiging,
               Migrants= Tussen.gemeenten.verhuisde.personen..aantal.)
origin <- rename(muni, origin = Regio.s)
destination <- rename(muni, destination = Regio.s)

origin$origin <- as.character(origin$origin)
data <- left_join(data, origin) %>%
  rename(code_o = code)
data$code_o <- gsub(" ","",data$code_o)


destination$destination <- as.character(destination$destination)
data <- left_join(data, destination) %>%
  rename(code_d = code)
data$code_d <- gsub(" ","",data$code_d)

######################
# Read in Shapefile
# origin: https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/wijk-en-buurtkaart-2018
######################

municipalities <- st_read(dsn = "./data/src/2018/gemeente_2018_v2.shp")
st_crs(municipalities) = 28992

# Filter out water areas (WATER = "JA") and
# Belgium areas (Baarle Nassau; WATER = "B")

municipalities <- municipalities %>%
  filter(WATER == "NEE")

########################
# Make descriptive plots
########################


housing <- gather(municipalities, key = "Housing_type", value = "Percentage", c("P_KOOPWON", "P_HUURCORP") ) %>%
  select(GM_CODE, GM_NAAM, Housing_type, Percentage)

variable_names <- c(
  "P_HUURCORP" = "Social Housing" ,
  "P_KOOPWON" = "Homeownership"
)

hist_housing <- ggplot(data = housing, aes(x = Percentage)) + 
  geom_histogram(aes(y = ..density..) ,col = "black", fill= "forest green", alpha = 0.7, breaks=seq(0, 100, by=5), position = "identity") +
  facet_wrap(~ Housing_type, labeller = labeller(Housing_type= variable_names)) +
  theme_bw() + 
  labs(x = "Percentage (%)", y = "")

data_mig_large <- filter(data, Migrants >= 20)
data_mig_small <- filter(data, Migrants < 20)
hist_mig_small <- ggplot(data = data_mig_small, aes(Migrants)) + 
  geom_histogram(col = "black", fill = "forest green", alpha = 0.7, bins = 20) + theme_bw() 
hist_mig_large <- ggplot(data = data_mig_large, aes(Migrants)) + 
  geom_histogram(col = "black", fill = "forest green", alpha = 0.7, bins = 20) +
  scale_x_continuous(breaks=seq(20, 4020, 1000)) +
  theme_bw()
hist_mig <- plot_grid(hist_mig_small, hist_mig_large, labels = c("Small flows", "Large flows"), label_x = 0.5, label_y = 0.96) 

pdf(file = "./fig/hist_mig.pdf" ,width=8,height=4) 
hist_mig
dev.off()

pdf(file = "./fig/hist_housing.pdf" ,width=8,height=4) 
hist_housing 
dev.off()

######################
# Calculate distances
######################

muni_centroid <- st_centroid(municipalities)
distance <- st_distance(muni_centroid)
rownames(distance) <- muni_centroid$GM_CODE
colnames(distance) <- muni_centroid$GM_CODE
distance <- list(
  code_o = rownames(distance)[row(distance)] %||% row(distance),
  code_d = colnames(distance)[col(distance)] %||% col(distance),
  distance = distance
) %>% 
  map_dfc(as.vector)

######################
# Merge databases 
######################

data <- left_join(data, distance, by = c("code_o" = "code_o", "code_d" = "code_d") ) 

# Add variables
municipalities$GM_CODE <- as.character(municipalities$GM_CODE)
muni_o <- municipalities %>%
  select(GM_CODE, AANT_INW, WOZ, WONINGEN, P_KOOPWON, P_HUURCORP, P_HUUROVVH, GEM_HH_GR) %>%
  rename(pop_o = AANT_INW, 
         housevalue_o = WOZ,
         woningen_o = WONINGEN,
         homeowners_o = P_KOOPWON, 
         socialhousing_o = P_HUURCORP,
         private_rent_o = P_HUUROVVH,         
         hhgrootte_o = GEM_HH_GR) %>%
  st_drop_geometry

muni_d <- municipalities %>%
  select(GM_CODE, AANT_INW, WOZ, WONINGEN, P_KOOPWON, P_HUURCORP, P_HUUROVVH, GEM_HH_GR) %>%
  rename(pop_d = AANT_INW, 
         housevalue_d = WOZ,
         woningen_d = WONINGEN,
         homeowners_d = P_KOOPWON, 
         socialhousing_d = P_HUURCORP, 
         private_rent_d = P_HUUROVVH,
         hhgrootte_d = GEM_HH_GR) %>%
  st_drop_geometry

data <- left_join(data, muni_o, by = c("code_o" = "GM_CODE") ) 
data <- left_join(data, muni_d, by = c("code_d" = "GM_CODE") ) 

##########################
# Save resulting database
##########################

save(data,file="./data/derived/migration_2018.Rda")

rm(list = ls()) # Remove all objects
  