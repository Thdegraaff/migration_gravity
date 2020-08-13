library("tidyverse")
library("sf")
library("RColorBrewer")

# Load Data files

load(file="./data/derived/migration_2018.Rda")
load(file="./output/m_srm.Rda")

# Define colour palette

myPal = colorRampPalette(brewer.pal(9,"PRGn"))(100)

# Load map

municipalities <- st_read(dsn = "./data/src/2018/gemeente_2018_v2.shp")
st_crs(municipalities) = 28992

# Filter out water areas (WATER = "JA") and
# Belgium areas (Baarle Nassau; WATER = "B")

municipalities <- municipalities %>%
  filter(WATER == "NEE")

# Prepare data for making maps

post <- extract.samples(m2 )

ori <- sapply( 1:nr , function(i) post$gr[,i,1] )
des <- sapply( 1:nr , function(i) post$gr[,i,2] )
ori <- apply( ori , 2 , mean )
des <- apply( des , 2 , mean )

municipalities$coef_in <- ori
municipalities$coef_out <- des

out_m <- data %>%
  group_by(code_o) %>%
  summarize(migrants_out = (sum(Migrants) )) %>%
  rename(GM_CODE = code_o)

in_m <- data %>%
  group_by(code_d) %>%
  summarize(migrants_in = (sum(Migrants) )) %>%
  rename(GM_CODE = code_d)

# Merge data

municipalities <- left_join(municipalities, out_m, by = c("GM_CODE" = "GM_CODE") ) 
municipalities <- left_join(municipalities, in_m, by = c("GM_CODE" = "GM_CODE") ) 

p_in <- ggplot() + geom_sf(data = municipalities, aes(fill = migrants_in)) + scale_fill_distiller(palette = "OrRd", direction = 1) + theme_bw()
p_out <- ggplot() + geom_sf(data = municipalities, aes(fill = migrants_out)) + scale_fill_distiller(palette = "OrRd", direction = 1) + theme_bw()

p_coef_in <- ggplot() + geom_sf(data = municipalities, aes(fill = coef_in), lwd = 0.4) + 
  scale_fill_distiller("Relative\n pull factor\n", palette = "RdBu", direction = -1, limits = c(-1, 1) ) + 
  theme_bw() 
p_coef_out <- ggplot() + geom_sf(data = municipalities, aes(fill = coef_out), lwd = 0.4) + 
  scale_fill_distiller("Relative\n push factor\n", palette = "RdBu", direction = -1, limits = c(-1, 1) ) + 
  theme_bw() 

p_homeown <- ggplot() + geom_sf(data = municipalities, aes(fill = P_KOOPWON)) + 
  scale_fill_distiller("Percentage \nhomeownership", palette = "Blues", direction = 1) + 
  theme_bw() 
p_socrent <- ggplot() + geom_sf(data = municipalities, aes(fill = P_HUURCORP)) + 
  scale_fill_distiller("Percentage \nsocial renting", palette = "Blues", direction = 1) + 
  theme_bw() 

pdf(file = "./fig/p_homeown.pdf" ,width = 9, height = 8) 
p_homeown
dev.off()

pdf(file = "./fig/p_socrent.pdf" ,width = 9, height = 8) 
p_socrent
dev.off()

pdf(file = "./fig/p_in.pdf" ,width = 9, height = 8) 
p_in
dev.off()

pdf(file = "./fig/p_out.pdf" ,width = 9, height = 8) 
p_out
dev.off()

pdf(file = "./fig/p_coef_in.pdf" ,width = 9, height = 8) 
p_coef_in 
dev.off()

pdf(file = "./fig/p_coef_out.pdf" ,width = 9, height = 8) 
p_coef_out
dev.off()
