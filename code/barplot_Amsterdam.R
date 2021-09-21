######################
# Read in libraries
######################

library("tidyverse")
library("dutchmasters")

######################
# Set Dutch masters theme
######################
# 
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
# Read data
######################

adam <- read.csv2(file = "./data/src/COROP/Amsterdam_migratie_2011_2020.csv", header = FALSE, skip = 1, sep = ";")
adam <- separate(adam, V1,  c("year", "region", "totaal_g", "0_5_g", "5_10_g",  "10_15_g",  "15_20_g",  "20_25_g",  "25_30_g",
                              "30_40_g", "40_50_g",  "50_65_g",  "65_85_g",  "85_g", 
                              "totaal_v", "0_5_v", "5_10_v",  "10_15_v",  "15_20_v",  "20_25_v",  "25_30_v",
                              "30_40_v", "40_50_v",  "50_65_v",  "65_85_v",  "85_v"), sep = ";")

######################
# Mutate data
######################

df_adam <- adam %>%
  mutate(
    Total = as.numeric(totaal_g) - as.numeric(totaal_v),
    `Below 15` = as.numeric(`0_5_g`) + as.numeric(`5_10_g`) + as.numeric(`10_15_g`) - (as.numeric(`0_5_v`) + as.numeric(`5_10_v`) + as.numeric(`10_15_v`) ),
    `15 to 30` = as.numeric(`15_20_g`) + as.numeric(`20_25_g`) + as.numeric(`25_30_g`) - (as.numeric(`15_20_v`) + as.numeric(`20_25_v`) + as.numeric(`25_30_v`) ),
    `30 to 50` = as.numeric(`30_40_g`) + as.numeric(`40_50_g`) - (as.numeric(`30_40_v`) + as.numeric(`40_50_v`) ),
    `50 to 65` = as.numeric(`50_65_g`) - (as.numeric(`50_65_v`) ),    
    `Above 65` = as.numeric(`65_85_g`) + as.numeric(`85_g`) - (as.numeric(`65_85_v`) + as.numeric(`85_v`) )
  ) %>%
  select(year, Total, `Below 15`,  `15 to 30`, `30 to 50`, `50 to 65`, `Above 65` ) %>%
  pivot_longer(cols = 2:7) %>%
  transform(name=factor(name,levels=c("Total","Below 15","15 to 30", "30 to 50", "50 to 65", "Above 65"))) %>%
  mutate(
    year = as.factor(year)
  ) 

######################
# Make plot
######################
  
plot_adam <- ggplot(df_adam, aes(x = year, y= value)) + 
  geom_bar(stat = "identity", color = "white", fill = "cornflowerblue") + 
  ylab("Net inmigration") + 
  xlab("Year") + 
  ggtitle("Net domestic regional migration to Amsterdam (2011-2020)") + facet_wrap(~name, nrow = 1)  +
  scale_x_discrete(labels=c("2011" = "", "2013" = "", "2014" = "", "2015" = "", "2016" = "", "2017" = "", "2019" = "", "2020" = "")) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

pdf(file = "./fig/outmig_amsterdam.pdf" ,width=8,height=4) 
plot_adam
dev.off()

png(file = "./fig/outmig_amsterdam.png") 
plot_adam
dev.off()
