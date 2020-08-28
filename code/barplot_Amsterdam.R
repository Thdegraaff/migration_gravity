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
# Read data
######################

adam <- read.csv2(file = "./data/src/COROP/Amsterdam_migratie.csv", header = TRUE)

######################
# Mutate data
######################

df_adam <- adam %>%
  select(-Geslacht, -Regio.s) %>%
  rename(
    age  = Leeftijd..31.december.,
    year = Perioden,
    in_mig = Tussen.gemeenten.verhuisde.personen.Gevestigd.in.de.gemeente..aantal.,
    out_mig = Tussen.gemeenten.verhuisde.personen.Vertrokken.uit.de.gemeente..aantal.
  ) %>%
  mutate(
    net_out = out_mig - in_mig, 
  ) %>%
  select(-in_mig, -out_mig)

df <- df_adam %>%
  pivot_wider(names_from = age, values_from = net_out) %>%
  mutate(
    Total = Totaal, 
    `Below 15` = `0 tot 5 jaar` + `5 tot 10 jaar` + `10 tot 15 jaar`,
    `15 to 30` = `15 tot 20 jaar` + `20 tot 25 jaar` + `25 tot 30 jaar`,
    `30 to 45` = `30 tot 35 jaar` + `35 tot 40 jaar` + `40 tot 45 jaar`,
    `45 to 60` = `45 tot 50 jaar` + `50 tot 55 jaar` + `55 tot 60 jaar`, 
    `Above 60` = `60 tot 65 jaar` + `65 tot 70 jaar` + `70 tot 75 jaar` + 
      `75 tot 80 jaar` + `80 tot 85 jaar` + `85 tot 90 jaar` + 
      `90 tot 95 jaar` + `95 tot 100 jaar` + `100 jaar of ouder`, 
  ) %>%
  select(year, Total, `Below 15`,  `15 to 30`, `30 to 45`, `45 to 60`, `Above 60` ) %>%
  pivot_longer(cols = 2:7) %>%
  filter(year %in% (2011:2019)) %>%
  transform(name=factor(name,levels=c("Total","Below 15","15 to 30", "30 to 45", "45 to 60", "Above 60"))) %>%
  mutate(
    year = as.factor(year)
  ) 

######################
# Make plot
######################
  
plot_adam <- ggplot(df, aes(x = year, y= value)) + 
  geom_bar(stat = "identity", fill = "#EEDA9D", color = "#DCA258") + 
  ylab("Net outmigration") + 
  xlab("Year") + 
  ggtitle("Net interregional outmigration of Amsterdam (2011-2019)") + facet_wrap(~name)  +
  scale_x_discrete(labels=c("2011" = "", "2013" = "", "2014" = "", "2016" = "", "2017" = "", "2019" = ""))

pdf(file = "./fig/outmig_amsterdam.pdf" ,width=8,height=4) 
plot_adam
dev.off()

png(file = "./fig/outmig_amsterdam.png" ,width=8,height=4) 
plot_adam
dev.off()
