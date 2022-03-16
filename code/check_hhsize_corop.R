library(cbsodataR)
library(brms)
library(spatialrisk)
library(socviz)
library(classInt)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(grid)


theme_map <- function(base_size=9, base_family="") {
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

oppervlakte <- cbs_get_data('83704ned') %>%
    cbs_add_date_column() %>%
    cbs_add_label_columns() 

opp <- oppervlakte %>%
    filter(substr(RegioS, 1, 2) == "CR") %>%
    filter(Woningtype_label == "Totaal") %>%
    filter(RegioS != "CR99") %>%
    select(Oppervlakteklasse_label, RegioS, Perioden_label, BeginstandWoningvoorraad_1) %>%
    pivot_wider(names_from = Oppervlakteklasse_label, values_from = BeginstandWoningvoorraad_1)

opp[,4:12] <- opp[,4:12]/t(opp[,3])

colnames(opp)[4:12] <- c('p2tot15', 'p15tot50', 'p50tot75', 'p75tot100', 'p100tot150', 'p150tot250', 'p250tot500','p500tot10000', 'onbekend') 

regional_data <- cbs_get_data('70072ned') %>%
  cbs_add_date_column() %>%
  cbs_add_label_columns() 

reg_dat <- regional_data %>%
  filter(substr(RegioS, 1, 2) == "CR") %>%
  mutate(Perioden_label = as.numeric(levels(Perioden_label))[Perioden_label]) %>%
  filter(Perioden_label >= 2012 ) %>%
  select(RegioS, RegioS_label, Perioden_label, GemiddeldeHuishoudensgrootte_89, 
         JongerDan5Jaar_13, k_5Tot10Jaar_14, k_10Tot15Jaar_15, k_15Tot20Jaar_16,
         k_20Tot25Jaar_17, k_25Tot45Jaar_18, k_45Tot65Jaar_19, k_65Tot80Jaar_20, 
         k_80JaarOfOuder_21, WesterseMigratieachtergrond_45, Marokko_47, 
         VoormaligeNederlandseAntillenAruba_48, Suriname_49, Turkije_50, 
         OverigNietWesterseMigratieachtergrond_51) %>%
  mutate(Perioden_label = as.factor(Perioden_label)) %>%
  rename(
    hhsize  = GemiddeldeHuishoudensgrootte_89, 
    j05 = JongerDan5Jaar_13,
    t0510 = k_5Tot10Jaar_14,
    t1015 = k_10Tot15Jaar_15,
    t1520 = k_15Tot20Jaar_16,
    t2025 = k_20Tot25Jaar_17,
    t2545 = k_25Tot45Jaar_18,
    t4565 = k_45Tot65Jaar_19,
    t6580 = k_65Tot80Jaar_20,
    o80 = k_80JaarOfOuder_21,
    westers = WesterseMigratieachtergrond_45, 
    marokko = Marokko_47, 
    antillen = VoormaligeNederlandseAntillenAruba_48, 
    suriname = Suriname_49, 
    turkije = Turkije_50,
    overig_nw = OverigNietWesterseMigratieachtergrond_51
  )

reg_dat <- left_join(reg_dat, opp)

n_distinct(reg_dat$RegioS, na.rm = FALSE)

m_0 <- brm(hhsize ~ 1, data = reg_dat, 
           cores = 3, chains = 3)
m_1 <- brm(hhsize ~ 1 + (1 | RegioS) + (1 | Perioden_label), data = reg_dat, 
           cores = 3, chains = 3, warmup = 2000, iter = 5000)
m_2 <- brm(hhsize ~ 0 + j05 + t0510 + t1015 + t1520 + t2025 + t2545 + t4565 +
                    t6580 + o80 + 
                    (1 | RegioS) + (1 | Perioden_label), data = reg_dat, 
                    cores = 3, chains = 3, warmup = 2000, iter = 5000)
m_3 <- brm(hhsize ~ 0 + j05 + t0510 + t1015 + t1520 + t2025 + t2545 + t4565 +
             t6580 + o80 + 
             westers + marokko + antillen + suriname + turkije + overig_nw +
             (1 | RegioS) + (1 | Perioden_label), data = reg_dat, 
           cores = 3, chains = 3, warmup = 2000, iter = 5000)
m_4 <- brm(hhsize ~ 0 + j05 + t0510 + t1015 + t1520 + t2025 + t2545 + t4565 +
               t6580 + o80 + 
               westers + marokko + antillen + suriname + turkije + overig_nw +
               p2tot15 + p15tot50 + p50tot75 + p75tot100 + p100tot150 + 
               p150tot250 + p250tot500 + p500tot10000 +
               (1 | RegioS) + (1 | Perioden_label), data = reg_dat, 
           cores = 3, chains = 3, warmup = 2000, iter = 5000)

summary(m_0)
summary(m_1)
summary(m_2)
summary(m_3)
summary(m_4)

r1 <- ranef(m_1)$RegioS[, , ] %>% data.frame()
r2 <- ranef(m_2)$RegioS[, , ] %>% data.frame()
r3 <- ranef(m_3)$RegioS[, , ] %>% data.frame()
r4 <- ranef(m_4)$RegioS[, , ] %>% data.frame()

data("nl_corop")
regions <- nl_corop

label_name <-  rep(NA, 40)
label_name[17] <-  "Utrecht"
label_name[23] <- "Amsterdam"
label_name[26] <- "The Hague"
label_name[29] <- "Rotterdam"


names <- rep(c("household size","controlled for age","and controlled for migration background", "and controlled for house size"),each = 40)
dat_corop <- tibble(corop_nr = rep(seq(1,40),4), model = names, waarde = rbind(r1,r2,r3,r4)[,1], label = rep(label_name,4) )
dat_corop$model_f <- factor(dat_corop$model, levels = c("household size","controlled for age",
                                                        "and controlled for migration background", "and controlled for house size"))
regions <- left_join(regions,dat_corop)

breaks_fixed <- classIntervals(regions$waarde, n=6, style="fixed",
                               fixedBreaks=c(-0.3, -0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.2, 0.3))
breaks_fixed
regions <- mutate(regions, waarde_cat = cut(waarde, breaks_fixed$brks, dig.lab=7))

p0 <- ggplot() + geom_sf(data = regions, aes(fill = waarde_cat), size = 0.01) + 
  facet_wrap(.~model_f, ncol = 4) +
  geom_sf_text(data = regions, aes(label = label), size = 2, colour = "black" )

p1 <- p0 + scale_fill_brewer(palette="RdBu") + 
  labs(title = "Regional differences in household size, 2012-2021",
       fill = "Regional difference household size") + theme_map()  

p2 <- p1 + theme(legend.position = "bottom",
           strip.background = element_blank())                     

ggsave(p2, filename = "./fig/hhsize_corop.pdf", width = 8, height = 4)
p2    
