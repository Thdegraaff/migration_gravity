  library("tidyverse")
  library("sf")
  library("RColorBrewer")
  library("rethinking")
  library("spatialrisk")
  library("grid")
  library("gridExtra")
  library(socviz)
  library(classInt)
  # library("dutchmasters")
  
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
  
  ######################
  # Set Dutch masters theme
  ######################
  
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
  
  
  # Load Data files
  
  load(file="./output/corop_final_model_hhsize.Rda")
  load(file="./data/derived/d_wonen.Rda")
  d_wonen <- d_wonen %>%
    filter(year == 2018) %>%
    select(corop, ownership, socialrent, rent, total_houses)
  
  # Define colour palette
  
  myPal = colorRampPalette(brewer.pal(9,"PRGn"))(100)
  
  # Load map
  
  nr_corop <- 40
  data("nl_corop")
  regions <- nl_corop
  
  post <- extract.samples( m )
  
  ori <- sapply( 1:nr_corop , function(i) post$gr[,i,1] )
  des <- sapply( 1:nr_corop , function(i) post$gr[,i,2] )
  ori <- apply( ori , 2 , mean )
  des <- apply( des , 2 , mean )
  
  # Merge data
  
  # regions <- left_join(regions, d_wonen, by = c("corop_nr" = "corop") ) 
  
  label_name <-  rep(NA, 40)
  label_name[17] <-  "Utrecht"
  label_name[23] <- "Amsterdam"
  label_name[26] <- "The Hague"
  label_name[29] <- "Rotterdam"
  
  names <- rep(c("Orgin (push) effect","Destination (pull) effect"),each = 40)
  dat_regional_effect <- tibble(corop_nr = rep(seq(1,40),2), model = names, waarde = c(ori, des), label = rep(label_name,2) )
  dat_regional_effect$model_f <- factor(dat_regional_effect$model, levels = c("Orgin (push) effect","Destination (pull) effect"))

  regions_varying <- left_join(regions, dat_regional_effect)
  
  breaks_fixed <- classIntervals(regions_varying$waarde, n=8, style="fixed",
                                 fixedBreaks=c(-1.5, -1, -0.5, -0.25, 0, 0.25, 0.5, 1, 1.5))
  breaks_fixed
  regions_varying <- mutate(regions_varying, waarde_cat = cut(waarde, breaks_fixed$brks, dig.lab=7))
  
  p0 <- ggplot() + geom_sf(data = regions_varying, aes(fill = waarde_cat), size = 0.01) + 
    facet_wrap(.~model_f, ncol = 2) +
    geom_sf_text(data = regions_varying, aes(label = label), size = 2, colour = "black" )
  
  p1 <- p0 + scale_fill_brewer(palette="RdBu") + 
    labs(title = "Regional push and pull effects",
         fill = "Regional effect") + theme_map()  
  
  p2 <- p1 + theme(legend.position = "bottom",
                   strip.background = element_blank())                     
  
  ggsave(p2, filename = "./fig/regional_effects.pdf", width = 6, height = 4)
  p2    
  
  names <- rep(c("Social renting","Home-ownership"),each = 40)
  dat_housing <- tibble(corop_nr = rep(seq(1,40),2), model = names, waarde = c(d_wonen$socialrent, d_wonen$ownership), label = rep(label_name,2) )
  dat_housing$model_f <- factor(dat_housing$model, levels = c("Social renting","Home-ownership"))
  
  regions <- nl_corop
  regions_housing <- left_join(regions, dat_housing)
  
  breaks_fixed <- classIntervals(regions_housing$waarde, n=6, style="fixed",
                                 fixedBreaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7))
  breaks_fixed
  regions_housing <- mutate(regions_housing, waarde_cat = cut(waarde, breaks_fixed$brks, dig.lab=7))
  
  p0 <- ggplot() + geom_sf(data = regions_housing, aes(fill = waarde_cat), size = 0.01) + 
    facet_wrap(.~model_f, ncol = 2) +
    geom_sf_text(data = regions_housing, aes(label = label), size = 2, colour = "black" )
  
  p1 <- p0 + scale_fill_brewer(palette="Blues") + 
    labs(title = "Dutch housing structure (2018)",
         fill = "Fraction") + theme_map()  
  
  p2 <- p1 + theme(legend.position = "bottom",
                   strip.background = element_blank())                     
  
  ggsave(p2, filename = "./fig/housing_structure.pdf", width = 6, height = 4)
  p2   
  
  ################ Check scatterplots for panel regional varying effects
  
  bev <- read.csv2(file = "./data/src/COROP/bevolking_2011_2020.csv", sep = ";", header = FALSE, skip = 1)
  bevolking <- separate(bev, V1,  c("Geslacht", "Leeftijd", "BurgerlijkeStaat", "year", "corop", "population"), sep = ";")
  bevolking <- bevolking %>%
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
    filter(year != 2011 & year != 2020) %>%
    group_by(corop) %>%
    summarize(
      population = mean(population),
      divorced = mean(divorced), 
      married = mean(married)
    )    
  
  ########################## Read in value (woz) data ######################
  
  value <- read.csv2(file = "./data/src/COROP/Waarde_onroerende_zaken.csv", sep = ";", header = FALSE, skip = 1)
  value <- separate(value, V1,  c("year", "corop", "value"), sep = ";")
  value <- value %>%
    select(-corop) %>%
    mutate(
      corop =  rep(1:40, 1, each = 10),
      value = as.integer(value)
    )   %>%
    group_by(corop) %>%
    mutate(
      prev = lag(value, n = 9), 
      growth = (value - prev)/prev
    ) %>%
    filter(year == "2020*") %>%
    select(corop, growth)
  
  load(file="./data/derived/d_wonen.Rda")
  housing_growth <- d_wonen %>%
    mutate(
      prev = lag(total_houses, n = 8), 
      growth_houses = (total_houses - prev)/prev
    ) %>%
    filter(year == 2020) %>%
    select(corop, growth_houses) 
  
  ########################## Read in nationality data ######################
    
  nat <- read.csv2(file = "./data/src/COROP/nationaliteit_2012_2020.csv", sep = ";", header = FALSE, skip = 1)
  nat <- separate(nat, V1,  c("total_gender", "total_age", "nationality", "year", "corop", "population"), sep = ";")
  nat <- nat %>%
    select(-total_gender, -total_age ) %>%
    mutate(
      year = rep(2012:2020, 160),
      corop = rep(1:40, 4, each = 9),
      population = as.numeric(population)
    ) %>%
    pivot_wider(names_from = nationality, values_from = population) %>%
    rename(
      total_pop = '"Totaal"',
      total_nd = '"Totaal niet-Nederlandse nationaliteit"',
      total_w = '"Westerse nationaliteiten ex. Nederlands"',
      total_nw = '"Niet-westerse nationaliteiten"'
    ) %>%
    mutate(
      growth_w = (total_w - lag(total_w, n = 8) )/total_w,
      growth_nd = (total_nd - lag(total_nd, n = 8) )/total_nd,
      growth_pop = (total_pop - lag(total_pop, n = 8) )/total_pop,
      perc_nd = total_nd/total_pop,
      perc_w = total_w/total_pop
    ) %>%
    filter(year == 2020)
  
  ########################## Join stuff
  
  regions <- left_join(regions, bevolking, by = c("corop_nr" = "corop") ) 
  regions <- left_join(regions, value, by = c("corop_nr" = "corop") ) 
  regions <- left_join(regions, housing_growth, by = c("corop_nr" = "corop") )
  regions <- left_join(regions, nat, by = c("corop_nr" = "corop") )
  
  data_plot <- regions %>%
    mutate(
      hhsize = population/total_houses,
      divorce_rate = divorced/population,
      population = population
    ) %>%
    select(areaname, corop_nr, coef_out, coef_in, hhsize, divorce_rate, population, growth, growth_houses, growth_w, growth_nd, total_w, total_nd, total_nw, growth_pop, 
           perc_nd, perc_w) 
  
  label_name <-  rep(NA, 40)
  label_name[17] <-  "Utrecht"
  label_name[23] <- "Amsterdam"
  label_name[26] <- "The Hague"
  label_name[29] <- "Rotterdam"
  data_plot <- cbind(data_plot, label_name)
  
  out_plot_1 <- ggplot(data = data_plot, aes(coef_out, divorce_rate)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Average divorce rate") + 
    xlab("Regional push factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  out_plot_1
  
  out_plot_2 <- ggplot(data = data_plot, aes(coef_out, hhsize)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Average household size") + 
    xlab("Regional push factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  out_plot_2
  
  out_plot_3 <- ggplot(data = data_plot, aes(coef_out, population)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Average population size") + 
    xlab("Regional push factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  out_plot_3
  
  out_plot_4 <- ggplot(data = data_plot, aes(coef_out, perc_nd)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Percentage non-dutch residents") + 
    xlab("Regional push factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  out_plot_4
  
  out_plot_5 <- ggplot(data = data_plot, aes(coef_out, total_nd)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Total non-dutch residents 2020") + 
    xlab("Regional push factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  out_plot_5
  
  out_plot_6 <- ggplot(data = data_plot, aes(coef_out, perc_w)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("percentage western immigrants") + 
    xlab("Regional push factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  out_plot_6
  
  g <- arrangeGrob(out_plot_1, out_plot_2, out_plot_3, out_plot_4, out_plot_5, out_plot_6, nrow = 2)
  ggsave(g, file="./fig/regional_out_plot.pdf", width  = 300, height = 160, units = "mm")
  
  in_plot_1 <- ggplot(data = data_plot, aes(coef_in, divorce_rate)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Average divorce rate") + 
    xlab("Regional pull factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  in_plot_1
  
  in_plot_2 <- ggplot(data = data_plot, aes(coef_in, hhsize)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Average household size") + 
    xlab("Regional pull factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  in_plot_2
  
  in_plot_3 <- ggplot(data = data_plot, aes(coef_in, population)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Average population size") + 
    xlab("Regional pull factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  in_plot_3
  
  in_plot_4 <- ggplot(data = data_plot, aes(coef_in, growth_houses)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Regional housing stock growth 2012-2020") + 
    xlab("Regional pull factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  in_plot_4
  
  in_plot_5 <- ggplot(data = data_plot, aes(coef_in, total_nd)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Total non-dutch residents 2020") + 
    xlab("Regional pull factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  in_plot_5
  
  in_plot_6 <- ggplot(data = data_plot, aes(coef_in, growth_nd)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Growth non-dutch residents 2012-2020") + 
    xlab("Regional pull factor") + 
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  in_plot_6
  
  g <- arrangeGrob(in_plot_1, in_plot_2, in_plot_3, in_plot_4, in_plot_5, in_plot_6, nrow = 2)
  ggsave(g, file="./fig/regional_in_plot.pdf", width  = 300, height = 160, units = "mm")
  
  #### map growth population
  
  breaks_fixed <- classIntervals(data_plot$growth_pop, n=6, style="fixed",
                                 fixedBreaks=c(-0.2, -0.1, -0.05, 0, 0.05, 0.1, 0.3))
  breaks_fixed
  data_plot <- mutate(data_plot, waarde_pop = cut(growth_pop, breaks_fixed$brks, dig.lab=7))
  
  growth_pop <- ggplot() + geom_sf(data = data_plot, aes(fill = waarde_pop), size = 0.01) + 
    scale_fill_brewer(palette = "RdBu", direction = 1) +
    theme_map() +
    geom_sf_text(data = regions, aes(label = label_name), size = 4, colour = "black" ) +
    xlab("") + ylab("") +
    theme(legend.position = "bottom",
          strip.background = element_blank()) +
    labs(title = "Growth population, 2012-2020",
         fill = "growth rates")
  growth_pop
  
  ggsave(growth_pop, file="./fig/growth_pop.pdf", width  = 4, height = 6)
  
  #### map growth housing value
  
  breaks_fixed <- classIntervals(data_plot$growth, n=6, style="fixed",
                                 fixedBreaks=c(-0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6))
  breaks_fixed
  data_plot <- mutate(data_plot, waarde_cat = cut(growth, breaks_fixed$brks, dig.lab=7))
  
  growth_woz  <- ggplot() + geom_sf(data = data_plot, aes(fill = waarde_cat), size = 0.01) + 
    scale_fill_brewer(palette="Blues")  +
    theme_map() +
    geom_sf_text(data = regions, aes(label = label_name), size = 4, colour = "black" ) +
    xlab("") + ylab("") +
    theme(legend.position = "bottom",
          strip.background = element_blank()) +
    labs(title = "Growth property tax value, 2012-2020",
         fill = "growth rates")
  growth_woz
  
  ggsave(growth_woz, file="./fig/growth_woz.pdf", width  = 4, height = 6)
