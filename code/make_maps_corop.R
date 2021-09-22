  library("tidyverse")
  library("sf")
  library("RColorBrewer")
  library("rethinking")
  library("spatialrisk")
  library("grid")
  library("gridExtra")
  # library("dutchmasters")
  
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
  
  load(file="./output/corop_dyad.Rda")
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
  
  post <- extract.samples( m_dyad )
  
  ori <- sapply( 1:nr_corop , function(i) post$gr[,i,1] )
  des <- sapply( 1:nr_corop , function(i) post$gr[,i,2] )
  ori <- apply( ori , 2 , mean )
  des <- apply( des , 2 , mean )
  
  regions$coef_out <- ori
  regions$coef_in <- des
  
  # Merge data
  
  regions <- left_join(regions, d_wonen, by = c("corop_nr" = "corop") ) 
  
  p_coef_in <- ggplot() + geom_sf(data = regions, aes(fill = coef_in), lwd = 0.4) + 
    scale_fill_distiller("Relative\n pull factor\n", palette = "RdBu", direction = -1, limits = c(-2, 2) ) +
    ggtitle("Regional destination effect") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  p_coef_out <- ggplot() + geom_sf(data = regions, aes(fill = coef_out), lwd = 0.4) + 
    scale_fill_distiller("Relative\n push factor\n", palette = "RdBu", direction = -1, limits = c(-2, 2) ) + 
    ggtitle("Regional origin effect") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  p_homeown <- ggplot() + geom_sf(data = regions, aes(fill = ownership)) + 
    scale_fill_distiller("Percentage \nhomeownership", palette = "Reds", direction = 1)  +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  p_socrent <- ggplot() + geom_sf(data = regions, aes(fill = socialrent)) + 
    scale_fill_distiller("Percentage \nsocial renting", palette = "Reds", direction = 1) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  housing <- arrangeGrob(p_homeown, p_socrent, nrow = 1)
  ggsave(housing, file="./fig/housing_types.pdf", width  = 200, height = 80, units = "mm")

  attractivity <- arrangeGrob(p_coef_in, p_coef_out, nrow = 1)
  ggsave(attractivity, file="./fig/attractivity_region.pdf", width  = 200, height = 80, units = "mm")
  
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
      growth_pop = (total_pop - lag(total_pop, n = 8) )/total_pop 
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
    select(corop_nr, coef_out, coef_in, hhsize, divorce_rate, population, growth, growth_houses, growth_w, growth_nd, total_w, total_nd, total_nw, growth_pop) 
  
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
  
  out_plot_4 <- ggplot(data = data_plot, aes(coef_out, growth_houses)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Regional housing stock growth 2012-2020") + 
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
  
  out_plot_6 <- ggplot(data = data_plot, aes(coef_out, growth_nd)) + 
    geom_point(color = "cornflowerblue", alpha = 1, size = 2) +
    geom_smooth(method='lm') +
    ylab("Growth non-dutch residents 2012-2020") + 
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
