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
    select(corop, ownership, socialrent, rent)
  
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
