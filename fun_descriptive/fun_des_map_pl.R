####################################################################################################
# Purpose of script: Function to plot the map
####################################################################################################
library(maps)

fun_des_map_pl <- function(dat = dat,
                           country_grouping = "reg_8",
                           pathogen = "varicella",
                           data_name = "dl") {
  
  # Data for map of the globe ----------------------------------------------------------------------
  world <- map_data("world") 
  
  # Selected countries from the map of the globe ---------------------------------------------------
  
  ## Rename countries to match 
  dat <- dat %>%
    mutate(country = case_when(country == "United States" ~ "USA",
                               .default = country))
  
  # Selected countries 
  dat_pm <- dat %>%
    rename(country_g_sel = all_of(country_grouping)) %>%
    mutate(country_g_sel = as.character(country_g_sel)) %>%
    group_by(study, lat_med, lon_med, country_g_sel) %>%
    summarise(tot = n())
  
  selected_map <- map_data("world") %>%
    filter(region %in% c(unique(as.character(dat$country))))
  
  # If differences 
  setdiff(unique(selected_map$region),unique(as.character(dat$country)))
  setdiff(unique(as.character(dat$country)),unique(selected_map$region))
  
  # Plot it ----------------------------------------------------------------------------------------
  
  pl <- ggplot() +
    # Give it lines 
    geom_hline(aes(yintercept = 0), linewidth = 0.5, color = "grey85") + 
    geom_hline(aes(yintercept = 35), linewidth = 0.5, color = "grey85") + 
    geom_hline(aes(yintercept = -35), linewidth = 0.5, color = "grey85") + 
    geom_hline(aes(yintercept = 23.5), linewidth = 0.5, color = "grey85") + 
    geom_hline(aes(yintercept = -23.5), linewidth = 0.5, color = "grey85") + 
    geom_hline(aes(yintercept = 50), linewidth = 0.5, color = "grey85") +
    geom_hline(aes(yintercept = -50), linewidth = 0.5, color = "grey85") + 
    # Add map 
    geom_polygon(data = world, aes(x = long, y = lat, group = group), 
                 fill = "grey90") +
    # Add poligons of selected countries
    geom_polygon(data = selected_map, aes(x = long, y = lat, group = group),
                 fill = "grey75") + 
    # Add points of the countries 
    geom_point(data = dat_pm,
               aes(x = lon_med, y = lat_med, color = country_g_sel, size = tot)) + 
    scale_y_continuous("Latitude (°)", breaks = c(-50, -35, -23.5, 0, 23.5, 35, 50)) +
    scale_x_continuous("Longitude (°)") + 
    scale_color_discrete_divergingx(name = country_grouping, palette = "Geyser") +
    scale_size_continuous("Total individuals", limits = c(1,1500)) + 
    guides(size = guide_legend(title.position = "top", nrow = 2, byrow = T),
           color = guide_legend(title.position = "top", nrow = 2, byrow = T)) + 
    theme(legend.position = "bottom",
          strip.background = element_rect(fill = "grey", colour = "white"),
          strip.text = element_text(colour = "white"),
          panel.grid = element_blank()) + 
    coord_quickmap()

  # Save it ----------------------------------------------------------------------------------------
  
  ggsave(plot = pl, glue("{dirs$figures}/{pathogen}_des{data_name}_map.pdf"),
         width = 10, height = 6) 
 
  return(pl)
}

####################################################################################################
# End 
####################################################################################################