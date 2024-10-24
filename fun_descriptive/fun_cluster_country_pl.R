####################################################################################################
# Purpose of script: Function to plot the selected cluster per country
####################################################################################################

fun_cluster_country_pl <- function(cluster_type_name = "kgc",
                                   pathogen = "varicella",
                                   dat = dat_l$d1_ii,
                                   dat_sens = NULL,
                                   file_name = "d1_ii") {  
  
  clusters_l <- readRDS(glue(
    "../00_data/processed/stu_climate_cluster_country/stu_cluster_country_list_{pathogen}.rds"))
  clusters_all <- clusters_l$clusters_all
  clusters_cou <- clusters_l$clusters_cou
  
  fun_pl <- function(cluster_var_name = "kgc_1") {
    
    if(str_detect(cluster_var_name, "reg") == FALSE) ncol_leg <- 3
    if(str_detect(cluster_var_name, "reg") == TRUE) ncol_leg <- 2
    
    if(length(dat_sens) > 0) {
      stu_sens <- dat_sens %>%
        mutate(stu_cou = glue("{study}_{country}")) %>%
        distinct(stu_cou)
      
      clusters_all <- clusters_all %>%
        mutate(stu_cou = glue("{study}_{country}"),
               cluvar = case_when(
                 stu_cou %in% stu_sens$stu_cou ~ cluvar,
                 .default = NA))
    }
    
    # Prepare data for plot
    dat_pl <- clusters_all  %>%
      filter(cluvar_name == cluster_var_name) %>%
      left_join(dat %>%
                  distinct(study, country, lon_med, lat_med)) %>%
      left_join(filter(clusters_cou, cluvar_name == cluster_var_name) %>%
                  rename(cluvar_country = cluvar) %>%
                  select(study, country, cluvar_country)) %>%
      mutate(study_country = glue("{study}_{country}")) %>%
      filter(study %in% unique(dat$study))
    
    # Plot it 
    pl <- dat_pl %>%
      ggplot(aes(x = cluvarp, 
                 y = reorder(study_country, lat_med), 
                 fill = reorder(cluvar, -cluvarp))) + 
      geom_bar(stat = "identity", position="fill") + 
      scale_fill_viridis_d(cluster_var_name, 
                           guide = guide_legend(title.position = "top", ncol = ncol_leg)) +
      scale_x_continuous("Proportion of centers") + 
      scale_y_discrete("") + 
      facet_grid(cluvar_country~., scales = "free", space = "free") + 
      theme(strip.text.y = element_text(angle = -90),
            strip.background = element_blank(),
            legend.position = "top") + dat_pl %>%
      ggplot(aes(x = cluvarp, 
                 y = reorder(study_country, lat_med), 
                 color = reorder(cluvar, -cluvarp), 
                 shape = gsk_loc)) + 
      geom_vline(aes(xintercept = 0.5), color = "grey") + 
      geom_point(size = 2) + 
      scale_color_viridis_d() +
      scale_shape_discrete("GSK data", 
                           guide = guide_legend(title.position = "top", ncol = 1)) + 
      scale_x_continuous("Proportion of centers") + 
      scale_y_discrete("") + 
      guides(color = FALSE) + 
      facet_grid(cluvar_country~., scales = "free", space = "free") + 
      theme(strip.text.y = element_text(angle = -90),
            strip.background = element_blank(),
            legend.position = "top",
            axis.text.y = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks.y = element_blank()) 
    return(pl)
  } 
  
  # Get list of plots
  pl_l <- lapply(str_subset(unique(clusters_cou$cluvar_name), cluster_type_name), fun_pl)
  
  # Put in a single plot 
  pl_all <- wrap_plots(pl_l) + plot_layout(nrow = 1)
  
  # Save plot 
  tot_plots <- length(str_subset(unique(clusters_cou$cluvar_name), cluster_type_name))
  if(cluster_type_name == "dayl\\.") cluster_type_name = "dayl"
  ggsave(glue("{dirs$outputs}/_figures/{pathogen}_cluster_countries_{cluster_type_name}_{file_name}.pdf"), 
         pl_all, width = 6*tot_plots, height = 14, limitsize = FALSE)
  
  return(pl_all)
}

# fun_cluster_country_pl <- function(clusters_all = clusters_all,
#                                    cluster_type_name = "kgc",
#                                    pathogen = "varicella") {
#   
#   fun_pl <- function(cluster_var_name = "kgc_1") {
#     
#     if(str_detect(cluster_var_name, "reg") == FALSE) ncol_leg <- 3
#     if(str_detect(cluster_var_name, "reg") == TRUE) ncol_leg <- 2
#     
#     # Prepare data for plot
#     dat_pl <- clusters_all  %>%
#       filter(cluvar_name == cluster_var_name) %>%
#       left_join(dat_spatial_all %>%
#                   group_by(study, country, gsk_loc) %>%
#                   mutate(lon_med = median(lon),
#                          lat_med = median(lat))) %>%
#       left_join(filter(clusters_cou, cluvar_name == cluster_var_name) %>%
#                   rename(cluvar_country = cluvar) %>%
#                   select(study, country, cluvar_country)) %>%
#       mutate(study_country = glue("{study}_{country}")) 
#     
#     # Plot it 
#     pl <- dat_pl %>%
#       ggplot(aes(x = cluvarp, 
#                  y = reorder(study_country, lat_med), 
#                  fill = reorder(cluvar, -cluvarp))) + 
#       geom_bar(stat = "identity", position="fill") + 
#       scale_fill_viridis_d(cluster_var_name, 
#                            guide = guide_legend(title.position = "top", ncol = ncol_leg)) +
#       scale_x_continuous("Proportion of centers") + 
#       scale_y_discrete("") + 
#       facet_grid(cluvar_country~., scales = "free", space = "free") + 
#       theme(strip.text.y = element_text(angle = -90),
#             strip.background = element_blank(),
#             legend.position = "top") + dat_pl %>%
#       ggplot(aes(x = cluvarp, 
#                  y = reorder(study_country, lat_med), 
#                  color = reorder(cluvar, -cluvarp), 
#                  shape = gsk_loc)) + 
#       geom_vline(aes(xintercept = 0.5), color = "grey") + 
#       geom_point(size = 2) + 
#       scale_color_viridis_d() +
#       scale_shape_discrete("GSK data", 
#                            guide = guide_legend(title.position = "top", ncol = 1)) + 
#       scale_x_continuous("Proportion of centers") + 
#       scale_y_discrete("") + 
#       guides(color = FALSE) + 
#       facet_grid(cluvar_country~., scales = "free", space = "free") + 
#       theme(strip.text.y = element_text(angle = -90),
#             strip.background = element_blank(),
#             legend.position = "top",
#             axis.text.y = element_blank(),
#             axis.title.y = element_blank(),
#             axis.ticks.y = element_blank()) 
#     
#     return(pl)
#   } 
#   
#   # Get list of plots
#   pl_l <- lapply(str_subset(unique(clusters_cou$cluvar_name), cluster_type_name), fun_pl)
#   
#   # Put in a single plot 
#   pl_all <- wrap_plots(pl_l) + plot_layout(nrow = 1)
#   
#   # Save plot 
#   tot_plots <- length(str_subset(unique(clusters_cou$cluvar_name), cluster_type_name))
#   if(cluster_type_name == "dayl\\.") cluster_type_name = "dayl"
#   ggsave(glue("{dirs$outputs}/_figures/{pathogen}_cluster_countries_{cluster_type_name}.pdf"), 
#          pl_all, width = 6*tot_plots, height = 14, limitsize = FALSE)
#   
#   return(pl_all)
# }

####################################################################################################
# End 
####################################################################################################