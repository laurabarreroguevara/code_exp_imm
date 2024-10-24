####################################################################################################
# Purpose of script: Function to plot the weekly clusters heatplot
####################################################################################################

library(colorspace)

fun_weekclu_heapl <- function(clusterg_name = c("reg", "dayl", "kgc", "tavg", "prec"),
                              dat = dat_l$d1_ii,
                              pathogen = "varicella",
                              data_name = "dl") {
  
  weekclug_pl_dl <- lapply(clusterg_name, function(x) {
    dat %>%
      select(starts_with(x), vac_week_v1, doses) %>%
      pivot_longer(starts_with(x)) %>%
      group_by(name, value, vac_week_v1) %>%
      summarise(tot = n()) %>%
      ungroup() %>%
      ggplot(aes(x = vac_week_v1, y = fct_rev(value), fill = value)) + 
      geom_tile() + 
      scale_x_continuous("Vaccination week", expand = c(0,0)) + 
      scale_y_discrete("") + 
      scale_fill_discrete_diverging(palette = "Tropic") + 
      facet_grid(name~., scales = "free", space = "free") + 
      theme(legend.position = "none",
            strip.background = element_blank())
    })
  weekclug_pl_dl <- wrap_plots(weekclug_pl_dl) + plot_layout(nrow = 1)
  ggsave(weekclug_pl_dl, file = glue("{dirs$outputs}/_figures/{pathogen}_des{data_name}_weekclug.pdf"),
         height = 14, width = 20)
  return(weekclug_pl_dl)  
  }


####################################################################################################
# End 
####################################################################################################