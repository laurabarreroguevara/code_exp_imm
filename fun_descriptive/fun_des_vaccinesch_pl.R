####################################################################################################
# Purpose of script: Function to plot the vaccine schedule data
####################################################################################################

fun_des_vaccinesch_pl <- function(dat = dat,
                                  pathogen = "varicella",
                                  data_name = "dl") {
  
  # Identify number of doses
  doses <- max(dat$doses)
  
  # Data time between events depending on number of doses ------------------------------------------
  
  if(doses == 1) {
    dat_pl <- dat %>%
      select(study, pid, time_s0_v1, time_vl_s1)
    npl <- 1
  }
  
  if(doses == 2) {
    dat_pl <- dat %>%
      select(study, pid, time_s0_v1, time_v1_v2, time_vl_s1)
    npl <- 1
  }
  
  if(doses == 3) {
    dat_pl <- dat %>%
      select(study, pid, time_s0_v1, time_v1_v2, time_v2_v3, time_vl_s1)
    npl <- 2
  }
  
  if(doses == 4) {
    dat_pl <- dat %>%
      select(study, pid, time_s0_v1, time_v1_v2, time_v2_v3, time_v3_v4, time_vl_s1)
    npl <- 2
  }
  
  if(doses == 5) {
    dat_pl <- dat %>%
      select(study, pid, time_s0_v1, time_v1_v2, time_v2_v3, time_v3_v4, time_v4_v5, time_vl_s1)
    npl <- 2
  }
  
  # Plot it
  
  pl <- dat_pl %>%
    pivot_longer(cols = starts_with("time")) %>%
    group_by(study) %>%
    mutate(study_count = n()) %>%
    ggplot(aes(x = value/7, fill = reorder(study, study_count))) + 
    geom_histogram() + 
    facet_wrap(~name, scales = "free") + 
    scale_fill_viridis_d(direction = -1) + 
    scale_y_continuous("Count") + 
    scale_x_continuous("Weeks") + 
    theme(legend.position = "none",
          strip.background = element_blank())
  
  # Save it ----------------------------------------------------------------------------------------
  
  ggsave(plot = pl, glue("{dirs$figures}/{pathogen}_des{data_name}_vaccinesch.pdf"),
         width = 10, height = 3*npl) 
  
  return(pl)
}

####################################################################################################
# End 
####################################################################################################