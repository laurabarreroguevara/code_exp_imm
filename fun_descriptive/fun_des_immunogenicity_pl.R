####################################################################################################
# Purpose of script: Function to plot the immunogenicity data
####################################################################################################

fun_des_immunogenicity_pl <- function(dat = dat,
                                      cutoff = 4,
                                      pathogen = "varicella",
                                      data_name = "dl") {
  
  # Identify number of doses
  doses <- max(dat$doses)
  
  # Plot the immunogenicity values -----------------------------------------------------------------
  pl1 <- dat %>%
    select(study, pid, starts_with("imm_val")) %>%
    pivot_longer(cols = starts_with("imm_val")) %>%
    group_by(study) %>%
    mutate(study_count = n()) %>%
    ggplot(aes(x = value, fill = reorder(study, study_count))) + 
    geom_vline(aes(xintercept = cutoff), color = "grey") + 
    geom_histogram() + 
    facet_wrap(~name, scales = "free_y") + 
    scale_fill_viridis_d(direction = -1) + 
    scale_y_continuous("Count") + 
    scale_x_continuous("Immunogenicity", trans = "log") + 
    theme(legend.position = "none",
          strip.background = element_blank())
  
  # Plot the censoring of imm1 values --------------------------------------------------------------
  pl2 <- dat %>%
    group_by(study, censored) %>%
    summarise(Count = n()) %>%
    group_by(censored) %>%
    mutate(censored_count = sum(Count)) %>%
    ggplot(aes(y = reorder(censored, censored_count), 
               x = Count, fill = reorder(study, Count))) + 
    geom_bar(stat = "identity") + 
    scale_y_discrete("Censored") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")
  
  # Plot the immunogneicity throught time ----------------------------------------------------------
  # This depends on the number of doses
  
  if(doses == 1) {
  pl3 <- dat %>%
    ggplot(aes(x = time_vl_s1, y = imm_val_s1, color = as.factor(doses))) + 
    geom_hline(aes(yintercept = cutoff), color = "grey") + 
    geom_point(alpha = 0.5, size = 3) + 
    scale_x_continuous("Days (v1_v2 + vl_i)", trans = "log10") + 
    scale_y_continuous("Immunogenicity (log)") + 
    scale_color_viridis_d("Doses", direction = -1, begin = 0.4, end = 0.8, option = "rocket")
  }
  
  if(doses == 2) {
    pl3 <- dat %>%
      ggplot(aes(x = time_v1_v2 + time_vl_s1, y = imm_val_s1, color = as.factor(doses))) + 
      geom_hline(aes(yintercept = cutoff), color = "grey") + 
      geom_point(alpha = 0.5, size = 3) + 
      scale_x_continuous("Days (v1_v2 + vl_i)", trans = "log10") + 
      scale_y_continuous("Immunogenicity (log)") + 
      scale_color_viridis_d("Doses", direction = -1, begin = 0.4, end = 0.8, option = "rocket")
  }
  
  if(doses == 3) {
    pl3 <- dat %>%
      ggplot(aes(x = time_v1_v2 + time_v2_v3 + time_vl_s1, y = imm_val_s1, 
                 color = as.factor(doses))) + 
      geom_hline(aes(yintercept = cutoff), color = "grey") + 
      geom_point(alpha = 0.5, size = 3) + 
      scale_x_continuous(trans = "log10") + 
      scale_y_continuous("Immunogenicity (log)", trans = "log") + 
      scale_color_viridis_d("Doses", direction = -1, begin = 0.4, end = 0.8, option = "rocket")
  }
  
  if(doses == 4) {
    pl3 <- dat %>%
      ggplot(aes(x = time_v1_v2 + time_v2_v3 + time_v3_v4 + time_vl_s1, y = imm_val_s1, 
                 color = as.factor(doses))) + 
      geom_hline(aes(yintercept = cutoff), color = "grey") + 
      geom_point(alpha = 0.5, size = 3) + 
      scale_x_continuous(trans = "log10") + 
      scale_y_continuous("Immunogenicity (log)", trans = "log") + 
      scale_color_viridis_d("Doses", direction = -1, begin = 0.4, end = 0.8, option = "rocket")
  }
  
  if(doses == 5) {
    pl3 <- dat %>%
      ggplot(aes(x = time_v1_v2 + time_v2_v3 + time_v3_v4 + time_v4_v5 + time_vl_s1, y = imm_val_s1, 
                 color = as.factor(doses))) + 
      geom_hline(aes(yintercept = cutoff), color = "grey") + 
      geom_point(alpha = 0.5, size = 3) + 
      scale_x_continuous(trans = "log10") + 
      scale_y_continuous("Immunogenicity (log)", trans = "log") + 
      scale_color_viridis_d("Doses", direction = -1, begin = 0.4, end = 0.8, option = "rocket")
  }
  
  # Bring plots together ---------------------------------------------------------------------------
  
  pl <- ((pl1 | pl2) + plot_layout(widths = c(1 ,0.5))) / pl3
  
  # Save it!
  ggsave(plot = pl, glue("{dirs$figures}/{pathogen}_des{data_name}_immunogenicity.pdf"),
         width = 15, height = 10)
  
  return(pl)
}

####################################################################################################
# End 
####################################################################################################