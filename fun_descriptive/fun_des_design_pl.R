####################################################################################################
# Purpose of script: Function to plot the weekly clusters heatplot
####################################################################################################

library(colorspace)

fun_des_design_pl <- function(dat = dat,
                              pathogen = "varicella",
                              data_name = "dl") {
  
  # Identify number of doses
  doses <- max(dat$doses)
  
  # Plot the rcts vaccination and immunogenicity AGES ----------------------------------------------
  # Prepare the data in general 
  dat_p <- dat %>%
    # Give names to the studies with year 
    select(!c(age_ref, age_date)) %>%
    group_by(study) %>%
    mutate(study_date = min(imm_date_s1),
           study_year = year(study_date),
           study_tot = n(),
           study = glue("{study} ({study_year})")) %>%
    ungroup()
  
  # Prepare the data specifically for doses 1 to 3 (change time to age)
  if(doses == 1) {
    dat_p1 <- dat_p %>%
      mutate(time_vl_s1 = time_vl_s1/7,
             age_s1 = age + time_vl_s1) %>%
      mutate(time_vl_s1 = na_if(time_vl_s1, 0))
    
    col = c("#62CDAC","darkorange")
    lab = c("1st dose", "Immunogenicity")
  }
  
  if(doses == 2) {
    dat_p1 <- dat_p %>%
      mutate(time_v1_v2 = time_v1_v2/7,
             time_vl_s1 = time_vl_s1/7,
             age_s1 = age + time_v1_v2 + time_vl_s1) %>%
      mutate(time_v1_v2 = na_if(time_v1_v2, 0),
             time_vl_s1 = na_if(time_vl_s1, 0)) %>%
      mutate(age_d1 = age,
             age_d2 = age + time_v1_v2)
    
    col = c("#62CDAC","#292850","darkorange")
    lab = c("1st dose", "2nd dose", "Immunogenicity")
  }
  
  if(doses == 3) {
    dat_p1 <- dat_p %>%
      mutate(time_v1_v2 = time_v1_v2/7,
             time_v2_v3 = time_v2_v3/7,
             time_vl_s1 = time_vl_s1/7,
             age_s1 = age + time_v1_v2 + time_v2_v3 + time_vl_s1) %>%
      mutate(time_v1_v2 = na_if(time_v1_v2, 0),
             time_v2_v3 = na_if(time_v2_v3, 0),
             time_vl_s1 = na_if(time_vl_s1, 0)) %>%
      mutate(age_d1 = age,
             age_d2 = age + time_v1_v2,
             age_d3 = age + time_v1_v2 + time_v2_v3)
    
    col = c("#62CDAC","#39639C","#292850","darkorange")
    lab = c("1st dose", "2nd dose", "3rd dose", "Immunogenicity")
  }
  
  if(doses == 4) {
    dat_p1 <- dat_p %>%
      mutate(time_v1_v2 = time_v1_v2/7,
             time_v2_v3 = time_v2_v3/7,
             time_v3_v4 = time_v2_v3/7,
             time_vl_s1 = time_vl_s1/7,
             age_s1 = age + time_v1_v2 + time_v2_v3 + time_v3_v4 + time_vl_s1) %>%
      mutate(time_v1_v2 = na_if(time_v1_v2, 0),
             time_v2_v3 = na_if(time_v2_v3, 0),
             time_v3_v4 = na_if(time_v3_v4, 0),
             time_vl_s1 = na_if(time_vl_s1, 0)) %>%
      mutate(age_d1 = age,
             age_d2 = age + time_v1_v2,
             age_d3 = age + time_v1_v2 + time_v2_v3,
             age_d4 = age + time_v1_v2 + time_v2_v3 + time_v3_v4)
    
    col = c("#62CDAC","#39639C","#292850", "black","darkorange")
    lab = c("1st dose", "2nd dose", "3rd dose", "4th dose", "Immunogenicity")
  }
  
  if(doses == 5) {
    dat_p1 <- dat_p %>%
      mutate(time_v1_v2 = time_v1_v2/7,
             time_v2_v3 = time_v2_v3/7,
             time_v3_v4 = time_v2_v3/7,
             time_v4_v5 = time_v4_v5/7,
             time_vl_s1 = time_vl_s1/7,
             age_s1 = age + time_v1_v2 + time_v2_v3 + time_v3_v4 + time_v4_v5 + time_vl_s1) %>%
      mutate(time_v1_v2 = na_if(time_v1_v2, 0),
             time_v2_v3 = na_if(time_v2_v3, 0),
             time_v3_v4 = na_if(time_v3_v4, 0),
             time_v4_v5 = na_if(time_v4_v5, 0),
             time_vl_s1 = na_if(time_vl_s1, 0)) %>%
      mutate(age_d1 = age,
             age_d2 = age + time_v1_v2,
             age_d3 = age + time_v1_v2 + time_v2_v3,
             age_d4 = age + time_v1_v2 + time_v2_v3 + time_v3_v4,
             age_d5 = age + time_v1_v2 + time_v2_v3 + time_v3_v4 + time_v4_v5)
    
    col = c("#62CDAC","#39639C","#292850", "black", "black","darkorange")
    lab = c("1st dose", "2nd dose", "3rd dose", "4th dose", "5th dose","Immunogenicity")
  }
  
  # Finish preparing the data in general
  dat_p1 <- dat_p1 %>%
    mutate(age_d1 = age) %>%
    select(-age) %>%
    select(study, study_date, starts_with("age"), doses) %>%
    pivot_longer(cols = starts_with("age"))
  
  # Plot the ages 
  pl1 <- dat_p1 %>%
    ggplot(aes(y = reorder(study, study_date), x = value, color = name)) + 
    geom_point(alpha = 0.5, size = 2) + 
    scale_color_manual("", values = col, labels = lab) + 
    scale_x_continuous("Age (weeks)", trans = "log10") + 
    scale_y_discrete("") + 
    guides(color = guide_legend(nrow = 2, byrow = F)) + 
    theme(legend.position = "top",
          strip.background = element_blank(),
          #strip.text = element_blank(),
          panel.grid.major.y = element_line()) +
    ggtitle(glue("RCTs {pathogen} = {length(unique(dat$study))}"),
            subtitle = glue("Total individuals = {length(dat[[1]])}"))
  
  # Plot the time between doses (when doses > 1) ---------------------------------------------------
  
  # Plot for time betwen 1 and 2 doses 
  if(doses > 1) {
    # Get frequencies for x axis
    freq_t <- sort(table(round((dat %>%
                                  group_by(rct_group) %>%
                                  summarise(time_v1_v2 = median(time_v1_v2)))$time_v1_v2/7)), 
                   decreasing = TRUE)
    
    pl2 <- dat_p %>%
      select(study, rct_group, pid, time_v1_v2, study_date) %>%
      mutate(time_v1_v2 = na_if(time_v1_v2, 0)) %>%
      ggplot(aes(x = time_v1_v2/7, 
                 y = reorder(study, study_date), 
                 color = time_v1_v2/7)) + 
      geom_point(alpha = 0.2, size = 3) + 
      scale_x_continuous("Weeks between doses 1 and 2", 
                         breaks = as.numeric(c(0, names(freq_t)))) + 
      scale_y_discrete("") +
      scale_color_viridis_c("Weeks") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line())
  }
  
  # Plot for time betwen 2 and 3 doses 
  if(doses >= 3) {
    # Get frequencies for x axis
    freq_t <- sort(table(round((dat %>%
                                  group_by(rct_group) %>%
                                  summarise(time_v2_v3 = median(time_v2_v3)))$time_v2_v3/7)), 
                   decreasing = TRUE)
    
    pl3 <- dat_p %>%
      select(study, rct_group, pid, time_v2_v3, study_date) %>%
      mutate(time_v2_v3 = na_if(time_v2_v3, 0)) %>%
      ggplot(aes(x = time_v2_v3/7, 
                 y = reorder(study, study_date), 
                 color = time_v2_v3/7)) + 
      geom_point(alpha = 0.2, size = 3) + 
      scale_x_continuous("Weeks between doses 2 and 3", 
                         breaks = as.numeric(c(0, names(freq_t)))) + 
      scale_y_discrete("") +
      scale_color_viridis_c("Weeks") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
  }
  
  # Plot for time between 3 and 4 doses 
  if(doses >= 4) {
    # Get frequencies for x axis
    freq_t <- sort(table(round((dat %>%
                                  group_by(rct_group) %>%
                                  summarise(time_v3_v4 = median(time_v3_v4)))$time_v3_v4/7)), 
                   decreasing = TRUE)
    
    pl4 <- dat_p %>%
      select(study, rct_group, pid, time_v3_v4, study_date) %>%
      mutate(time_v3_v4 = na_if(time_v3_v4, 0)) %>%
      ggplot(aes(x = time_v3_v4/7, 
                 y = reorder(study, study_date), 
                 color = time_v3_v4/7)) + 
      geom_point(alpha = 0.2, size = 3) + 
      scale_x_continuous("Weeks between doses 3 and 4", 
                         breaks = as.numeric(c(0, names(freq_t)))) + 
      scale_y_discrete("") +
      scale_color_viridis_c("Weeks") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
  }
  
  # Plot for time between 4 and 5 doses 
  if(doses == 5) {
    # Get frequencies for x axis
    freq_t <- sort(table(round((dat %>%
                                  group_by(rct_group) %>%
                                  summarise(time_v4_v5 = median(time_v4_v5)))$time_v4_v5/7)), 
                   decreasing = TRUE)
    
    pl5 <- dat_p %>%
      select(study, rct_group, pid, time_v4_v5, study_date) %>%
      mutate(time_v4_v5 = na_if(time_v4_v5, 0)) %>%
      ggplot(aes(x = time_v4_v5/7, 
                 y = reorder(study, study_date), 
                 color = time_v4_v5/7)) + 
      geom_point(alpha = 0.2, size = 3) + 
      scale_x_continuous("Weeks between doses 3 and 4", 
                         breaks = as.numeric(c(0, names(freq_t)))) + 
      scale_y_discrete("") +
      scale_color_viridis_c("Weeks") +
      theme(legend.position = "top",
            panel.grid.major.y = element_line(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())
  }
  
  # Plot the time between dose and immunogenicity --------------------------------------------------
  # Get frequencies for x axis
  freq_t <- sort(table(round((dat %>%
                                group_by(rct_group) %>%
                                summarise(time_vl_s1 = median(time_vl_s1)))$time_vl_s1/7)), 
                 decreasing = TRUE)
  
  pli <- dat_p %>%
    select(study,rct_group, pid, time_vl_s1, study_date) %>%
    group_by(rct_group) %>%
    mutate(date_mean = mean(time_vl_s1)) %>%
    ggplot(aes(x = time_vl_s1/7, 
               y = reorder(study, study_date), 
               color = time_vl_s1/7)) + 
    geom_point(alpha = 0.2, size = 3) + 
    scale_x_continuous("Weeks from last dose to immunogenicity",
                       breaks = as.numeric(c(0, names(freq_t))),
                       trans = "log10") + 
    scale_y_discrete(" ") + 
    scale_color_viridis_c("Weeks") + 
    theme(legend.position = "top",
          panel.grid.major.y = element_line())
  
  # Take out axis y if doses > 1 
  if(doses > 1) {pli <- pli + theme(axis.text.y = element_blank(),
                                    axis.ticks.y = element_blank(),
                                    axis.title.y = element_blank())}                                           
  
  # Bring plots together depending on number of doses ----------------------------------------------
  if(doses == 1) { pl <- pl1 + pli }
  if(doses == 2) { pl <- pl1 + pl2 + pli }
  if(doses == 3) { pl <- pl1 + pl2 + pl3 + pli + plot_layout(nrow = 1) }
  if(doses == 4) { pl <- pl1 + pl2 + pl3 + pl4 + pli + plot_layout(nrow = 1) }
  if(doses == 5) { pl <- pl1 + pl2 + pl3 + pl4 + pl5 + pli + plot_layout(nrow = 1) }
  # Save it! 
  ggsave(plot = pl, glue("{dirs$figures}/{pathogen}_des{data_name}_design.pdf"),
         width = 15, height = 0.28*length(unique(dat$study)))
  
  return(pl)
}

####################################################################################################
# End 
####################################################################################################