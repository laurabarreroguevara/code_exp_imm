####################################################################################################
# Purpose of script: Function to plot the participants data
####################################################################################################

fun_des_participants_pl <- function(dat = dat,
                                    pathogen = "varicella",
                                    data_name = "dl") {
  
  # Plot the studies -------------------------------------------------------------------------------
  pl1 <- dat %>%
    group_by(study) %>%
    summarise(Count = n()) %>%
    ggplot(aes(y = reorder(study, Count), x = Count, fill = reorder(study, Count))) + 
    geom_bar(stat = "identity") + 
    scale_y_discrete("Study") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")

  # Plot the countries -----------------------------------------------------------------------------
  pl2 <- dat %>%
    group_by(study, country) %>%
    summarise(Count = n()) %>%
    group_by(country) %>%
    mutate(country_count = sum(Count)) %>%
    ggplot(aes(y = reorder(country, country_count), x = Count, fill = reorder(study, Count))) + 
    geom_bar(stat = "identity") + 
    scale_y_discrete("Country") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")
  
  # Plot the sex -----------------------------------------------------------------------------------
  pl3 <- dat %>%
    group_by(study, sex) %>%
    summarise(Count = n()) %>%
    group_by(sex) %>%
    mutate(sex_count = sum(Count)) %>%
    ggplot(aes(y = reorder(sex, sex_count), x = Count, fill = reorder(study, Count))) + 
    geom_bar(stat = "identity") + 
    scale_y_discrete("Sex") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")
  
  # Plot the age -----------------------------------------------------------------------------------
  pl4 <- dat %>%
    group_by(study) %>%
    mutate(study_count = n()) %>%
    ggplot(aes(x = age, fill = reorder(study, study_count))) + 
    geom_histogram() + 
    scale_y_continuous("Count") +
    scale_x_continuous("Age (weeks)") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")

  # Bring plots together ---------------------------------------------------------------------------

  pl <- ((pl1 + pl2) / (pl3 + pl4)) + plot_layout(heights = c(1,0.5))
  
  # Save it!
  ggsave(plot = pl, glue("{dirs$figures}/{pathogen}_des{data_name}_participants.pdf"),
         width = 15, height = 0.28*length(unique(dat$study)))
  
  return(pl)
}

####################################################################################################
# End 
####################################################################################################