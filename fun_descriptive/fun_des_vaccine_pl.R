####################################################################################################
# Purpose of script: Function to plot the vaccine data
####################################################################################################

library(colorspace)

fun_des_vaccine_pl <- function(dat = dat,
                               pathogen = "varicella",
                               data_name = "dl") {
  
  # Plot the number of doses -----------------------------------------------------------------------
  pl1 <- dat %>%
    mutate(doses = as.factor(doses)) %>%
    group_by(study, doses) %>%
    summarise(Count = n()) %>%
    group_by(doses) %>%
    mutate(doses_count = sum(Count)) %>%
    ggplot(aes(y = reorder(doses, doses_count), x = Count, fill = reorder(study, Count))) + 
    geom_bar(stat = "identity") + 
    scale_y_discrete("Doses") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")
  
  # Plot the number of vaccine concentration -------------------------------------------------------
  pl2 <- dat %>%
    group_by(study) %>%
    mutate(study_count = n()) %>%
    ggplot(aes(x = vac_conc_n, fill = reorder(study, study_count))) + 
    geom_histogram() + 
    scale_y_continuous("Count") +
    scale_x_continuous("Vaccine concentration (log10)", trans = "log10") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")

  # Plot the vaccination week ----------------------------------------------------------------------
  pl3 <- dat %>%
    group_by(study) %>%
    mutate(study_count = n()) %>%
    ggplot(aes(x = vac_week_v1, fill = reorder(study, study_count))) + 
    geom_histogram() + 
    scale_y_continuous("Count") +
    scale_x_continuous("Week of vaccination") + 
    scale_fill_viridis_d(direction = -1) + 
    theme(legend.position = "none")
  
  # Plot vaccination names and concomitants --------------------------------------------------------
  pl4 <- dat %>%
    select(!c(starts_with("vac_date"), starts_with("vac_conc"), starts_with("vac_week"))) %>%
    select(study, pid, starts_with("vac_")) %>%
    group_by(study) %>%
    mutate(study_count = n()) %>%
    pivot_longer(cols = starts_with("vac_")) %>%
    group_by(study, study_count, name, value) %>%
    summarise(Count = n()) %>%
    group_by(name, value) %>%
    mutate(name_count = n()) %>%
    ggplot(aes(y = reorder(value, name_count), x = Count, fill = reorder(study, study_count))) + 
    geom_bar(stat = "identity") + 
    facet_wrap(~name, scales = "free") + 
    scale_fill_viridis_d(direction = -1) + 
    scale_y_discrete("") + 
    theme(legend.position = "none",
          strip.background = element_blank())
  
  # Bring plots ------------------------------------------------------------------------------------
  pl <- ((pl1 | pl2 | pl3) / (pl4)) + plot_layout(heights = c(0.5,1))
  
  # Save it!
  ggsave(plot = pl, glue("{dirs$figures}/{pathogen}_des{data_name}_vaccine.pdf"),
         width = 15, height = 10)
  
  return(pl)
}

####################################################################################################
# End 
####################################################################################################