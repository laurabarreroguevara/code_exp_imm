####################################################################################################
# Function to create data frame of concomitant vaccines
####################################################################################################

fun_dgsk_vacconco <- function(df_expo_all, df_expo, vac_names = c("vzv")) {
  # Arguments:
  # - df_expo_all: (list of data frames) List of vaccination data frames from all included studies, 
  #   containing all administered vaccinations, regardless of pathogen.
  # - df_expo: (data frame) Tidy vaccination data frame containing only doses administered 
  #   against the specific pathogen of interest.
  # - vac_names: (vector of strings) Names of vaccines targeting the specific pathogen.
  # Output:
  # - df_expo_conco: (data frame) Tidy data frame that captures concomitant vaccinations (i.e., 
  #   other vaccines administered) around the time of vaccinations against the specific pathogen.
  
  # Determine the number of doses administered for the specific pathogen of interest. 
  n_doses <- length(str_subset(names(df_expo), "vac_name"))
  
  # Identify concomitant vaccinations  
  # Concomitant vaccinations are those that are NOT targeting the specific pathogen and were 
  # administered to participants (v_adm == "Y").
  df_expo_conco <- bind_rows(df_expo_all) %>%
    separate_longer_delim(vac_name, delim = "_") %>%
    # Consider only administered vaccinations other than against the specific pathogen
    filter(!(vac_name %in% vac_names) & !is.na(vac_name) & v_adm == "Y") %>%
    rename("vaccine_conco" = vac_name) %>%
    # Join with the specific vaccination data (df_expo) to link the concomitant vaccines
    left_join(df_expo, by = join_by(study, pid))
  
  # Initialize list to store concomitant vaccinations per dose
  df_expo_conco_l <- vector("list", n_doses)
  
  # Iterate over doses and create the necessary data frames
  for (dose in 1:n_doses) {
    df_expo_conco_l[[dose]] <- df_expo_conco %>%
      # Identify vaccinations given within the ±15-day window relative to each specific dose
      mutate(concomitant = case_when(
        vac_date >= (get(paste0("vac_date_v", dose)) - 15) & 
          vac_date <= (get(paste0("vac_date_v", dose)) + 15) ~ TRUE, .default = FALSE)) %>%
      filter(concomitant == TRUE) %>%
      group_by(study, pid) %>%
      arrange(vaccine_conco) %>%
      summarise(!!paste0("vac_name_conco_v", dose) := str_c(vaccine_conco, collapse = "_"),
                .groups = "drop")
  }
  
  # Combine the data frames for all doses
  df_expo_conco <- reduce(df_expo_conco_l[seq_len(n_doses)], left_join, by = c("study", "pid"))
  
  return(df_expo_conco)
}

####################################################################################################
# End
####################################################################################################