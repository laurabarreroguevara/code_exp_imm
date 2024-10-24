####################################################################################################
# Function to process and tidy immunogenicity data relative to vaccination doses, including time 
# differences, baseline values, and censoring status
####################################################################################################

fun_dgsk_vacimm <- function(dat) {
  # Arguments:
  # - dat: (data frame) A data frame containing vaccination and immunogenicity information per 
  #   individual, including dates of vaccination, and immunogenicity measurements.
  # Output:
  # - dat: (data frame) The input data frame with tidied immunogenicity and vaccination data, 
  #   including calculated time differences between doses and immunogenicity, censoring status, 
  #   and age at first vaccination.
  
  # Determine the number of vaccination doses
  n_doses <- length(str_subset(names(dat), "vac_name_v"))
  
  # Step 1: Identify baseline immunogenicity (imm_val_s0) ------------------------------------------
  imm_s0 <- dat %>%
    # Take only the date before or same day as 1st vaccination date:
    filter(imm_date <= vac_date_v1) %>%
    # Calculate difference between vaccination date and immunogenicity date: 
    mutate(time_s0_v1 = vac_date_v1 - imm_date) %>%
    # Select the smallest difference per individual: 
    group_by(study, pid, imm_met, imm_sero) %>%
    filter(time_s0_v1 == min(as.numeric(time_s0_v1))) %>%
    ungroup() %>%
    select(study, pid, imm_val, imm_met, imm_date, imm_sero, imm_cutoff) %>%
    rename_with(~ paste0(.x, "_s0"), c("imm_val", "imm_met", "imm_date", "imm_sero", "imm_cutoff"))
  
  # Step 2: Identify immunogenicity values for each dose -------------------------------------------
  # Initialize list to store vaccinations per dose
  df_imm_l <- vector("list", n_doses)
  
  # Process each dose
  for(dose in 1:n_doses) {
    if(n_doses == dose) {
      # If this is the last dose
      df_imm_l[[dose]] <- dat %>%
        # Take only immunogenicity values after this dose
        filter(imm_date > get(paste0("vac_date_v", dose)))
    } else if(n_doses > dose) {
      # If there are doses after this dose
      df_imm_l[[dose]]  <- dat %>%
        # Take only immunogenicity values after this dose and before the next dose (if available)
        filter((is.na(get(paste0("vac_date_v", dose + 1))) & 
                  imm_date > get(paste0("vac_date_v", dose))) |
                (!(is.na(get(paste0("vac_date_v", dose + 1)))) & 
                   imm_date > get(paste0("vac_date_v", dose)) & 
                   imm_date <= get(paste0("vac_date_v", dose + 1))))
    }
    
    df_imm_l[[dose]]  <- df_imm_l[[dose]]  %>%
      # Calculate difference between vaccination date and immunogenicity values:
      mutate(!!paste0("time_v", dose, "_s1") := imm_date - get(paste0("vac_date_v", dose))) %>%
      # Select the smallest difference per individual:
      group_by(study, pid, imm_met, imm_sero) %>%
      filter(get(paste0("time_v", dose, "_s1")) == min(get(paste0("time_v", dose, "_s1")))) %>%
      ungroup() %>%
      select(study, pid, imm_val, imm_met, imm_date, imm_sero, imm_cutoff) %>%
      rename_with(~ paste0(.x, "_s1"), c("imm_val", "imm_met", "imm_date", "imm_sero", "imm_cutoff")) %>%
      mutate(doses = dose)
  }
  
  # Step 3: Combine baseline (s0) and post-vaccination (s1) immunogenicity values ------------------
  vacimm <- dat %>%
    select(-c(activity, imm_val, imm_met, imm_date, imm_sero, imm_cutoff)) %>%
    distinct() %>%
    left_join(imm_s0, by = join_by(study, pid)) %>%
    left_join(bind_rows(df_imm_l), by = join_by(study, pid), relationship = "many-to-many") %>%
    # Calculate days between baseline immunogenicity and first vaccination 
    mutate(time_s0_v1 = as.numeric(vac_date_v1 - imm_date_s0),
           time_s0_v1 = replace_na(time_s0_v1, 0)) %>%
    # Calculate the age at first vaccination
    mutate(age = as.numeric((age_ref + (vac_date_v1 - age_ref_date))/7)) %>%
    # Add censoring variable (immunogenicity value under the cutoff)
    mutate(censored = case_when(imm_val_s1 < imm_cutoff_s1 ~ 1,
                                imm_val_s1 >= imm_cutoff_s1 ~ 0))
  
  # Step 4: Calculate time intervals between doses and immunogenicity ------------------------------
  if(n_doses >= 1) {
    vacimm <- vacimm %>%
      # Calculate days between s1 and each vaccine dose
      mutate(time_vl_s1 = case_when(doses == 1 ~ as.numeric(imm_date_s1 - vac_date_v1)),
             time_vl_s1 = replace_na(time_vl_s1, 0))
  }
  if(n_doses >= 2) {
    vacimm <- vacimm %>%
      # Calculate days between v2 and v1 
      mutate(time_v1_v2 = as.numeric(vac_date_v2 - vac_date_v1),
             time_v1_v2 = replace_na(time_v1_v2, 0),
             time_v1_v2 = case_when(doses %in% c(1) ~ 0, .default = time_v1_v2),
             # Calculate days between s1 and each vaccine dose
             time_vl_s1 = case_when(doses == 1 ~ as.numeric(imm_date_s1 - vac_date_v1),
                                    doses == 2 ~ as.numeric(imm_date_s1 - vac_date_v2)),
             time_vl_s1 = replace_na(time_vl_s1, 0))
  }
  if(n_doses >= 3) {
    vacimm <- vacimm %>%
      # Calculate days between v3 and v2 
      mutate(time_v2_v3 = as.numeric(vac_date_v3 - vac_date_v2),
             time_v2_v3 = replace_na(time_v2_v3, 0),
             time_v2_v3 = case_when(doses %in% c(1:2) ~ 0, .default = time_v2_v3),
             # Calculate days between s1 and each vaccine dose
             time_vl_s1 = case_when(doses == 1 ~ as.numeric(imm_date_s1 - vac_date_v1),
                                    doses == 2 ~ as.numeric(imm_date_s1 - vac_date_v2), 
                                    doses == 3 ~ as.numeric(imm_date_s1 - vac_date_v3)),
             time_vl_s1 = replace_na(time_vl_s1, 0))
  }
  if(n_doses >= 4) {
    vacimm <- vacimm %>%
      # Calculate days between v4 and v3 
      mutate(time_v3_v4 = as.numeric(vac_date_v4 - vac_date_v3),
             time_v3_v4 = replace_na(time_v3_v4, 0),
             time_v3_v4 = case_when(doses %in% c(1:3) ~ 0, .default = time_v3_v4),
             # Calculate days between s1 and each vaccine dose
             time_vl_s1 = case_when(doses == 1 ~ as.numeric(imm_date_s1 - vac_date_v1),
                                    doses == 2 ~ as.numeric(imm_date_s1 - vac_date_v2), 
                                    doses == 3 ~ as.numeric(imm_date_s1 - vac_date_v3),
                                    doses == 4 ~ as.numeric(imm_date_s1 - vac_date_v4)),
             time_vl_s1 = replace_na(time_vl_s1, 0))
  }
  if(n_doses >= 5) {
    vacimm <- vacimm %>%
      # Calculate days between v5 and v4 
      mutate(time_v4_v5 = as.numeric(vac_date_v5 - vac_date_v4),
             time_v4_v5 = replace_na(time_v4_v5, 0),
             time_v4_v5 = case_when(doses %in% c(1:4) ~ 0, .default = time_v4_v5),
             # Calculate days between s1 and each vaccine dose
             time_vl_s1 = case_when(doses == 1 ~ as.numeric(imm_date_s1 - vac_date_v1),
                                    doses == 2 ~ as.numeric(imm_date_s1 - vac_date_v2), 
                                    doses == 3 ~ as.numeric(imm_date_s1 - vac_date_v3),
                                    doses == 4 ~ as.numeric(imm_date_s1 - vac_date_v4),
                                    doses == 5 ~ as.numeric(imm_date_s1 - vac_date_v5)),
             time_vl_s1 = replace_na(time_vl_s1, 0))
  }
  
  return(vacimm)
}

####################################################################################################
# End
####################################################################################################
