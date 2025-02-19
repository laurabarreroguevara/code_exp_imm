####################################################################################################
# Function to create data frame for mono/multi-pathogen and mono/multivariate models.
####################################################################################################

fun_dgsk_modpat <- function(dat = stu_ppat[[pat]], method = "elisa", seroname = "anti_mumps",
                         dose = "1", multi_pat = FALSE, clim = NA, clim_thr = 0.8) {
  # Arguments:
  # - dat: (data frame) Data with vaccination and immunogenicity measurements per participant.
  # - method: (string) Selected immunogenicity method (e.g., "elisa").
  # - seroname: (string) Selected immunogenicity marker (e.g., "anti_mumps").
  # - dose: (string) Selected vaccination dose (e.g., "1" or "l" for the last dose).
  # - multi_pat: (logical) Is this for a multivariate model?
  # - clim: (string) Climatic cluster to filter (optional).
  # - clim_thr: (numeric) Threshold to filter climate and photoperiod (0-1 scale).
  
  # Output:
  # - dat: (data frame) Tidy data ready for use in the model.
  
  # Filter based on all types of climate and photoperiod clusters
  if(!is.na(clim)) dat <- filter(dat, !!sym(paste0(clim,"_p")) >= clim_thr)
  
  # Filter by immunogenicity method, serology, and climate threshold
  dat <- dat %>%
    filter(if_all(starts_with("reg_"), ~ .x >= clim_thr)) %>%
    filter(imm_met_s1 %in% method) %>%
    filter(imm_sero_s1 %in% seroname)
  
  # Filter doses
  if(dose %in% c("1","2","3","4","5","6")) {
    dat <- filter(dat, doses == dose) 
  } else if(dose == "l") {
    dat <- group_by(dat, study, pid) %>%
      filter(doses <= 3) %>%
      mutate(ldose = max(doses)) %>%
      filter(doses == ldose) %>%
      ungroup() %>%
      select(-ldose)
  }
  
  # Tidy up the data
  dat <- dat %>%
    mutate(
      # Tidy individual data
      pid = glue("{study}_{pid}"),    
      center = sprintf("%03d", as.numeric(center)),
      center = glue("{study}_{center}"),
      rct_group = glue("{study}_{rct_group}"),
      # Tidy vaccine concentrations 
      vac_conc_n = vac_conc,
      vac_conc = c(scale(vac_conc)),
      pi_vac_week_v1 = vac_week_v1 * (2*pi/52),
      across(starts_with("vac_name_v"), ~ replace_na(.x, "No_dose")),
      across(starts_with("vac_name_conco"), ~ replace_na(.x, "No_conco"))) %>%
    select(-c(pat, age_unit, time_s0_v1, imm_val_cutoff_s0, imm_val_s0_all)) %>%
    mutate(
      across(c(all_of(c(
        "study", "country", "center", "rct_group", "eligibility", "pid", "sex", "imm_met_s1", 
        "imm_sero_s1")), starts_with("vac_name_v"), starts_with("vac_name_conco"), starts_with("reg_"),
        starts_with("kgc_")), as.factor)) %>%
    # Relocate variables
    relocate(study, country, center, rct_group,                                          # RCT data
             eligibility, pid, sex, age, age_ref, age_ref_date, birth_date, birth_week,  # Ind data
             doses, vac_conc, vac_conc_n, vac_conc_unit,                                 # Vac data
             ends_with("v1"),
             ends_with("v2"),
             ends_with("v3"), 
             ends_with("v4"), 
             ends_with("v5"), 
             imm_met_s1, imm_sero_s1, imm_cutoff_s1, 
             imm_date_s1, time_vl_s1, imm_val_s1, censored,
             lon_med, lat_med, starts_with("kgc"), starts_with("reg"))                   # Im1 data  
  
  # Handle multi-unipathogen case (multiple serology markers for a single pathogen)
  if(length(seroname) > 1 & length(method) == 1 & multi_pat == FALSE) {
    dat <- dat %>%
      pivot_wider(names_from = imm_sero_s1, values_from = c(censored, imm_val_s1, imm_cutoff_s1)) %>%
      filter(if_all(starts_with("imm_val_s1"), ~ !is.na(.x))) %>%
      relocate(study, country, center, rct_group, eligibility, pid, sex, age, age_ref, age_ref_date, 
               birth_date, birth_week, doses, vac_conc, vac_conc_n, vac_conc_unit, ends_with("v1"),
               ends_with("v2"), ends_with("v3"),  ends_with("v4"), ends_with("v5"), imm_met_s1, 
               starts_with("imm_cutoff_s1"), imm_date_s1, time_vl_s1, starts_with("imm_val_s1"), 
               starts_with("censored"), lon_med, lat_med, starts_with("kgc"), starts_with("reg"))
  }
  
  # Handle multi-multipathogen case (multiple serology markers for multiple pathogens)
  if(length(seroname) > 1 & multi_pat == TRUE) {
    dat <- dat %>%
      select(-c(starts_with("vac_name_conco"), vac_conc, vac_conc_n)) %>%
      pivot_wider(names_from = imm_sero_s1, values_from = c(censored, imm_val_s1, imm_met_s1, imm_cutoff_s1)) %>%
      filter(if_all(starts_with("imm_val_s1"), ~ !is.na(.x))) %>%
      relocate(study, country, center, rct_group, eligibility, pid, sex, age, age_ref, age_ref_date, 
               birth_date, birth_week, doses, vac_conc_unit, ends_with("v1"),
               ends_with("v2"), ends_with("v3"),  ends_with("v4"), ends_with("v5"), 
               starts_with("imm_cutoff_s1"), imm_date_s1, time_vl_s1, starts_with("imm_val_s1"), 
               starts_with("censored"), lon_med, lat_med, starts_with("kgc"), starts_with("reg"))
  }
  
  return(dat)
}

####################################################################################################
# End
####################################################################################################























