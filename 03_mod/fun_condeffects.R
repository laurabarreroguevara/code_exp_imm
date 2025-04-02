####################################################################################################
# Function to read conditional effects 
####################################################################################################
fun_condeffects <- function(mod = "bmv1_s08_hav_cs.rds", clu_name = NA, multi = FALSE, pat = "hav", 
                            vac = "1") {
  
  # Read in data
  dat_raw <- readRDS(paste0(dirs$modfit, "/data_brms_", pat, "_v", vac, "_s08.rds")) %>%
    mutate(pi_vac_week_v1 = round(pi_vac_week_v1, digits = 11)) 
  
  # Read in conditional effects
  ce_effects <- readRDS(glue("{dirs$ceffects}/ce_{mod}"))
  
  # Prepare data 
  dat <- distinct(dat_raw, vac_week_v1, pi_vac_week_v1)
  pref_clu_name <- if(!is.na(clu_name)) paste0(":", clu_name) else ""
  # Create group vector 
  var_vector <- c()
  
  # Prepare clusters if necessary
  if(!is.na(clu_name)) {
    # Create cluster variable 
    ce_effects <- ce_effects %>%
      rename(clu = all_of(clu_name)) %>%
      mutate(clu = as.character(clu))
    # Create df of total values per week
    dat_tot <- dat_raw %>%
      rename(clu = all_of(clu_name)) %>%
      mutate(clu = as.character(clu)) %>%
      group_by(pi_vac_week_v1, vac_week_v1, clu) %>%
      summarise(tot = n(), .groups = "drop") %>%
      group_by(clu) %>%
      mutate(checkgaps = fun_checkgaps(vac_week_v1)) %>%
      ungroup()
    
    # Attach cluster to group vector 
    var_vector <- c(var_vector, "clu")
  } else {
    # Create df of total values per week
    dat_tot <- group_by(dat_raw, pi_vac_week_v1, vac_week_v1) %>%
      summarise(tot = n(), .groups = "drop") %>%
      mutate(checkgaps = fun_checkgaps(vac_week_v1))
  }
  
  if(multi) var_vector <- c(var_vector, "imm_name")
  
  # Obtain estimates 
  ce_effects <- ce_effects %>%
    mutate(pi_vac_week_v1 = round(pi_vac_week_v1, digits = 11)) %>%
    select(pi_vac_week_v1, estimate__, lower__, upper__, all_of(var_vector)) %>%
    left_join(dat, by = join_by(pi_vac_week_v1)) %>%
    left_join(dat_tot) %>%
    rename(mean_estimate = estimate__, ci_lower = lower__, ci_upper = upper__) %>%
    # Fix checkgaps now there are NAs
    mutate(checkgaps_all = !all(checkgaps == FALSE, na.rm = TRUE))
  
  return(ce_effects)
}
####################################################################################################
# END
####################################################################################################