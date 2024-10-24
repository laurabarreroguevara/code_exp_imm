################################################################################
# Reformat data frame of model simulations in long format
################################################################################

fun_reformatsimulations <- function(sims_wide, delta_t = delta_t) {
  # Args: 
  # sims_wide: simulations in wide format (data frame)
  # Returns: simulations in long format, with year and week no added
  
  sims_long <- sims_wide %>% 
    mutate(E = E1 + E2, 
           I = I1 + I2, 
           week = (time - delta_t / 2) %% 52 + delta_t / 2,
           year = (time - delta_t / 2) %/% 52 + 1) %>% 
    select(time, everything(), -c("E1", "E2", "I1", "I2")) %>% 
    pivot_longer(cols = -c(.id, time, year, week), 
                 names_to = "state_var", 
                 values_to = "val") %>% 
    mutate(.id = as.integer(.id))
  
  #browser()
  # Check that all years and weeks appear the same no of time 
  stopifnot(length(unique(table(sims_long$year))) == 1)
  
  return(sims_long)
}

################################################################################
# End
################################################################################